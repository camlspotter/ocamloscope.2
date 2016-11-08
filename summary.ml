open Spotlib.Spot
open Utils
open List
open Data
open Outcometree
open Data.DB

let flatten_result xs : (int * DB.item * 'trace1 * 'trace2) list =
  concat_map (fun (s,itts) -> map (fun (i,t1,t2) -> s,i,t1,t2) itts) xs

let item (_, i, _, _ : int * DB.item * 'trace1 * 'trace2) = i
  
(* Give the item self as an alias if it has none *)
let alias i =
  match i.alias with
  | None -> Path (i.kind, i.path)
  | Some p -> p
  
let path db = db.path

let path_complexity p =
  let rec f = function
    | Oide_ident _ -> 1
    | Oide_dot (p,_) -> f p + 1
    | Oide_apply (p1, p2) -> f p1 + f p2 + 3
  in
  f p
    
let alias_complexity = function
  | Primitive _ -> 0
  | Path (_,p) -> path_complexity p
    
let package = function
  | Oide_ident _ -> None (* predef *)
  | i ->
      let rec f = function
        | Oide_ident i -> Some i
        | Oide_dot (i, _) -> f i
        | Oide_apply (i1,_i2) -> f i1
      in
      f i

let package_of_alias = function
  | Primitive _ -> None
  | Path (_,p) -> package p
      
let top_package = function
  | None -> None
  | Some p ->
      match Packpath.parse p with
      | None -> None
      | Some [] -> assert false
      | Some (x::_) -> Some (top_package_name x)

(* Comparison chain *)
let (>>>) res1 cmp2 = match res1 with
  | 0 -> cmp2 ()
  | n -> n 

let compare_top_packages x y = match x, y with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some x, Some y when x = y -> 0
  | Some "stdlib", _ -> -1
  | _, Some "stdlib" -> 1
      (* XXX distributed by Ocaml thingies *)
  | Some x, Some y -> compare x y

(* group by the aliases, then sort by the lowest distance *)
let group_by_alias (xs : (int * DB.item * 'trace1 * 'trace2) list)
    : (alias * int * (int * DB.item * 'trace1 * 'trace2) list) list =
  let compare_groups (a1,d1,_xs1) (a2,d2,_xs2) =
    compare d1 d2 >>> fun () -> 
    compare_top_packages (top_package & package_of_alias a1) (top_package & package_of_alias a2) >>> fun () ->
    compare (alias_complexity a1) (alias_complexity a2)
  in
  sort compare_groups
  & map (fun xs -> alias (item & hd xs), fold_left1 min & map (fun (s,_,_,_) -> s) xs, xs)
  & sort_then_group_by (fun x y -> compare (alias & item x) (alias & item y)) xs
    
let sort_inside_group items : (int * DB.item * 'trace1 * 'trace2) list list =
  let nonaliased, aliased = partition (fun (_,i,_,_) -> i.alias = None) items in
  let compare_groups g1 g2 =
    let tp1 = top_package & package & path & item & hd g1 in
    let tp2 = top_package & package & path & item & hd g2 in
    compare_top_packages tp1 tp2 >>> fun () -> 
    compare (length g1) (length g2)
  in
  nonaliased
  :: sort compare_groups
     (sort_then_group_by (fun x y ->
         compare_top_packages
           (top_package & package & path & item x)
           (top_package & package & path & item y)) aliased)

let group xs : (alias * int * (int * DB.item * 'trace1 * 'trace2) list list) list =
  map (fun (alias, d, xs) -> (alias, d, sort_inside_group xs))
  & group_by_alias
  & flatten_result xs

let format_alias ppf = function
  | Path (k,p) ->
      Format.fprintf ppf "%s %a"
        (Sig.string_of_k k)
        Xoprint.print_ident p (*XXX fsig.  The value may be hidden by signature *)
  | Primitive n -> Format.fprintf ppf "primitive %s" n

let final_print ppf (g : (alias * int * (int * DB.item * 'trace1 * 'trace2) list list) list)  = flip iter g & fun (a, d, (xss : (int * DB.item * 'trace1 * 'trace2) list list) ) ->
  let nonaliased, aliased = partition (fun i -> i.alias = None) & map (fun (_,i,_,_) -> i) & flatten xss in
  let (!!%) fmt = Format.fprintf ppf fmt in
  let fsig i = Data.DB.fsignature_item i in

  let doc i = Option.bind i.DB.v Hump.get_doc in

  let format_item ppf x =
    let fsig = fsig x in
    let doc = doc x in
    begin match doc with
    | None -> 
        Sigext.Print.fsignature_item false ppf fsig;
        Format.fprintf ppf "@."
    | Some doc ->
        Format.fprintf ppf "@[<v>%a@,(** %s *)@]@."
          (Sigext.Print.fsignature_item false) fsig
          doc
    end;
    (* XXX need switch *)
    Format.fprintf ppf "@[%a@]@." (Option.format Hump.print_v) x.DB.v
  in
  match nonaliased with
  | [] ->
      !!% "@[<v4>%d : @[%a@]@ aliases:@   @[<v>%a@]@]@.@."
        d
        format_alias a
        (Format.list "@ " format_item) aliased
  | _ ->
      match aliased with
      | [] ->
          !!% "@[%d : @[<v>%a@]@]@.@."
            d
            (Format.list "@ " format_item) nonaliased
      | _ -> 
          !!% "@[<v4>%d : @[<v>%a@]@ aliases:@   @[<v>%a@]@]@.@."
            d
            (Format.list "@ " format_item) nonaliased
            (Format.list "@ " format_item) aliased
            
let group_and_print ppf = final_print ppf *< group
