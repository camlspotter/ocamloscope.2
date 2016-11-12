open Spotlib.Spot
open List
open Tyxml
module H = Html

let html_to_string html =
  let b = Buffer.create 1000 in
  let ppf = Format.formatter_of_buffer b in
  H.pp () ppf html;
  Format.pp_print_flush ppf ();
  Buffer.contents b

(** Same as [html_to_string] but takes any elt *)
let html_elt_to_string html =
  let b = Buffer.create 1000 in
  let ppf = Format.formatter_of_buffer b in
  H.pp_elt () ppf html;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let%html oc_header = {|
  <head>
    <title>OCamlOScope.2 server</title>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="style.css">
  </head>
|}
    
let spans ?a s = H.span ?a [ H.pcdata s ]

let query_form pspec (v : string) =
  let open Query.PackageSpec in
  let packs = match pspec with
    | All_but xs -> xs
    | Just    xs -> xs
    | Vanilla xs -> xs
  in
  let mk_option v p =
    H.a_value v
    :: if p pspec then [ H.a_selected () ] else []
  in
  [%html {| 
     <div class="query">
       <span class="logo">OC&#x1f441;</span>
       <form action="/" method="get"> |}
         [ H.pcdata "Query: "; H.input ~a:[ H.a_input_type `Text; H.a_id "q"; H.a_name "q"; H.a_value v] ()
         ; H.input ~a:[ H.a_input_type `Submit; H.a_id "submit" ] () 
         ; H.br ()
         ; H.pcdata " Packages: "
         ; H.select ~a: [ H.a_name "packtype"; H.a_id "packtype" ]
           [ H.option ~a:(mk_option "vanilla" (function Vanilla _ -> true | _ -> false)) (H.pcdata "Vanilla and")
           ; H.option ~a:(mk_option "allbut" (function All_but _ -> true | _ -> false)) (H.pcdata "All but")
           ; H.option ~a:(mk_option "just" (function Just _ -> true | _ -> false)) (H.pcdata "Just")
           ]
         ; H.pcdata " "
         ; H.input ~a:[ H.a_input_type `Text; H.a_name "packs"; H.a_id "packs"; H.a_value (String.concat " " packs)] () ]
     {|</form>
     </div>
  |} ]

let path_last = function
  | Outcometree.Oide_dot (_, s) -> Outcometree.Oide_ident s
  | x -> x

let hpath p = spans ~a:[H.a_class ["path"]] & Format.sprintf "%a" Xoprint.print_ident p

let fsignature_item orgdoc (i : Data.DB.item) =
  let open Data.DB in
  let open Sig in
  let open Outcometree in
  let module M = Sigext.Print.Make(struct let simplif_path = Sigext.Print.path_simplifier i.kind i.path end) in

  let string_of_type ty = Format.sprintf "%a" !Xoprint.out_type (M.simplif_type ty) in
  let htype ty = spans ~a:[H.a_class ["type"]] & string_of_type ty in

  let htyparams ?(cls=false) = function
    | [] -> []
    | [x] -> [ htype x; spans " " ]
    | xs when cls -> spans ~a:[H.a_class ["type"]] "[" :: intersperse (spans ", ") (map htype xs) @ [ spans "] " ]
    | xs -> spans ~a:[H.a_class ["type"]] "(" :: intersperse (spans ", ") (map htype xs) @ [ spans ") " ]
  in
    
  let ((k,p),res) = fsignature_item i in
  let hk = spans ~a:[H.a_class ["kind"]] (Sig.string_of_k k ^ " ") in
  let hp = hpath p in
  let fsig = 
    H.div ~a: [ H.a_class [ "fsig" ] ] &
    match res with
    | FValue (ty, _) -> [ hk; hp; spans " : "; htype ty ]
    (* | FValue (ty, SVal_prim) -> [ spans "external"; hp; spans " : "; htype ty ] *)
    | FModule _ -> [ hk; hp; spans " : "; spans "sig ... end" ]
    | FModtype None -> [ hk; hp ]
    | FModtype (Some _) -> [ hk; hp; spans " = "; spans "sig ... end" ]
    | FType (pars, tk, pf, aliaso, rs) -> (* Trec_not or other *)
        hk
        :: (match rs with Trec_not -> [ spans "norec" ] | _ -> [])
        @ htyparams pars
        @ [ hp ]
        @ (match aliaso with None -> [] | Some ty -> [ spans " = "; htype ty ])
        @ (match tk with
           | FAbstract -> []
           | FOpen -> [ spans " = "; spans ".." ]
           | FRecord fsig ->
               let unarrow = function
                 | Otyp_arrow (_, _, ty) -> ty
                 | ty -> Otyp_tuple [ Otyp_stuff "unarrow failed"; ty ]
               in
               let field = function
                 | ((_, p), FRecordField (Immutable, ty)) ->
                     H.span [ hpath & path_last p
                            ; spans " : "
                            ; htype & unarrow ty
                            ]
                 | ((_, p), FRecordField (Mutable, ty)) ->
                     H.span [ spans "mutable "
                            ; hpath & path_last p
                            ; spans " : "
                            ; htype & unarrow ty
                            ]
                 | _ -> assert false
               in
               spans " = "
               :: (if pf = Private then [ spans "private" ] else [])
               @ [ spans "{";
                   H.div (tl & concat_map (fun s -> [ H.br (); H.space (); H.space (); field s; spans ";" ]) fsig);
                   spans "}" ]
           | FVariant fsig ->
               let constr = function
                 | ((_, p), FVariantConstructor (ty, None)) ->
                     begin match ty with
                     | Otyp_arrow (_, ty, _) ->
                         H.span [ H.space (); H.space (); spans "| "; hpath & path_last p; spans " of "; htype ty ] 
                     | _ ->
                         H.span [ H.space (); H.space (); spans " | "; hpath & path_last p] 
                     end
                 | ((_, p), FVariantConstructor (ty, Some _)) -> (* XXX I guess the return type is integrated into ty already *)
                     H.span [ H.space (); H.space (); spans "| "; hpath & path_last p; spans " : "; htype ty ] 
                 | _ -> assert false
               in
               spans " = "
               :: (if pf = Private then [ spans "private" ] else [])
               @ [ H.div (intersperse (H.br ()) & map constr fsig); ]
          )
    | FRecordField (Immutable, ty) -> [ hk; hp; spans " : "; htype ty ]
    | FRecordField (Mutable, ty) -> [ hk; spans "mutable"; hp; spans " : "; htype ty ]
    
    | FVariantConstructor (ty, None) -> [ hk; hp; spans " : "; htype ty ] 
    | FVariantConstructor (ty, Some _) -> [ hk; hp; spans " : "; htype ty ] (* XXX I guess the retun type is integrated into ty already *)
  
    | FTypext (ts, t', rto, pf) ->
        let args, rt  = match t' with
          | Otyp_arrow ("", ts, rt) ->
              begin match ts with
              | Otyp_tuple ts -> ts, rt
              | Otyp_record _ -> [ts], rt
              | _ -> assert false
              end
          | _ -> assert false
        in
        let oext_type_name = match rt with
          | Otyp_constr (oi, _) -> hpath oi
          | _ -> assert false
        in
        hk
        :: htyparams ts
        @ [ oext_type_name; spans "+=" ]
        @ (if pf = Private then [ spans "private" ] else [])
        @ [ hp ]
        @ (match rto with
           | None ->
               begin match args with
               | [] -> []
               | _ -> [ spans " of "; htype (Otyp_tuple args) ]
               end
           | Some rty ->
               begin match args with
               | [] -> [ spans " : "; htype rty ]
               | _ -> [ spans " : "; htype (Otyp_arrow ("", Otyp_tuple args, rty)) ]
               end)
    | FClass (tys, _fs, t, _p, (vf, _r)) -> 
  (*
        let clt = 
          let rec f = function
            | Otyp_arrow (s, t1, t2) -> Octy_arrow (s, t1, f t2)
            | Otyp_object (meths, _ (* CR jfuruse: todo None: closed, Some true: _.., Some false: .. *) ) ->
                (* CR jfuruse: we should use fs instead of t? *)
                Octy_signature (None (* CR jfuruse: self_ty*), 
                                flip map meths & fun (s,t) ->
                                  Ocsg_method (s, true (*?*), true(*?*), t))
            | Otyp_alias (t, a) ->
                (* not sure... *)
                begin match f t with
                | Octy_signature (None, xs) ->
                    Octy_signature (Some (Otyp_var (false, a)), xs)
                | _ -> assert false
                end
            | t -> 
                !!% "?!?!: %s@." (string_of_type t);
                assert false
          in
          f & M.simplif_type t
        in
  *)
        hk
        :: (if vf = Virtual then [ spans "virtual" ] else [])
        @ htyparams ~cls:true tys
        @ [hp] (* XXX how abstract class is encoded?? *)
        @ [ spans " : "; htype t ]
  
    | FClassType (pars, _fs, t, _p, (vf, _r)) ->
  (*
        let clt = 
          let rec f = function
            | Otyp_arrow (s, t1, t2) -> Octy_arrow (s, t1, f t2)
            | Otyp_object (meths, _ (* CR jfuruse: todo None: closed, Some true: _.., Some false: .. *) ) ->
                (* CR jfuruse: we should use fs instead of t? *)
                Octy_signature (None (* CR jfuruse: self_ty*), 
                                flip map meths & fun (s,t) ->
                                  Ocsg_method (s, true (*?*), true(*?*), t))
            | Otyp_alias (t, a) ->
                (* not sure... *)
                begin match f t with
                | Octy_signature (None, xs) ->
                    Octy_signature (Some (Otyp_var (false, a)), xs)
                | _ -> assert false
                end
            | t -> 
                !!% "?!?!: %s@." (string_of_type t);
                assert false
          in
          f & M.simplif_type t
        in
  *)
        hk
        :: (if vf = Virtual then [ spans "virtual" ] else [])
        @ htyparams ~cls:true pars
        @ [hp] (* XXX how abstract class is encoded?? *)
        @ [ spans " = "; htype t ]
  
    | FMethod ty -> [ hk; hp; spans " : "; htype ty ]
    | FTypextRaw _
    | FVariantConstructorRaw _ -> assert false
  in
  let doc =
    match 
      orgdoc, Option.bind i.Data.DB.v Hump.get_doc
    with
    | _, None -> [ H.div ~a: [H.a_class [ "docstring" ]] [ H.pcdata "no doc" ] ]
    | Some orgd, Some d when orgd = d -> []
    | _, Some d ->  [ H.div ~a: [H.a_class [ "docstring" ]] [ H.pcdata d ] ]
  in
  H.div ~a: [ H.a_class [ "item" ] ]
    ( fsig
      :: doc
(*
      @ [ H.pre [H.pcdata & Format.sprintf "@[%a@]" Hump.print_v (match i.v with Some v -> v | None -> Hump.LocNone) ]]
*)
    )
    
let print_summary (sum : ( Data.alias
                           * int
                           * (int * Data.DB.item * 'trace1 * 'trace2) list list ) list) =
  let group i (alias, _dist, xss) =
    let nonaliased, aliased = partition (fun (_,i,_,_) -> i.Data.DB.alias = None) & flatten xss in
    let orgdoc = match filter_map (fun (_,i,_,_) -> Option.bind i.Data.DB.v Hump.get_doc) nonaliased with
      | [] -> None
      | [x] -> Some x
      | _ -> None
    in
    let nonaliased = match nonaliased with
      | [] ->
          H.div [
            match alias with
            | Data.Primitive n ->
                H.div ~a: [H.a_class [ "original" ]]
                  [ spans ~a:[H.a_class ["kind"]] "primitive "
                  ; spans n
                  ]
            | Path (k,p) ->
                H.div ~a: [H.a_class [ "original" ]]
                  [ spans ~a:[H.a_class ["kind"]] (Sig.string_of_k k ^ " ")
                  ; hpath p
                  ]
          ]
      | _ ->
          H.div & map (fun (_,i,_,_) -> fsignature_item None i) nonaliased
    in
    let aliased = match aliased with
      | [] -> []
      | _ -> [ H.div ~a:[H.a_class ["aliased"]]
               & H.div ~a:[H.a_class ["aliases"]] [ H.pcdata "aliases:" ]
                 :: map (fun (_,i,_,_) -> fsignature_item orgdoc i) aliased ]
    in
    H.div ~a:[ H.a_class [ "group" ^ string_of_int (i mod 2) ] ]
      & [ nonaliased ]
        @ aliased
        @ [ H.br () ]
  in
  H.div ~a:[H.a_id "result"] & mapi group sum
