open Spotlib.Spot
open List
open Parsetree
open Outcometree
open Asttypes

let rec out_of_longident = let open Longident in function
  | Lident s -> Oide_ident s
  | Ldot (l,s) -> Oide_dot (out_of_longident l, s)
  | Lapply (l,l') -> Oide_apply (out_of_longident l, out_of_longident l')

let out_of_core_type cty =
  let longident = out_of_longident in
  let arg_label = function
    | Nolabel -> ""
    | Labelled s -> s
    | Optional s -> "?" ^ s
  in
  let rec ty cty = match cty.ptyp_desc with
    | Ptyp_any -> Otyp_stuff "any"
    | Ptyp_var s -> Otyp_var (s.[0] = '_', s) (* CR jfuruse: should remove _ *)
    | Ptyp_arrow (l, cty1, cty2) -> Otyp_arrow (arg_label l, ty cty1, ty cty2) 
    | Ptyp_tuple ctys -> Otyp_tuple (map ty ctys)
    | Ptyp_constr ({txt= lid}, ctys) -> 
        Otyp_constr (longident lid, map ty ctys)
    | Ptyp_object (fields, closed) ->
        Otyp_object (
          flip map fields (fun (s, _, cty) -> (s, ty cty)),
          (match closed with 
          | Closed -> None
          | Open -> Some false (* ? *))
        )
    | Ptyp_class ({txt=lid}, ctys) ->
        (* (ty,..,ty) #a *)
        (* CR jfuruse: I am afraid lid does not contain # *)
        Otyp_constr ( longident lid, map ty ctys )
    | Ptyp_alias (cty, s) -> Otyp_alias (ty cty, s)
    | Ptyp_variant (row_fields, closed_flag, labels_opt) -> 
        (* Parsetree.mli says: *)
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])

           impos             (flag = Open;   labels = Some _) 


and row_field =
  | Rtag of label * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
         *)
(*
  | Otyp_variant of
      bool (*non_gen*) * out_variant * bool (*closed*) * (string list) option (*tags*)

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_name of out_ident * out_type list

*)

        let ovs =
          let fs = flip map row_fields & function
            | Rtag (l,_,b,tys) -> (l,b, map ty tys)
            | Rinherit _ -> assert false (* we should not take it *)
          in
          Ovar_fields fs
        in
        Otyp_variant (false,
                      ovs,
                      (match closed_flag with Open -> false | Closed -> true),
                      labels_opt)
  
    | Ptyp_poly ([], cty) ->
        (* object method type like x : int is actually marked as a poly in core_type *)
        ty cty

    | Ptyp_poly (vars, cty) -> Otyp_poly (vars, ty cty)
    | Ptyp_package _ -> assert false (* I guess it is no longer valid *)
    | Ptyp_extension _ -> assert false (* CR jfuruse: todo *)
  in
  ty cty
