open Spotlib.Spot
open Utils
open List

type k = 
    | KValue     
    | KType      
    | KTypext
    | KModule    
    | KModtype
    | KClass
    | KClassType
  [@@deriving conv{ocaml_of}]
  
type t = (k * Ident.t) * value * doc

and env = t list

and locinfo =
  | Def of Location.t (** The definition code is here *)
  | LocNone           (** No location *)
  | LocConstrain of locinfo * locinfo (** a by b *)
  | LocIncluded of locinfo * locinfo (** object defined at b included at a *)

and doc = string list
    
and value = 
  | Value    of locinfo 
  | Type     of locinfo * type_kind
  | Typext   of locinfo
  | Module   of locinfo * module_
  | Modtype  of locinfo * module_ option
  | Alias    of locinfo * k * Ident.t * module_
  | VConstrain of value * value

and type_kind =
  | Abstract (* Note: it may still have a manifest type *)
  | Open
  | Record of (Ident.t * locinfo * doc) list
  | Variant of (Ident.t * locinfo * doc) list (* TODO: record arg *)

(* At this stage, we do not evaluate functor applications *)
and module_ =
  | Structure of env
  | Functor of Ident.t * module_ option * module_ (* we need env *)
  | App of module_ * module_
  | Constraint of module_ * module_
  | Var of Path.t

[@@deriving conv{ocaml_of}]

let format = Ocaml.format_with ocaml_of_t

let rec k_of_value = function
  | Value   _ -> KValue
  | Type    _ -> KType
  | Typext  _ -> KTypext
  | Module  _ -> KModule
  | Modtype _ -> KModtype
  | Alias (_,k,_,_) -> k
  | VConstrain (v1,_) -> k_of_value v1

let constrain_locinfo l ~by = match l, by with
  | l, LocNone -> l
  | LocNone, by -> by
  | _ -> LocConstrain (l,by)

let included_locinfo ~at l = LocIncluded (at,l)

(* CR jfuruse: It is not really constrain, but included *)
let rec alias_locinfo ~at v = 
  match v with
  | Value l -> Value (included_locinfo ~at l)
  | Type  (l,tk) -> Type (included_locinfo ~at l, tk)
  | Typext l -> Typext (included_locinfo ~at l)
  | Module (l,m) -> Module (included_locinfo ~at l, m)
  | Modtype (l,mo) -> Modtype (included_locinfo ~at l, mo)
  | Alias (l,k,i,m) -> Alias (included_locinfo ~at l,k,i,m)
  | VConstrain (v1,v2) -> VConstrain (alias_locinfo ~at v1, alias_locinfo ~at v2) (*!?!?! *)

let rec constrain_value v ~by = match v, by with
  | Value l, Value by -> Value (constrain_locinfo l ~by) 
  | _, (Value LocNone | Typext LocNone) -> v
  | Type (l,tk), Type (by,tk') ->
      Type (constrain_locinfo l ~by, constrain_type_kind tk tk')
  | Typext l, Typext by -> Typext (constrain_locinfo l ~by)
  | Module (l, m), Module (by, m') -> Module (constrain_locinfo l ~by, constrain_module m m')
      
  | Modtype (l, None), Modtype (by, None) -> Modtype (constrain_locinfo l ~by, None)
  | Modtype (l, Some m), Modtype (by, Some m') -> Modtype (constrain_locinfo l ~by, Some (constrain_module m m'))
  | _ -> VConstrain (v, by)


and constrain_type_kind tk tk' = match tk, tk' with
  | _, Abstract -> Abstract
  | _, Open -> Open
  | Record fields, Record fields' ->
      Record (flip map fields' (fun (id',by,doc') ->
        match find_opt (fun (id,_l,_doc) -> id.Ident.name = id'.Ident.name) fields with
        | None -> assert false
        | Some (_id, l, doc) -> id', constrain_locinfo l ~by, doc'@doc))
  | Variant fields, Variant fields' ->
      Variant (flip map fields' (fun (id',by,doc') ->
        match find_opt (fun (id,_l,_doc) -> id.Ident.name = id'.Ident.name) fields with
        | None -> assert false
        | Some (_id, l, doc) -> id', constrain_locinfo l ~by, doc'@doc))
  | _ -> assert false

and constrain_t t t' = match t, t' with
  | ( (k, v, doc ), (k', v', doc') ) when k = k' -> 
      (k, constrain_value v ~by:v', doc' @ doc)
  | _ -> assert false
      
and constrain_module m by = match m, by with
  | Structure ts, Structure bys -> 
      let kvs = rev_map (fun ((k,id),_,_ as t) -> (k,id.Ident.name),t) ts 
      in
      let ts' = flip map bys & fun ((k',id'),_,_ as t') ->
        match assoc_opt (k', id'.Ident.name) kvs with
        | None -> assert false
        | Some t -> constrain_t t t'
      in
      Structure ts'
  | _ -> Constraint (m, by)


(*
  | Type     of locinfo * type_kind
  | Module   of locinfo * module_
  | Modtype  of locinfo * module_ option
  | Alias    of locinfo * k * Path.t
*)

let rec eval t = match t with
  | (KValue, _), Value _, _
  | (KType, _), Type _, _
  | (KTypext, _), Typext _, _
  | (KModtype, _), Modtype (_, None), _ -> t
  | (KModule, id), Module (loc, m), doc ->
      (KModule, id), Module (loc, module_ m), doc
  | (KModtype, id), Modtype (loc, Some m), doc ->
      (KModtype, id), Modtype (loc, Some (module_ m)), doc
  | (k,id), Alias (loc, k', id', m), doc ->
      assert (k = k');
      assert (id.Ident.name = id'.Ident.name);
      let m = module_ m in
      begin match m with
      | Structure ts ->
          let kvs = rev_map (fun ((k,id),_,_ as t) -> (k,id.Ident.name),t) ts in
          begin match assoc_opt (k,id.Ident.name) kvs with
          | None -> t
          | Some ((k'',id''), v'',  doc'') ->
              assert (k = k'');
              assert (id.Ident.name = id''.Ident.name);
              (k'',id''), alias_locinfo ~at:loc v'', doc'' @ doc
          end
      | _ -> (k,id), Alias (loc, k', id', m), doc
      end
  | _ -> assert false

and env ts = map eval ts
  
and module_ m = match m with 
  | Structure ts -> Structure (env ts)
  | App (m,m') -> App (module_ m, module_ m')
  | Functor (id, None, m) -> Functor (id, None, module_ m)
  | Functor (id, Some m, m') -> Functor (id, Some (module_ m), module_ m')
  | Var p -> Var p
  | Constraint (m, by) ->
      let m = module_ m in
      let by = module_ by in
      constrain_module m by
