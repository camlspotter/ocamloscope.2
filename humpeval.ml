open Spotlib.Spot
open List

open Hump

let no_alpha s =
  try
    let p = String.index s ' ' in
    String.sub s 0 p
  with
  | Not_found -> s

let (===) n1 n2 = no_alpha n1 = no_alpha n2

let assoc_opt_x (k,n) =
  let n = no_alpha n in
  find_map_opt (fun ((k',n'),v) ->
    if k = k' && n = no_alpha n' then Some v else None)
  
(* let expr = Hashcons.hump_expr *)
let expr = id
  
(* XXX I do not know the evaluation of module X = M in SIGNATURE *)    

let coerce_v v1 v2 = match v1, v2 with
  | _ when v1 = v2 -> v1 (* it should not take long time to compare them *)
  | _, LocNone -> v1
  | LocNone, v2 -> v2 (* this happens for functor arguments *)
  | Coerced (_v11, v12), v3 when v12 = v3 -> v1
  | _ -> Coerced (v1,v2)

let rec add_alias v' e = match e with
  | ELet (b, xs, e) -> ELet (b, xs, add_alias v' e)
  | ESignature ies -> ESignature (map (fun (i,e) -> (i, add_alias v' e)) ies)
  | EModule (v, e) -> EModule (Aliased (v', v), add_alias v' e)
  | EModtype (v, eo) -> EModtype (Aliased (v', v), Option.fmap (add_alias v') eo)
  | EFunctor (id, mtyo, e) -> EFunctor (id, mtyo (*?*), add_alias v' e)
  | EType (v, ies) -> EType (Aliased (v', v), map (fun (i,e) -> (i, add_alias v' e)) ies)
  | ETypext v -> ETypext (Aliased (v', v))
  | EValue v -> EValue (Aliased (v', v))
  | EClass (v, ies) -> EClass (Aliased (v', v), map (fun (i,e) -> (i, add_alias v' e)) ies)
  | EClasstype (v, ies) -> EClasstype (Aliased (v', v), map (fun (i,e) -> (i, add_alias v' e)) ies)
  | EConstructor v -> EConstructor (Aliased (v', v))
  | EField v -> EField (Aliased (v', v))
  | EMethod v -> EMethod (Aliased (v', v))

  | EUnknownPath _ 
  | ERecM _
      -> e
  | EError _ -> e

  (* not expanded ? :-( *)
  | EAnnotate _
  | EGVar _
  | EVar _
  | EDot _
  | EApply _
  | ECoerce _
  | EWith _
  | EAddAlias _
    ->
(*
EAddAlias failed for EDot
                       (ECoerce
                          (EVar
                             (KModule, "Base \"{batteries}.BatNumber\" 1586"),
                           ESignature
                             ([ ((KType, "t"), EType (LocNone, []));
                                ((KValue, "compare"), EValue (LocNone));
                        KValue, "compare")
*)
      (* !!% "EAddAlias failed for %a@." Hump.format e; *)
      EAddAlias(v', e)

module Make(A : sig
  val global_source : Hump.path -> Hump.expr option
  val go_on_even_at_coercion_errors : bool
end) = struct

  let global_cache = Hashtbl.create 17

  exception Coercion_failure
    
  let rec eval_global p = match Hashtbl.find_opt global_cache p with
    | Some (`Ok x) -> Some x
    | Some (`Error e) ->
        !!% "Humpeval.Make(..).eval_global: %a: %s@." (Ocaml.format_with Hump.ocaml_of_path) p e;
(*
        criticalf "Humpeval.Make(..).eval_global: %s" e
 *)
        Some (EError "looped")
    | None ->
        match A.global_source p with
        | None ->
            !!% "Warning: Eval: %a is not registered@." (Ocaml.format_with Hump.ocaml_of_path) p;
            None (* it is not registered *)
        | Some x ->
            !!% "Eval %a...@." (Ocaml.format_with Hump.ocaml_of_path) p;
            Hashtbl.replace global_cache p (`Error "loop");
            let x = eval [] x in
            Hashtbl.replace global_cache p (`Ok x);
            !!% "Eval %a done!@." (Ocaml.format_with Hump.ocaml_of_path) p;
            Some x
    
  and eval e x0 = match x0 with
    | EError _ -> x0
    | ERecM _ -> x0 (* no further evaluation of recursive module *)
    | EAddAlias (v,x) ->
        let x = eval e x in
        add_alias v x
        
    | EVar (k,n) ->
        begin match assoc_opt (k,n) e with
        | Some (Some v) -> v
        | Some None -> x0
        | None -> x0 (* XXX This is very strange *)
        end
    | EGVar p -> eval_global p // x0
    | EDot (EUnknownPath p,_k,n) -> EUnknownPath (Oide_dot (p,n))
    | EDot (x,k,n) ->
        (* We do not evaluate [x] here.

           Otherwise, O(n^2) calcuation
           
           let *include* = M in   <- here *include* must be evaluated to a signature
           let x = *include*.x in
           let y = *include*.y in
           let z = *include*.z in
           ...
           
        *)
        
        let x = eval e x in
        let rec f = function
          | EModule (_, x) -> f x
          | EModtype (_, Some x) -> f x
          | ESignature vs ->
              let rev_vs = rev vs in
              (* failure here is very strange and we should make it an error *)
              begin match assoc_opt (k,n) rev_vs with
              | None -> EDot (x,k,n)
              | Some v -> v
              end
          | ECoerce (x,y) -> ECoerce (f x, f y)
          | _ -> 
              (* If something missing, [x] may not be evaluated to a signature
              *)
              EDot (x,k,n)
        in
        f x
    | ELet (false,vbs,x) ->
        (* Hope things are evaluated to some sort of normal form *)
        let vbs = map (fun (id,x) -> (id, eval e x)) vbs in
        let e = map (fun (id,x) -> (id,Some x)) vbs @ e in
        eval e x
    | ELet (true,vbs,x) ->
        let e = map (fun (id,_) -> (id, Some (ERecM (id,e,vbs)))) vbs @ e in
        (* NOTE: This explodes vbs if eval'ed repeatedly! *) 
        let vbs = map (fun (id,x) -> (id, eval e x)) vbs in
        let e = map (fun (id,x) -> (id, Some x)) vbs @ e in
        eval e x

    | ESignature vbs ->
        ESignature (map (fun (id,x) -> (id,eval e x)) vbs)

    | EApply (x1, x2) ->
        let x2 = eval e x2 in
        let rec f = function
          | EModule (_, x) -> f x
          | EFunctor (_id, None, x) -> 
              (* I believe x2 is evaluated to ESignature [] *)
              eval e x
          | EFunctor (id, Some _mty, x) ->
              let e = (id, Some x2)::e in
              eval e x
          | x1 -> EApply (x1, x2)
        in
        f & eval e x1

    (* We evaluate the body: 
       when flattened, we want to see the internal of functors *)           
    | EFunctor(id, None, x) ->
        EFunctor(id, None, eval ((id, None) :: e) x)
    | EFunctor(id, Some mty, x) ->
        let mty = eval e mty in
        EFunctor(id, Some mty, eval ((id, None) :: e) x)

    | ECoerce(x1,x2) -> (* x1 by x2 *)
        let x1 = eval e x1 in
        let x2 = eval e x2 in
        let rec coerce e x1 x2 =
          let coerce_vbs e ixs1 ixs2 =
            let rev_ixs1 = rev ixs1 in
            flip map ixs2 & fun (id,ex2) ->
              (id,
               match assoc_opt_x id rev_ixs1 with
               | None ->
                   (* likely a bug of scraper or evaluator *)
                   !!% "@[<v2>Coerce failure. Not found %a :@ %a@ by %a@]@."
                     (Ocaml.format_with ocaml_of_id) id
                     Hump.format x1
                     Hump.format x2;
                   raise Coercion_failure
               | Some ex1 -> coerce e (*?*) ex1 ex2
              )
          in
          let doit f =
            try f () with Coercion_failure ->
              if not A.go_on_even_at_coercion_errors then assert false
              else EAnnotate("Coercion error", ECoerce(x1,x2))
          in
          doit & fun () -> begin match x1, x2 with
            | EVar (KModule, n), EVar (KModule, n') when n === n' ->
                (* This happens when a functor argument is coerced by mli *)
                x1 
            | _, EValue LocNone -> x1
            | EValue LocNone, _ -> x2
            | EValue v1, EValue v2 -> expr & EValue (coerce_v v1 v2)
            | ETypext v1, ETypext v2 -> expr & ETypext (coerce_v v1 v2)
            | EConstructor v1, EConstructor v2 -> expr & EConstructor (coerce_v v1 v2)
            | EUnknownPath _, EUnknownPath p -> expr & EUnknownPath p
            | EField v1, EField v2 -> expr & EField (coerce_v v1 v2)
            | EMethod v1, EMethod v2 -> expr & EMethod (coerce_v v1 v2)
      
            | EModule (v1, x1), EModule (v2, x2) ->
                EModule (coerce_v v1 v2, coerce e x1 x2)
            | EModule (d, v1), v2 -> EModule (d, coerce e v1 v2)
            | v1, EModule (_, v2) -> coerce e v1 v2
    
            | EModtype (v1, _), EModtype (v2, None) ->
                EModtype (coerce_v v1 v2, None)
            | EModtype (v1, Some mty1), EModtype (v2, Some mty2) ->
                EModtype (coerce_v v1 v2, Some (coerce e mty1 mty2))
            | EModtype (d, Some v1), v2 -> EModtype (d, Some (coerce e v1 v2))
            | v1, EModtype (_, Some v2) -> coerce e v1 v2
    
            | EType (v1, x1), EType (v2, x2) ->
                expr & EType (coerce_v v1 v2, coerce_vbs e x1 x2)
    
            | EClass (v1, x1), EClass (v2, x2) ->
                expr & EClass (coerce_v v1 v2, coerce_vbs e x1 x2)
            | EClasstype (v1, x1), EClasstype (v2, x2) ->
                expr & EClasstype (coerce_v v1 v2, coerce_vbs e x1 x2)
      
            | EFunctor (id1, None, x1), EFunctor (_id2, None, x2) ->
                (* id1 === id2 but id1 <> id2 *)
                let e = (id1, None)::e in
                EFunctor (id1, None, coerce e x1 x2)
      
            | EFunctor (id1, Some mty1, x1), EFunctor (_id2, Some mty2, x2) ->
                (* id1 === id2 but id1 <> id2 *)
                let e' = (id1, None)::e in
                (* It is opposite in the functor arg! *)
                EFunctor (id1,
                          Some (coerce e mty2 mty1), (* XXX is it fine to have reversed location coerce? *)
                          coerce e' x1 x2)
      
            | ESignature ixs1, ESignature ixs2 ->
                ESignature (coerce_vbs e ixs1 ixs2)
    
            | _ ->
  (*
                !!% "@[<2>WARNING: unable to coerce.@ @[%a@]@." format x1;
                !!% "@[<2>and@ @[%a@]@." format x2;
  *)
                ECoerce (x1,x2)
          end
        in
        coerce e x1 x2

    | EWith(x1, x2) ->
        (* This is x1 but values of x2 should be copied to x1
           (well, not quite correct... but)
        *)
        let x1 = eval e x1 in
        let x2 = eval e x2 in
        let rec with_ e x1 x2 = 
          let with_vbs e ixs1 ixs2 =
            let rev_ixs2 = rev ixs2 in
            flip map ixs1 & fun (id,ex1) ->
              (id,
               match assoc_opt_x id rev_ixs2 with
               | None ->
                   (* Unlike ECoerce, it is possible that x1 has extra fields than x2 *)
                   ex1
               | Some ex2 -> with_ e (*?*) ex1 ex2
              )
          in
          begin match x1, x2 with
          | EVar (KModule, n), EVar (KModule, n') when n === n' -> x1 
          | _, EValue LocNone -> x1
          | EValue LocNone, _ -> x2
          | EValue v1, EValue v2 -> expr & EValue (coerce_v v1 v2)
          | ETypext v1, ETypext v2 -> expr & ETypext (coerce_v v1 v2)
          | EConstructor v1, EConstructor v2 -> expr & EConstructor (coerce_v v1 v2)
          | EUnknownPath p, EUnknownPath _ -> expr & EUnknownPath p
          | EField v1, EField v2 -> expr & EField (coerce_v v1 v2)
          | EMethod v1, EMethod v2 -> expr & EMethod (coerce_v v1 v2)
              
          | EModule (v1, x1), EModule (v2, x2) ->
              EModule (coerce_v v1 v2, with_ e x1 x2)
          | EModule (d, v1), v2 -> EModule (d, with_ e v1 v2)
          | v1, EModule (_, v2) -> with_ e v1 v2
              
          | EModtype (v1, _), EModtype (v2, None) ->
              EModtype (coerce_v v1 v2, None)
          | EModtype (v1, Some mty1), EModtype (v2, Some mty2) ->
              EModtype (coerce_v v1 v2, Some (with_ e mty1 mty2))
          | EModtype (d, Some v1), v2 -> EModtype (d, Some (with_ e v1 v2))
          | v1, EModtype (_, Some v2) -> with_ e v1 v2
              
          | EType (v1, x1), EType (v2, x2) ->
              expr & EType (coerce_v v1 v2, with_vbs e x1 x2)
    
          | EClass (v1, x1), EClass (v2, x2) ->
              expr & EClass (coerce_v v1 v2, with_vbs e x1 x2)
          | EClasstype (v1, x1), EClasstype (v2, x2) ->
              expr & EClasstype (coerce_v v1 v2, with_vbs e x1 x2)
      
          | EFunctor (id1, None, x1), EFunctor (_id2, None, x2) ->
              (* id1 === id2 but id1 <> id2 *)
              let e = (id1, None)::e in
              EFunctor (id1, None, with_ e x1 x2)
      
          | EFunctor (id1, Some mty1, x1), EFunctor (_id2, Some mty2, x2) ->
              (* id1 === id2 but id1 <> id2 *)
              let e' = (id1, None)::e in
              (* It is opposite in the functor arg! *)
              EFunctor (id1,
                        Some (with_ e mty2 mty1), (* XXX correct?!?!? *)
                        with_ e' x1 x2)
      
          | ESignature ixs1, ESignature ixs2 ->
              ESignature (with_vbs e ixs1 ixs2)
    
          | _ ->
              EWith (x1,x2)
          end
        in
        with_ e x1 x2
        
    | EModule (v, x) ->
        begin match EModule (v, eval e x) with
        | EModule (v, EModule (v', x)) ->
            let alias v v' = match v, v' with
              | _, LocNone -> v
              | _ -> Aliased (v, v')
            in
            EModule (alias v v', add_alias v (*XXX We want to have Loc *) x)
        | x -> x
        end
    | EModtype (_, None) -> x0
    | EModtype (v, Some x) ->
        begin match EModtype (v, Some (eval e x)) with
        | EModtype (v, Some (EModtype (v', x))) ->
            let alias v v' = match v, v' with
              | _, LocNone -> v
              | _ -> Aliased (v, v')
            in
            EModtype (alias v v', Option.fmap (add_alias v (* XXX We want to have Loc *)) x)
        | x -> x
        end

    | EType (v, vbs) ->
        expr & EType (v, map (fun (id,x) -> (id, eval e x)) vbs)
    | EClass (v, vbs) ->
        expr & EClass (v, map (fun (id,x) -> (id, eval e x)) vbs)
    | EClasstype (v, vbs) -> 
        expr & EClasstype (v, map (fun (id,x) -> (id, eval e x)) vbs)
  
    | ETypext _
    | EValue _
    | EConstructor _
    | EField _
    | EMethod _ -> x0
    | EUnknownPath _
    | EAnnotate _ -> x0
end
