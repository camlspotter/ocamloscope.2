open Spotlib.Spot
open Utils
open List

open Hump

module H = Hump.Make(Ident)
open H

let eq_ident id1 id2 = id1.Ident.name = id2.Ident.name

let eq_k (k1,id1) (k2,id2) = k1 = k2 && eq_ident id1 id2

let rec coerce ~by m = match m, by with
  | Structure ms, Structure bys ->
      let ms = rev ms in
      Structure (flip map bys & function 
        | ((KValue | KType), _ as k), v, doc ->
            from_Some & flip find_map_opt ms & fun (k', v', doc') ->
              if eq_k k k' then
                let doc = Option.(doc' >>=! fun () -> doc) in
                match v with
                | Value LocNone | Type LocNone -> Some (k',v',doc)
                | _ -> 
                    Some (k', CoerceBy (fst k', v, v'), doc)
              else None
        | (KModule, _ as k), (Module (_l, mby) as v), doc ->
            from_Some & flip find_map_opt ms & fun (k', v', doc') ->
              if eq_k k k' then
                let doc = Option.(doc' >>=! fun () -> doc) in
                match v' with
                | Module (l',m') ->
                    (* should fix l' by l? *)
                    Some (k', Module (l', coerce ~by:mby m'), doc)
                | _ -> 
                    Some (k', CoerceBy (fst k', v, v'), doc)
              else None
         | _ -> assert false)
  | Functor _, Functor _ -> assert false
  | _ -> assert false

let env_of_structure xs = concat & flip rev_map xs & function
    | (Value _ | Type _), _ -> []
    | ((Module _, _) as x) -> [x]
    | _ -> assert false

let rec find_module env = let open Path in function
  | Pident id ->
      flip find_map_opt env (function
        | (k', Module (_,m), _doc) when eq_k (KModule, id) k' -> Some m
        | _ -> None)
  | Pdot (p, n, _) ->
      begin match find_module env p with
      | None -> assert false
      | Some (Structure env) -> 
          find_module env (Pident (Ident.create n)) (* dummy *)
      | Some _ -> assert false
      end
  | _ -> assert false

let find_k env k = flip find_map_opt env & fun (k',v') -> if eq_k k k' then Some v' else None

let store_env k v env = match k with
  | (KModule, _) -> (k,v)::env
  | ((KValue | KType | KTypext | KModtype | KClass | KClassType), _) -> env
    
let rec eval env = function
  | Value l -> Value l
  | Type l  -> Type l
  | Typext l -> Typext l
  | Module (l, m) -> Module (l, module_ env m)
  | Alias (_l, k, mp, i) as v -> (* CR jfuruse: we lose _l *)
      begin match Path.is_global mp with
      | Some _ -> v
      | None ->
        match find_module env mp with
        | None -> 
            !!% "Path %s is not found!@." (Path.string_of mp);
            assert false
        | Some (Functor _) -> assert false
        | Some (Structure menv) -> from_Some & find_k menv (k, i)
        (* handling of   include (struct module M = ... end) *)
        | Some _ -> v
      end
  | _ -> assert false

and module_ env = function
  | Structure kvs -> 
      let _, rkvs =
        fold_left (fun (env,rkvs) (k,v) ->
          let v = eval env v in
          let env = store_env k v env in
          env, (k,v)::rkvs) (env, []) kvs
      in
      Structure (rev rkvs)
  | Constraint (m, by) ->
      let m = module_ env m in
      let by = module_ env by in
      coerce ~by m
  | Var p when Path.is_global p <> None -> Var p
  | Var p ->
      begin match find_module env p with 
      | Some m -> m
      | None -> assert false
      end
  | Functor (id, mopt, m) ->
      let env = ((KModule, id), Module (LocNone, Var (Pident id))) :: env in
      let mopt = Option.fmap (module_ env) mopt in
      let m = module_ env m in
      Functor (id, mopt, m)
  | App (m1, m2) ->
      let m1 = module_ env m1 in
      let m2 = module_ env m2 in
      match m1 with
      | Structure _ -> assert false
      | Functor (id, None, m) ->
          let env = ((KModule, id), Module (LocNone, m2))::env in
          module_ env m
      | Functor (id, Some mty, m) ->
          let m2 = coerce ~by:mty m2 in
          let env = ((KModule, id), Module (LocNone, m2))::env in
          module_ env m
      | _ -> App (m1, module_ env m2)
          
