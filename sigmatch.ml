open Spotlib.Spot
open List
open Option
open Outcometree

let fail = None

module PathLimit = struct

  let decr ?(by=1) score =
    let score = score - by in
    if score < 0 then None
    else Some score

end

module TypeLimit = struct

  type t = { score: int; 
             expansions: int;
           }

  let create score = { score; expansions = 50 }
  (* Type alias expansion is limited to 50. CR jfuruse: make it configurable. *)

  let decr ?(by=1) ({ score } as desc) =
    let score = score - by in
    if score < 0 then None
    else Some { desc with score }

  let max t1 t2 =
    match t1, t2 with
    | None, None -> None
    | None, _ -> t2
    | _, None -> t1
    | Some ({ score= s1 }, _), Some ({ score= s2 }, _) ->
        (* we ignore expansion *)
        if s1 >= s2 then t1 else t2

  let maxs a = fold_left1 max a

end

module Make( A: sig

  val cache : Levenshtein.StringWithHashtbl.cache

end) = struct

  (* XXX this filtering by the case of the first letter should be done
     by kinds, not char comparison *)
  let case_check s1 s2 = match s1, s2 with
    | "", "" -> true
    | "", _ | _, "" -> false
    | _ ->
        let case s = match String.unsafe_get s 0 with
          | 'a'..'z' -> 0
          | 'A'..'Z' -> 1
          | _ -> 2
        in
        case s1 = case s2
      
  let match_name_levenshtein
      : string  (*+ pattern *)
      -> string (*+ target *)
      -> int    (*+ limit (upperbound) *)
      -> int option
  = fun n0 m0 limit ->
    if not & case_check n0 m0 then fail else
    match n0 with
    | "_" | "_*_" -> return limit
    | "(_)" when m0 <> ""
                 && begin match String.unsafe_get m0 0 with 
                    | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' | '#' (* class type *) -> false
                    | _ -> true
                    end -> return limit
    | _ when n0 = m0 -> return limit
    | _ -> 
        let n = String.lowercase n0
        and m = String.lowercase m0 in
        let upper_bound = min (min (String.length n / 3) 3) limit + 1 in (* CR jfuruse: should be configurable *)
        let dist = 
          match Levenshtein.StringWithHashtbl.distance A.cache ~upper_bound n m with
          | Levenshtein.Exact n -> n
          | GEQ n -> n
        in
        if dist >= upper_bound then fail
        else return (limit - dist)

  let match_name_substring 
      : string  (*+ pattern *)
      -> string (*+ target *)
      -> int    (*+ limit (upperbound) *)
      -> int option
  = fun n0 m0 limit ->
    if not & case_check n0 m0 then fail else
    match n0 with
    | "_" | "_*_" -> return limit
    | "(_)" when m0 <> ""
                 && begin match String.unsafe_get m0 0 with 
                    | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' | '#' (* class type *) -> false
                    | _ -> true
                    end -> return limit
    | _ when n0 = m0 -> return limit
    | _ ->
        let n = String.lowercase n0
        and m = String.lowercase m0 in
        let score = 
          if String.is_substring ~needle:n m then
            limit - (String.length m - String.length n)
          else 0
        in
        if score <= 0 then fail 
        else return score

  let match_name = match true with
    | true  -> match_name_levenshtein
    | false -> match_name_substring

  (* or match_against_strings *)
  let match_package 
      : string (* pattern *)
      -> string list (* packages *)
      -> int (* limit (upperbound) *)
      -> int option
  = fun n ps limit ->
    fold_left (fun st p ->
      match st, match_name n p limit with
      | x, None -> x
      | (Some limit as x), Some limit' when limit >= limit' -> x
      | _, (Some _ as x) -> x
    ) None ps

(*
  let no_match_ident = Oide_ident ""
*)

  let rec match_path pat p limit : (int * _) option =
    match pat, p with
    | Oide_dot(n1, "_*_"), Oide_dot(m1, _m2) -> do_;
        (limit, d1) <-- max (match_path n1 m1 limit) (match_path pat m1 limit);
        return (limit, `Dot(d1, "_*_"))
    | Oide_ident n,   Oide_ident p when Some ps <-- Packpath.parse p -> do_;
        (* This is inefficient! *)
        limit <-- match_package n ps limit; 
        return (limit, `Ident n)
    | Oide_ident n,   Oide_dot(_m1, m2) -> do_;
        limit <-- match_name n m2 limit;
        return (limit, `Dot (`No, n))
    | Oide_ident n,   Oide_ident m -> do_;
        limit <-- match_name n m limit;
        return (limit, `Ident n)
    | Oide_dot(n1, n2), Oide_dot(m1, m2) -> do_;
        limit <-- match_name n2 m2 limit;
        (limit,d) <-- match_path n1 m1 limit;
        return (limit, `Dot (d, n2))
    | Oide_apply (li1, _li2), Oide_apply (p1, _p2) -> do_;
        (* A(B) matches with A(C) *)
        (* CR jfuruse: li2 is given but never used... *)
        (limit, d) <-- match_path li1 p1 limit;
        return (limit, `Apply(d, `No))
    | li, Oide_apply (p1, _p2) -> do_;
        (* A matches with A(C) but with slight penalty *)
        x <-- PathLimit.decr limit;
        (limit, d) <-- match_path li p1 x;
        return (limit, `Apply(d, `No))
    | _ -> fail

  let dummy_pattern_type_var = Otyp_stuff "any"
  let dummy_type_expr_var = Otyp_var (true, "") (* right?right? *)
    
  (* I never see good result with no_target_type_instantiate=false *)
  let match_type (* ?(no_target_type_instantiate=true) *) pattern target limit 
      : (TypeLimit.t * _) option
      =
    (* prof_type pattern target; *)
    let open TypeLimit in

    (* We try to remember matches like 'a = 'var. 
       If we see a match 'a = 'var2 later, we lower the limit slightly
       in order to give 'a = 'var higher score
       
       CRv2 jfuruse: We can extend this to var to non var type case,
       since the types are hcons-ed.  Ah, but we have subtyping...
    *)
    let var_match_history = Hashtbl.create 107 in
  
    let rec match_type pattern target limit =
      (* pattern = tvar is treated specially since it always returns the maximum score w/o changing the pattern *)
      match pattern, target with
      | Otyp_var (_, s), Otyp_var _ -> 
          begin match Hashtbl.find_opt var_match_history s with
          | None -> 
              Hashtbl.add var_match_history s (`Found target); 
              return (limit, `Var pattern)
          | Some (`Found target') when target == target' -> 
              return (limit, `Var pattern)
          | Some (`Found _) ->
              (* A variable matches with different types 
                 We mark this fact so that limit is lowered at most one time 
                 for one variable.
              *)
              Hashtbl.replace var_match_history s `MultiMatch;
              decr limit >>= fun limit -> 
              return (limit, `Var pattern)
          | Some `MultiMatch -> 
              return (limit, `Var pattern)
          end
          
      | Otyp_stuff "any", Otyp_var _ -> 
          (* return (limit, Attr (`Ref pattern, target)) *)
          (* Any is used for dummy pattern vars in match_types, so it should not be highlighted *)
          return (limit, `None)

      | Otyp_stuff "any", _ -> 
          (* Any matches anything but with a slight penalty. No real unification. *)
          decr limit >>= fun limit -> 
          return (limit, `None)
    
      | _ ->

      maxs [
        remove_target_type_option pattern target limit;
        make_tuple_left  pattern target limit;
        make_tuple_right pattern target limit;
        match_arrow_types pattern target limit;
  
        match_alias pattern target limit;
  
        (match pattern, target with
    
         | Otyp_tuple ts1, Otyp_tuple ts2 -> 
             match_types ts1 ts2 limit
             >>= fun (score, ds) -> return (score, `Tuple ds)
    
         | Otyp_constr (p1, ts1), Otyp_constr (p2, ts2) ->
            match_path p1 p2 limit.score >>= fun (score, pd) -> 
            match_types ts1 ts2 { limit with score } >>= fun (score, ds) -> 
            return (score, `Constr(`Path pd, ds))
  
         | _, Otyp_var _ ->
            decr ~by:(* (if no_target_type_instantiate then 1000 
                      else size_type pattern) *) 1000 limit
            >>= fun limit -> return (limit, `None)
    
         | _ -> fail)
      ]
    
    and match_alias _pattern _target _limit = fail (* we have no alias expansion for now *)
      (*
      if limit.expansions <= 0 then fail
      else
        match target with
        | Constr ({dt_path= _p; dt_aliases= {contents = Some (Some (params, ty))}}, ts2) ->
            let limit = { limit with expansions = limit.expansions - 1 } in
            if length params <> length ts2 then begin
              !!% "@[<2>ERROR: aliased type arity mismatch:@ %a@ (where alias = (%a).%a)@]@."
                Stype.format target
                Format.(list ", " Stype.format) params
                Stype.format ty;
              error := true;
              fail
            end else
              match_type pattern (Stype.subst params ts2 ty) limit 
              >>= fun (limit, d) -> 
              return (limit, Attr(`Ref pattern, d))
        | _ -> fail
      *)
      
    and match_arrow_types pattern target limit =
      let rec get_arrows = function
        | Otyp_arrow (_,t,t') -> let ars, r = get_arrows t' in t::ars, r
        | t -> [], t
      in
      let parrows, preturn = get_arrows pattern in
      let tarrows, treturn = get_arrows target in
      match parrows, tarrows with
      | [], [] -> fail (* avoid inf loop *)
      | _ ->
          match_type preturn treturn limit >>= fun (limit, dret) -> 
          match_types parrows tarrows limit >>= fun (limit, dts) -> 
          return (limit, `Arrow(dret,dts))
    
    and remove_target_type_option pattern target limit =
      match target with
      | Otyp_constr ( Oide_ident "option", [t2]) ->
          decr ~by:10 limit >>= (* CR jfuruse: must be configurable *)
          match_type pattern t2 >>= fun (limit, d) -> 
          return (limit, `Constr(`None, [d]))
      | _ -> fail
    
    and make_tuple_left pattern target limit =
      match pattern, target with
      | _, Otyp_tuple ts2 -> 
          match_types [pattern] ts2 limit >>= fun (limit, ds) -> 
          return (limit, `Tuple ds)
      | _ -> fail
    
    and make_tuple_right pattern target limit =
      match pattern, target with
      | Otyp_tuple ts1, _ ->
          begin match_types ts1 [target] limit >>= function
            | (limit, [d]) -> return (limit, d)
            | _ -> assert false
          end
      | _ -> fail
    
    and match_types pats targets limit =
      (* matching of two list of types, with permutation and addition/removal *)
    
      match pats, targets with
      | [], [] -> return (limit, [])
      | [pat], [target] ->
          match_type pat target limit >>= fun (limit, d) -> return (limit, [d])
      | _ ->
          let len_pats = length pats in
          let len_targets = length targets in
    
          let pats, targets, penalty =
            (* Some component might be missing in pattern, we fill variables for them
               but with a rather big price

               At 8a9d320d4 :
                   pattern: int -> int -> int -> int
                   target:  nat -> int -> int -> nat -> int -> int -> int
                   distance = 12 = 3 * (addition(3) + instance(1))

               I think 2 arguments addition is enough
                   2 * (addition(x) + 1) < 30
               how about 7? (2*(7+1)=16 3*(7+1)=24<30 4(7+1)=32>30 
            *)
            if len_pats < len_targets then
              map (fun _ -> dummy_pattern_type_var) (1 -- (len_targets - len_pats)) @ pats,
              map (fun target -> target, true) targets,
              (len_targets - len_pats) * 7
            else if len_pats > len_targets then
              (* The target can have less components than the pattern,
                 but with huge penalty

                 At 8a9d320d4 : x5
                 changed to     x10
              *)
              pats,
              map (fun _ -> dummy_type_expr_var, false) (1 -- (len_pats - len_targets)) 
              @ map (fun target -> target, true) targets,
              (len_pats - len_targets) * 10
            else pats, map (fun x -> x, true) targets, 0
          in
  
          decr ~by:penalty limit >>= fun limit -> 
    
          (* O(n^2) *)
          let targets_array = Array.of_list (map fst targets) in
          let score_table =
            (* I believe laziness does not help here *)
            map (fun pat ->
              Array.map (fun target ->
                match match_type pat target limit with
                | None -> None
                | Some (limit', x) -> Some (limit.score - limit'.score, x)
              ) targets_array) pats
          in
    
          (* I've got [GtkEnums._get_tables : unit -> t1 * .. * t70].
             Its permutation is ... huge: 1.19e+100.
           *)
    
          let rec perm_max target_pos xs limit = match xs with
            | [] -> return (limit, [])
            | xs ->
                (* [choose [] xs = [ (x, xs - x) | x <- xs ] *)
                let rec choose sx = function
                  | [] -> assert false
                  | [x] -> [x, rev sx]
                  | x::xs -> (x, rev_append sx xs) :: choose (x::sx) xs
                in
                let xss = choose [] xs in
  
                let matches =
                  filter_map (fun (x,xs) ->
                    match Array.unsafe_get x target_pos with
                    | None -> (* too much cost *) fail
                    | Some (score,d) -> do_;
                        (* CR jfuruse: bug: We ignore the changes of expansions here *)
                        score <-- PathLimit.decr ~by:score limit.score;
                        (score, ds) <-- perm_max (target_pos+1) xs { limit with score };
                        return (score,d::ds)) xss
                in
                match matches with
                | [] -> fail
                | _ -> return & fold_left1 (fun (s1, d1) (s2, d2) -> if s1 >= s2 then (s1, d1) else (s2, d2)) matches
            
          in do_;
    
          (limit, ds) <-- perm_max 0 score_table limit;
          return (limit, 
                  (combine ds targets 
                        |> filter_map (function
                             | (_, (_, false)) -> None
                             | (d, (_, true)) -> Some d)))
    in
    
    match_type pattern target limit 
          
(*  
  let dummy_type_expr_var = Var (-1)

  let hist_type = ref []

  let prof_type pat targ =
    if Conf.prof_match_types then
      try 
        let xs = assq pat !hist_type in
        try 
          let r = assq targ !xs in
          incr r
        with
        | Not_found ->
            xs := (targ, ref 1) :: !xs
      with
      | Not_found ->
          hist_type := (pat, ref [(targ, ref 1)]) :: !hist_type
    
  let report_prof_type () =
    if Conf.prof_match_types then
      let distinct, total = 
        fold_left (fun st (_, xs) ->
          fold_left (fun (d,t) (_, r) -> (d+1, t + !r)) st !xs) (0,0) !hist_type
      in
      !!% "distinct=%d total=%d@." distinct total
*)
  
  (* Return distance, not score *)
  let match_path_type (p1, ty1) (p2, ty2) limit_path limit_type =
    let open TypeLimit in do_;
    (limit_path', match_path) <-- match_path p1 p2 limit_path;
    (limit, match_xty) <-- match_type ty1 ty2 (create ( limit_type - limit_path +  limit_path'));
    return (limit_type - limit.score, (match_path, match_xty))
    
  
  (* Return distance, not score *)
  let match_type (* ?no_target_type_instantiate *) t1 t2 limit_type =
    let open TypeLimit in
    match_type (* ?no_target_type_instantiate *) t1 t2 (create limit_type) >>= fun (limit, desc) -> 
    return (limit_type - limit.score, desc)
  
  (* Return distance, not score *)
  let match_path p1 p2 limit =
    match_path p1 p2 limit >>= fun (score, desc) -> return (limit - score, desc)

end 

(*


module MakePooled( A: sig

  val cache : Levenshtein.StringWithHashtbl.cache
  val pooled_types : Stype.t array

end) = struct

  module M = Make(A)

  let error = M.error

  (* Return distance, not score *)
  let match_path = M.match_path

  module WithType(T : sig
    val pattern : Stype.t
    val cache : [ `NotFoundWith of int | `Exact of int * Stype.t ] array
  end) = struct

    (* Return distance, not score *)
    let match_type t2 limit_type =
      let t1 = T.pattern in
      match t2 with
      | Item.Not_pooled t2 -> M.match_type t1 t2 limit_type
      | Item.Pooled n ->
          let t2 = Array.unsafe_get A.pooled_types n in
          match Array.unsafe_get T.cache n with
          | `NotFoundWith n when n >= limit_type -> None
          | _ ->
              match M.match_type t1 t2 limit_type with
              | None -> 
                  Array.unsafe_set T.cache n (`NotFoundWith limit_type);
                  None
              | Some dtrace as res ->
                  Array.unsafe_set T.cache n (`Exact dtrace);
                  res
    
    (* Return distance, not score *)
    let match_path_type p1 (p2, ty2) limit_path limit_type = do_;
      (dist_path, desc_path) <-- match_path p1 p2 limit_path;
      (dist, desc_type) <-- match_type ty2 (limit_type - dist_path);
      return (dist + dist_path, (desc_path, desc_type))
  end
  
  let report_prof_type = M.report_prof_type
end 
*)
  
