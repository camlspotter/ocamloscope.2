(* This module implements dynamic search distance threshold tweak.

   The search distance must be relatively small integer, 
   since we sort the result using Hashtbl.
*)

open Spotlib.Spot
open List

type 'a t = {
  total                : int; (** current items in t *)
  total_below_farthest : int; (** current items in t, excluding atthe  farthest level *)
  num_limit            : int; (** items at the farthest level are thrown away if the total below_farthest exceeds limit *)
  farthest             : int; (** the farthest distance in tbl *)
  tbl                  : (int, 'a list) Hashtbl.t; (** the table *)
  threshold            : int;
}

let invariant t =
  let list = Hashtbl.to_list t.tbl in
  let farthest = fold_left (fun acc (d,_) -> max acc d) 0 list in
  let total = fold_left (fun acc (_,l) -> acc + length l) 0 list in
  let total_below_farthest = total - length (try Hashtbl.find t.tbl farthest with Not_found -> []) in
  assert (t.farthest             = farthest);
  assert (t.total                = total);
  assert (t.total_below_farthest = total_below_farthest);
  assert (t.threshold            > farthest)

let create ~thresh ~limit:num_limit =
  { total                = 0;
    total_below_farthest = 0;
    num_limit            = num_limit;
    farthest             = 0;
    tbl                  = Hashtbl.create 107;
    threshold            = thresh
  }

let next_farthest t =
  let rec search = function
    | -1 -> None
    | i ->
        match Hashtbl.find_opt t.tbl i with
        | None -> search (i-1)
        | Some _ -> Some i
  in
  search (t.farthest-1)


let throw_away_farthest t =
  match next_farthest t with
  | None -> t (* We cannot throw away the best scored items *)
  | Some next_farthest ->
      let farthest = t.farthest in
      let len_farthest = length (Hashtbl.find t.tbl t.farthest) in
      let len_next_farthest = length (Hashtbl.find t.tbl next_farthest) in
      Hashtbl.remove t.tbl t.farthest;
      { t with
        total                = t.total - len_farthest;
        total_below_farthest = t.total - len_farthest - len_next_farthest;
        farthest             = next_farthest;
        threshold            = farthest;
      } (* |- fun t -> !!% "!!!! total= %d thresh=%d@." t.total t.threshold *)

let need_throw_away t = t.farthest > 0 && t.total_below_farthest > t.num_limit

let may_throw_away_farthest t =
  if need_throw_away t then throw_away_farthest t else t

let add t dist a =
  if dist >= t.threshold then t (* too far away. do not add *)
  else begin
    let lst = Option.default (Hashtbl.find_opt t.tbl dist) (fun _ -> []) in
    Hashtbl.replace t.tbl dist (a :: lst);
    let t = 
      match compare dist t.farthest with
      | -1 -> 
          { t with 
            total                = t.total + 1;
            total_below_farthest = t.total_below_farthest + 1;
          }
      | 0 ->
          { t with
            total = t.total + 1;
          }
      | 1 ->
          { t with
            total                = t.total + 1;
            total_below_farthest = t.total;
            farthest             = dist;
          }
      | _ -> assert false
    in
    may_throw_away_farthest t
  end

let with_invariant t = invariant t; t

let create ~thresh ~limit = with_invariant & create ~thresh ~limit
let add t dist a = with_invariant & add t dist a
let to_list t = sort (compare_on fst) & Hashtbl.to_list t.tbl
 
let thresh t = t.threshold
  
