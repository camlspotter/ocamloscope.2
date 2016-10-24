(** Search result grouping *)

open Spotlib.Spot
open Data

val group
  : (int * (Data.DB.item * 'trace1 * 'trace2) list) list
  -> ( (Sig.k * Data.alias)
       * int
       * (int * Data.DB.item * 'trace1 * 'trace2) list list
     ) list

val group_and_print
  : Format.t
  -> (int * (DB.item * 'trace1 * 'trace2) list) list
  -> unit
(** Print out search results *)

