(** Search result grouping *)

open Spotlib.Spot
open Data

val group
  : (int * (Data.DB.item * 'trace1 * 'trace2) list) list
  -> ( Data.alias
       * int
       * (int * Data.DB.item * 'trace1 * 'trace2) list list
     ) list
(** Group the search results *)

val group_and_print
  : bool (*+ show alias info or not *)
  -> Format.t
  -> (int * (DB.item * 'trace1 * 'trace2) list) list
  -> unit
(** Print out search results *)

