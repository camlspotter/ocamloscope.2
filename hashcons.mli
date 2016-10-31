(** It is crucial for OCamlOScope to maximize data sharings.
    This module provides some hashcons'ing.
*)

open Outcometree

val string     : string            -> string
val out_ident  : out_ident         -> out_ident
val out_type   : out_type          -> out_type
val fsignature : Sig.fsignature    -> Sig.fsignature
val sigfile    : Data.SigFile.t    -> Data.SigFile.t
val humpfile   : Data.HumpFile.t   -> Data.HumpFile.t
val dat_t      : Data.Dat.t        -> Data.Dat.t
val location_t : Location.t        -> Location.t
val hump_expr  : Hump.expr         -> Hump.expr
val hump_v     : Hump.v            -> Hump.v
  
val reset : unit -> unit
(** Reset the hashcons cache. *)
