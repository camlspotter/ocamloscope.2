open Utils

val dump_source_of_cmt_cmti : Format.formatter -> FilePath.t -> unit
(** retrieve the source code from cmt/cmti and print it *)

val source : FilePath.t option ref

val dump_it : unit -> unit
(** print the source of cmt/cmti specified by [source] *)

