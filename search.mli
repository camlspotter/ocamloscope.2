(** Search session loop *)

open Utils

val do_search : FilePath.t -> unit
(** Load the database file (normally "all.all") then start a search session *)

val dump : FilePath.t -> unit
(** Dump the database file. Normally useless since the output is too huge *)

