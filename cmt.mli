(** *.cmt/cmti files and load path *)

open Utils

val get_load_path : Cmt_format.cmt_infos -> FilePath.t list
(** Return the load path for the compilation recorded in a *.cmt/cmti file.

    For packed modules, it returns the load path necessary for its sub-modules.
 *)

val set_load_path : Cmt_format.cmt_infos -> unit
(** Set the current load path to the same one recorded in the *.cmt/cmti file *)
