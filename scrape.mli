open Utils

val packages : FilePath.t -> string list -> unit
(** [packages data_dir packs] scrape the given OCamlFind package groups
    [packs] then save the result to [data_dir ^/ <package>.dat] files
*)
