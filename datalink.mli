open Utils
open Data

val link_db : FilePath.t (*+ data dir *) -> DB.t
(** [link_db data_dir] checks dat files stored under [data_dir] and update
    [data_dir ^/ "all.all"] file if necessary.  It returns the whole DB ready
    for search.
*)
