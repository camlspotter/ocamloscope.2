open Utils

val test_cmt : FilePath.t -> Hump.expr

val scrape_ocamlfind_package
  : FilePath.t (*+ destdir *)
  -> bool (*+ dump *)
  -> Opamfind.Ocamlfind.Analyzed_group.t
  -> Data.HumpFile.t
(** Scrape Hump data of the modules of the given OCamlFind package group,
    then save it to <destdir>/<packname>.hump
*)
