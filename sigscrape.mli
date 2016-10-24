open Utils

val test_cmi : FilePath.t -> unit
(** Scrape the given cmi file and print the result to stderr *)

val scrape_ocamlfind_package
  : FilePath.t (*+ destdir *)
  -> Opamfind.Ocamlfind.Analyzed_group.t
  -> Data.SigFile.t
(** Scrape the given OCamlFind package. The result is saved as
    <destdir>/<packagename>.sig
*)
