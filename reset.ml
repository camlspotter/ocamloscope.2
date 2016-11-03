(** Reset OCaml internal caches *)
let typing () =
  Env.reset_cache (); (* also need to call reset_cache_toplevel? *)
  Envaux.reset_cache ()
