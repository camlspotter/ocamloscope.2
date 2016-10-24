open Spotlib.Spot
open List

let predefined_type_names =
  let f e =
    let rec scan = function
      | Env.Env_empty -> []
      | Env_type (s, id, _) -> id :: scan s
      | Env_value (s, _, _)
      | Env_extension (s, _, _)
      | Env_module (s , _, _)
      | Env_modtype (s , _, _)
      | Env_class (s , _, _)
      | Env_cltype (s , _, _)
      | Env_open (s , _)
      | Env_functor_arg (s , _) -> scan s
    in
    scan & Env.summary e
  in
  map (fun i -> i.Ident.name) & unique & f Env.initial_safe_string @ f Env.initial_unsafe_string

(*
let () = !!% "Predefined type names: %a@." Format.(list "@ " string) predefined_type_names
*)

  
