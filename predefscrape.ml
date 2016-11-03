open Spotlib.Spot
open Types

let sig_of_predefs =
  let rec scan = function
    | Env.Env_empty -> []
    | Env_type (s, id, td) -> Sig_type (id, td, Trec_first) :: scan s
    | Env_value (s, id, vd) -> Sig_value (id, vd) :: scan s
    | Env_extension (s, id, ec) -> Sig_typext (id, ec, Text_exception) :: scan s
    | Env_module (_ , _, _)
    | Env_modtype (_ , _, _)
    | Env_class (_ , _, _)
    | Env_cltype (_ , _, _)
    | Env_open (_ , _)
    | Env_functor_arg (_ , _) -> assert false
  in
  scan & Env.summary & Env.initial_unsafe_string

let sig_ () = Sigext.scrape None sig_of_predefs
let hump () = Humpext.types_signature Env.initial_unsafe_string sig_of_predefs

