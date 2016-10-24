open Spotlib.Spot
open Opamfind.Utils
open List
open Opamfind

let make_from_names names =
  let names = unique & sort compare names in
  let best_name = Ocamlfind.choose_best_package_name names in
  "{"
  ^ String.concat "," (best_name :: filter (fun x -> x <> best_name) names)
  ^ "}"

let make aps =
  let names = map Ocamlfind.Analyzed.name aps in
  make_from_names names

let parse = function
  | "" -> None
  | s when String.(unsafe_get s 0 = '{' && unsafe_get s (length s - 1) = '}') ->
      Some (String.(split (function ',' -> true | _ -> false)
              & sub s 1 (length s - 2)))
  | _ -> None
