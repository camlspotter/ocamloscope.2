open Spotlib.Spot
open List

module Re = Ppx_orakuda.Regexp.Re_pcre
open Re.Infix
open Re.Literal

let parse s = case s
  |> ( {m|^\s*#(.*)$|m} ==> fun r ->
         let lexbuf = Lexing.from_string r#_1 in
         Some (Exn.catch Parse.expression lexbuf)
     )
  |> default (fun () -> None)

let interpret db e pspec = match e with
  | [%expr packages] ->
      let open Data.DB in
      flip iter db.packs (fun (n,v) ->
        match v with
        | None -> !!% "%s@." n
        | Some v -> !!% "%s.%s@." n v);
          pspec
  | [%expr all]     -> Query.PackageSpec.All_but []
  | [%expr vanilla] -> Query.PackageSpec.Vanilla []
  | [%expr none]    -> Query.PackageSpec.Just []
  | [%expr quit] ->
      !!% "Bye@.";
      exit 0
  | _ ->
      !!% "illegal directive@.";
      pspec
      
