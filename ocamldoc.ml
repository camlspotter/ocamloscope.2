open Spotlib.Spot
open List

let get_doc (atrs : Typedtree.attributes) : (string * Location.t) list =
  let open Location in
  let open Parsetree in
  let a = function
    | {txt="ocaml.doc"}, payload ->
        begin match payload with
        | PStr [{ pstr_loc= loc
                ; pstr_desc= Pstr_eval ({pexp_desc= Pexp_constant (Pconst_string (s, _))}, _)}] ->
            begin match s with
            | "/*" -> (*   (**/**)   *)
                None
            | _ -> Some (s, loc)
            end
        | _ -> assert false
        end
  | _ -> None
  in
  filter_map a atrs

let extract_structure str =
  let docs = ref [] in
  let module E = Attr.Make(struct
    let f attrs = docs := get_doc attrs @ !docs
  end)
  in
  E.iter_structure str;
  !docs
        
let extract_signature sg =
  let docs = ref [] in
  let module E = Attr.Make(struct
    let f attrs = docs := get_doc attrs @ !docs
  end)
  in
  E.iter_signature sg;
  !docs

module DocSet = Set.Make(struct
  type t = string * Location.t
  let compare = compare
end)

let partition_ok_and_ambiguous ds =
  let all, amb =
    fold_left (fun (all, amb) d ->
      if DocSet.mem d all then (all, DocSet.add d amb)
      else (DocSet.add d all, amb)) (DocSet.empty, DocSet.empty) ds
  in
  (DocSet.diff all amb, amb)

let warn_ambiguous ds =
  flip DocSet.iter ds & fun (_s,l) ->
    !!% "Warning: %a: ambiguous OCamlDoc comment was ignored@."
      Location.print_compact l
 
module Re = Ppx_orakuda.Regexp.Re_pcre
open Re.Literal

let normalize s =
  let ss =
    s |> {s|(^\s+|\s+$)//g|s} (* Remove start and end spaces *)
      |> [%s "\r?\n\\s*/ /g" ] (* unindent *) (* XXX Bug of ppx_orakuda: \r and \n are not accepted in {s|..|s}!! *)
      |> Re.split {m|\.\s+|m} (* split into sentences *)
  in
  let rec get len = function
    | _ when len <= 0 -> [ "..." ]
    | [] -> []
    | s::ss -> s :: get (len - String.length s) ss
  in
  let s = String.concat ".  " & get 256 ss in
  if String.length s <= 512 then s
  else String.sub s 0 509 ^ "..."

        


  
