val get_doc : Typedtree.attributes -> (string * Location.t) list
(** Extract Docstrings from attributes *)
  
val extract_structure : Typedtree.structure -> (string * Location.t) list
(** Extract Docstrings from structure *)

val extract_signature : Typedtree.signature -> (string * Location.t) list
(** Extract Docstrings from signature *)

module DocSet : Set.S with type elt = (string * Location.t)
(** Set of Docstrings *)
  
val partition_ok_and_ambiguous : (string * Location.t) list -> DocSet.t * DocSet.t
(** [partition_ok_and_ambiguous docs] takes a list of Docstrings obtained from
    [extra_structure] and [extra_signature] and filter out ambiguous Docstrings
    which appear in [docs] more than once.  The first component of the returned
    tuple is the set of Docstrings without ambiguities.  The second one is 
    the set of ambiguous Docstrings which appear in more than one AST nodes.
*)

val warn_ambiguous : DocSet.t -> unit
(** print warnings of ignoring ambiguous Docstrings *)
  
val normalize : string -> string
(** Normalize Docstring. Long Docstrings are trimed up to between 256 and 512 chars *)
  
