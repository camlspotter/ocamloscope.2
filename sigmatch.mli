(** The heart of OCamlOScope, the match algorithm *)

open Outcometree

module PathLimit : sig

  type desc =
    | Apply of desc
    | Dot of desc * string
    | Ident of string
    | IdentMod of string
    | ModIdent of string
    | No

end
  
module Make(A : sig 
  val cache : Levenshtein.StringWithHashtbl.cache 
end) : sig
  val match_name_levenshtein : string -> string -> int -> int option

  val match_name_substring   : string -> string -> int -> int option

  val match_name             : string -> string -> int -> int option
  (** Currently it is equal to [match_name_levenshtein] *)
    
  val match_package : string -> string list -> int -> int option

  val match_path_type :
    out_ident * out_type ->
    out_ident * out_type ->
    int ->
    int ->
    (int 
     * (PathLimit.desc
        * ([> `Arrow of 'b * 'b list
           | `Constr of [> `None
                        | `Path of PathLimit.desc ] * 'b list
           | `None
           | `Tuple of 'b list
           | `Var of out_type ]
              as 'b)
       )
    ) option
      
  val match_type :
    out_type ->
    out_type ->
    int ->
    (int 
     * ([> `Arrow of 'a * 'a list
        | `Constr of
            [> `None
            | `Path of PathLimit.desc ] * 'a list
        | `None
        | `Tuple of 'a list
        | `Var of out_type ]
           as 'a)
    ) option

  val match_path :
    out_ident ->
    out_ident ->
    int ->
    (int 
     * PathLimit.desc
    ) option
end
