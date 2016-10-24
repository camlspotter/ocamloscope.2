(** The heart of OCamlOScope, the match algorithm *)

open Outcometree

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
     * (([> `Apply of 'a * [> `No ]
         | `Dot of 'a * string
         | `Ident of string
         | `No ] as 'a)
        * ([> `Arrow of 'b * 'b list
           | `Constr of [> `None
                        | `Path of
                            [> `Apply of 'c * [> `No ]
                            | `Dot of 'c * string
                            | `Ident of string
                            | `No ]
                              as 'c ] * 'b list
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
            | `Path of
                [> `Apply of 'b * [> `No ]
                | `Dot of 'b * string
                | `Ident of string
                | `No ]
                  as 'b ] * 'a list
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
     * ([> `Apply of 'a * [> `No ]
        | `Dot of 'a * string
        | `Ident of string
        | `No ]
           as 'a)
    ) option
end
