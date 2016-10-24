(** Query *)

open Sig
open Data

type q = 
  | Path      of out_ident
  | Path_type of out_ident * out_type
  | Type      of out_type
[@@deriving conv{ocaml_of}]

type t = k option * q
[@@deriving conv{ocaml_of}]
(** Type of queries *)

val parse : string -> t list
(** Tries to parse a string as a query. 
    One string may be regarded as more than one form of queries.
*)

module PackageSpec : sig  
  type t =
    | Just of string list
    | All_but of string list
    | Vanilla of string list
  [@@deriving conv{ocaml_of}]
  
  val vanilla : string list
  (** Vanilla packages: i.e. OCamlFind packages installed along with OCaml
      compiler, except compilerlibs and ocamldoc
  *)
end

val query 
  : DB.t
  -> PackageSpec.t
  -> t list 
  -> (int (*+ distance *)
      * ( DB.item
          * ([> `Apply of 'c * [> `No ]
             | `Dot of 'c * string
             | `Ident of string
             | `No ] as 'c) option 
          * ([> `Arrow of 'd * 'd list
             | `Constr of
                 [> `None
                 | `Path of
                     [> `Apply of 'e * [> `No ]
                     | `Dot of 'e * string
                     | `Ident of string
                     | `No ]
                       as 'e ] *
                   'd list
             | `None
             | `Tuple of 'd list
             | `Var of out_type ]
                as 'd) option ) list
        ) list

