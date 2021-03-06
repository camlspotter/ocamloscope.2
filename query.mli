(** Query *)

open Sig
open Data

type q = 
  | Path      of out_ident            (** By path *)
  | Path_type of out_ident * out_type (** By path and type *)
  | Type      of out_type             (** By type *)
[@@deriving conv{ocaml_of}]

type t = k option * q
[@@deriving conv{ocaml_of}]
(** Type of queries *)

val parse : string -> t list * string list
(** Tries to parse a string as a query. 
    One string may be regarded as more than one form of queries.
    The second member of the tuple is the warning messages.
*)

module PackageSpec : sig  
  type t =
    | Just of string list    (** Only these packages *)
    | All_but of string list (** All the packages linked except the specified *)
    | Vanilla of string list (** [vanilla] plus the specified *)
  [@@deriving conv{ocaml_of}]
  
  val vanilla : string list
  (** Vanilla packages: i.e. OCamlFind packages installed along with OCaml
      compiler, except compilerlibs and ocamldoc
  *)
end

(** BFG 9000 *)
val query 
  : DB.t
  -> PackageSpec.t
  -> t list 
  -> (int (*+ distance *)
      * ( DB.item
          * Sigmatch.PathLimit.desc option 
          * ([> `Arrow of 'd * 'd list
             | `Constr of
                 [> `None
                 | `Path of Sigmatch.PathLimit.desc ] *
                   'd list
             | `None
             | `Tuple of 'd list
             | `Var of out_type ]
                as 'd) option ) list
        ) list

