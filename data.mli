(** Data file load/save *)

open Utils
open Sig

module SigFile : sig
  (** Image of <module>.sig file *)
  type t =
      { name  : string
      ; packs : (out_ident * fsignature) list
      ; stamp : Digest.t list (** the digests of the package *.cm* files *) 
      }
  [@@deriving conv{ocaml_of}, typerep]

  val sizes : t -> int (*+ packages *) * int (*+ sigitems *)

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

module HumpFile : sig
  (** Image of <module>.hump file *)
  type t =
      { name  : string
      ; humps : (out_ident * Hump.expr) list
      ; stamp : Digest.t list (** the digests of the package *.cm* files *) 
      }
  [@@deriving conv{ocaml_of}, typerep]

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

type alias = Path of out_ident | Primitive of string
  [@@deriving conv{ocaml_of}, typerep]

module Dat : sig
  (** Image of .dat file *)

  type humped_fsignature_item =
    (k * out_ident) * (res * alias option * Hump.v option)

  and t =
      { name    : string
      ; version : string option (** the one of the main OCamlFind package *)
      ; items   : humped_fsignature_item list
      ; stamp   : Digest.t list (** the digests of the package *.cm* files *)
      }
  [@@deriving conv{ocaml_of}, typerep]

  val size : t -> int (*+ sigitems *)

  val save : FilePath.t -> t -> unit

  val load : FilePath.t -> t
  (** Safe with tag-checking.  If check fails, the funciton fails. *)
end

module DB : sig
  (** type of the db *)
  type item  =
      { kind     : k
      ; top_pack : string 
      ; path     : out_ident
      ; res      : res (* CR jfuruse: should be called desc *)
      ; tyid     : int option (** position in top_types *)
      ; alias    : alias option
      ; v        : Hump.v option
      }

  and t =
      { items     : item list
      ; top_types : out_type array
      ; packs     : (string * string option) list
      }
  [@@deriving conv{ocaml_of}, typerep]

  val fsignature_item : item -> fsignature_item
  (** Rebuild [fsignature_item] for printing *)

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

