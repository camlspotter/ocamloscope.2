(** Data file load/save *)

open Utils
open Sig

module SigFile : sig
  (** Image of <pack>.sig file.

      <pack>.sig file contains signature data of OCamlFind package <pack> and 
      its sub-packages such as <pack.xxx>
  *)
  type t =
      { name  : string                        (** <pack> *)
      ; packs : (out_ident * fsignature) list (** packages and their signatures *)
      ; stamp : Digest.t list                 (** the digests of the package *.cm* files *) 
      }
  [@@deriving conv{ocaml_of}, typerep]

  val sizes : t -> int (*+ packages *) * int (*+ sigitems *)

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

module HumpFile : sig
  (** Image of <pack>.hump file 

      <pack>.hump file contains code abstractions of OCamlFind package <pack> 
      and its sub-packages such as <pack.xxx>
  *)
  type t =
      { name  : string                       (** <pack> *)
      ; humps : (out_ident * Hump.expr) list (** packages and their humps *)
      ; stamp : Digest.t list                (** the digests of the package *.cm* files *) 
      }
  [@@deriving conv{ocaml_of}, typerep]

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

(** Alias information *)
type alias =
  | Path of out_ident (** Alias of some path *)
  | Primitive of string (** A primitive %xxx *)
  [@@deriving conv{ocaml_of}, typerep]

module Dat : sig
  (** Image of .dat file 

      <pack>.dat is the linked data of <pack>.sig and <pack>.hump.
      Code abstracitons of <pack>.hump and its depending hump files
      are evaluated in <pack>.dat

      <pack>.dat file is portable: if OCamlOScope server lacks the information
      of some package, someone can create a <pack>.dat in his environment
      then send it to the server administrator to register it.
  *)

  type humped_fsignature_item =
    (k * out_ident) * (res * alias option * Hump.v option)

  and t =
      { name    : string                      (** <pack> *)
      ; version : string option               (** the one of the main OCamlFind package *)
      ; items   : humped_fsignature_item list (** all the items *)
      ; stamp   : Digest.t list               (** the digests of the package *.cm* files *)
      }
  [@@deriving conv{ocaml_of}, typerep]

  val size : t -> int (*+ sigitems *)

  val save : FilePath.t -> t -> unit

  val load : FilePath.t -> t
  (** Safe with tag-checking.  If check fails, the funciton fails. *)
end

module DB : sig
  (** Type of the database.

      OCamlOScope stores all the data in memory.  The data is also saved 
      to a file "all.all".
  *)

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
      ; top_types : out_type array                (** the unique types of items, for faster search *)
      ; packs     : (string * string option) list (** linked packages *)
      }
  [@@deriving conv{ocaml_of}, typerep]

  val fsignature_item : item -> fsignature_item
  (** Rebuild [fsignature_item] for printing *)

  val save : FilePath.t -> t -> unit

  val unsafe_load : FilePath.t -> t
end

