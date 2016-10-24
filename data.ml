open Spotlib.Spot
open List
open Utils
open Sig
open Sigtyperep.Sig
open Typerep_lib.Std

let name_of_data_file path = Filename.(chop_extension & basename path)
    
let check_file_name ~pack ~path = pack = name_of_data_file path

module SigFile = struct
  type t = { name  : string
           ; packs : (out_ident * fsignature) list
           ; stamp : Digest.t list
           }
  [@@deriving conv{ocaml_of}, typerep]

  let sizes t =
    length t.packs, fold_left (+) 0 & flip map t.packs & fun (_, fsig) -> length fsig

  let save p ({name=n} as sig_) =
    if not & check_file_name ~pack:n ~path:p then
      failwithf "save: %s has a conflicting name %s" p n;
    with_oc (open_out_bin p) & fun oc -> Marshal.to_channel oc sig_ []

  let unsafe_load p : t =
    with_ic (open_in_bin p) & fun ic -> Marshal.from_channel ic
end
  
module HumpFile = struct
  type t = { name : string
           ; humps : (out_ident * Hump.expr) list
           ; stamp : Digest.t list
           }
  [@@deriving conv{ocaml_of}, typerep]

  let save p ({name=n} as hump) =
    if not & check_file_name ~pack:n ~path:p then
      failwithf "save: %s has a conflicting name %s" p n;
    with_oc (open_out_bin p) & fun oc -> Marshal.to_channel oc hump []

  let unsafe_load p : t =
    with_ic (open_in_bin p) & fun ic -> Marshal.from_channel ic
end
  
type alias = Path of out_ident | Primitive of string
  [@@deriving conv{ocaml_of}, typerep]

module Dat = struct
  type humped_fsignature_item =
    (k * out_ident) * (res * alias option * Hump.v option)

  and t = { name : string
          ; version : string option (** the one of the main OCamlFind package *)
          ; items : humped_fsignature_item list
          ; stamp : Digest.t list
          }
  [@@deriving conv{ocaml_of}, typerep]

  let size t = length t.items

  (* Hashconsing must be done by the caller *)
  let save p ({name=n} as dat) =
    if not & check_file_name ~pack:n ~path:p then
      failwithf "Dat.save: %s has a conflicting name %s" p n;
    with_oc (open_out_bin p) & fun oc -> Marshal.to_channel oc dat []

  let load p : t =
    !!% "Loading %s...@." p;
    let o : Obj.t = with_ic (open_in_bin p) & fun ic -> Marshal.from_channel ic in
    !!% "Load done.@.";
    !!% "Typing...@.";
    let dat = Unmagic.obj ~sharing:false typerep_of_t o in
    !!% "Typing done.@.";
    let sz = size dat in
    !!% "Got %d sigitems@." sz;
    if not & check_file_name ~pack:dat.name ~path:p then
      failwithf "load: %s has a conflicting name %s" p dat.name;
    dat
end
  
module DB = struct
  open Sigtyperep.Sig

  type item  = { kind : k
               ; top_pack : string 
               ; path : out_ident
               ; res  : res (* CR jfuruse: should be called desc *)
               ; tyid : int option
               ; alias : alias option
               ; v : Hump.v option
               }

  and t = { items : item list
          ; top_types : out_type array
          ; packs : (string * string option) list
          }
  [@@deriving conv{ocaml_of}, typerep]
  (** type of db *)

  let fsignature_item i = (i.kind, i.path), i.res

  let save p (db : t) =
    with_oc (open_out_bin p) (fun oc -> Marshal.to_channel oc db [])

  let unsafe_load p : t =
    with_ic (open_in_bin p) & fun ic -> Marshal.from_channel ic
end
