(** Generation of typerep methods for tag-checking *)

(* XXX simply moved to sig.ml ? *)

(* open Typerep_lib.Std *)

module Sig = struct
  open Sig
  type res                   = [%import: Sig.res]
  and ftypekind              = [%import: Sig.ftypekind]
  and fconstructor_arguments = [%import: Sig.fconstructor_arguments]
  and fsignature_item        = [%import: Sig.fsignature_item]
  and fsignature             = [%import: Sig.fsignature]
  and fmodule                = [%import: Sig.fmodule]
  (* [@@deriving typerep] *)
end

