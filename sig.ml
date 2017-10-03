open Spotlib.Spot
open Ocaml_conv.Default
(* open Typerep_lib.Std *)

type k = KModule | KModtype | KType | KTypext | KValue | KClass | KClasstype | KConstructor | KField | KMethod
[@@deriving conv{ocaml_of}(*, typerep*)]

let string_of_k = function
  | KModule      -> "module"
  | KModtype     -> "module type"
  | KType        -> "type"
  | KTypext      -> "typext"
  | KValue       -> "val"
  | KField       -> "field"
  | KConstructor -> "constr"
  | KClass       -> "class"
  | KClasstype   -> "class type"
  | KMethod      -> "method"

let parse_k s = match String.split (function ' ' | '\t' -> true | _ -> false) s with
  | [ "val" ]            -> Some KValue
  | [ "type" ]           -> Some KType
  | [ "typext" ]         -> Some KTypext
  | [ "exception" ]      -> Some KTypext (* CR jfuruse: needs a special one? *)
  | [ "module" ]         -> Some KModule
  | [ "module"; "type" ] -> Some KModtype
  | [ "class" ]          -> Some KClass
  | [ "class"; "type" ]  -> Some KClasstype
  | [ "field" ]          -> Some KField
  | [ "constructor" ]    -> Some KConstructor
  | [ "method" ]         -> Some KMethod
  | _ -> None

type value_kind = SVal_prim | SVal_reg [@@deriving conv{ocaml_of}(*, typerep*)]
  
type rec_status   = [%import: Types.rec_status]
and  private_flag = [%import: Asttypes.private_flag]
and  mutable_flag = [%import: Asttypes.mutable_flag]
and  virtual_flag = [%import: Asttypes.virtual_flag]
[@@deriving conv{ocaml_of}(*, typerep*)]

type out_ident     = [%import: Outcometree.out_ident]
and  out_type      = [%import: Outcometree.out_type]
and  out_variant   = [%import: Outcometree.out_variant]
and  out_attribute = [%import: Outcometree.out_attribute]
[@@deriving conv{ocaml_of}(*, typerep*)]

module Flatten(A : sig
  type type_expr [@@deriving conv{ocaml_of}]
  type path      [@@deriving conv{ocaml_of}]
  type ident     [@@deriving conv{ocaml_of}]
end) = struct
    open A
      
  type res = 
    | FModule of fmodule * rec_status
    | FModtype of fmodule option
    | FType of type_expr list (* TODO variance *)
             * ftypekind
             * private_flag
             * type_expr option
             * rec_status
    | FTypextRaw of type_expr list
               * fconstructor_arguments
               * type_expr
               * type_expr option (* if gadt *)
               * private_flag (* This must be converted to FTypext in Globalized *)
    | FTypext of type_expr list
               * type_expr (* constructor type *)
               * type_expr option (* return type if gadt *)
               * private_flag
    | FValue of type_expr * value_kind
    | FRecordField of mutable_flag * type_expr
    | FVariantConstructorRaw of fconstructor_arguments * type_expr (* -> return_type *) * type_expr option (* return_type if it is a gadt *) (* This must be converted to FVariantConstructor in Globalized *)
    | FVariantConstructor of type_expr * type_expr option (* return type if gadt *) 
    | FClass of type_expr list (* params TODO variance *)
                * fsignature (* only FMethod *)
                * type_expr (* new type *)
                * path (* class path? *)
                * (virtual_flag (* * type_expr option (* cty_new *) *)
                   * rec_status)
    | FClassType of type_expr list (* params TODO variance *)
                 * fsignature
                 * type_expr (* new type *)
                 * path (* class path? *)
                 * (virtual_flag * rec_status)
    | FMethod of type_expr
  
  and ftypekind =
    | FAbstract
    | FOpen
    | FRecord of fsignature (* only contains FRecordField *)
    | FVariant of fsignature (* only contains FVairantConstructor *)
  
  and fconstructor_arguments =
    | FCRecord of (ident * mutable_flag * type_expr) list
    | FCTuple of type_expr list
  
  and fsignature_item = ((k * path) * res)
  
  and fsignature = fsignature_item list
  
  and fmodule =
    | FFunctor of ident * fmodule option * fmodule
    | FSignature of fsignature
    | FUNKNOWN_ident of path
    | FUNKNOWN_alias of path
  [@@deriving conv{ocaml_of}]

  let format = Ocaml.format_with ocaml_of_fsignature

  let the_type = function
    | FTypextRaw _ | FVariantConstructorRaw _ -> assert false
    | FModule _
    | FModtype _ -> None
    | FType (_, _, _, Some ty, _) -> Some ty
    | FType _     -> None
    | FTypext (_, t, _, _)
    | FValue (t, _)
    | FRecordField (_, t)
    | FVariantConstructor (t, _)
    | FClass (_, _, t, _, _)
    | FClassType (_, _, t, _, _)
    | FMethod t    -> Some t
end

include Flatten(struct
  let rec string_of_out_ident = function
    | Oide_ident s -> s
    | Oide_dot (id, s) -> String.concat "." [string_of_out_ident id; s]
    | Oide_apply (id1, id2) ->
        String.concat ""
          [string_of_out_ident id1; "("; string_of_out_ident id2; ")"]

  type type_expr = out_type
  let ocaml_of_type_expr oty = Ocaml.String (Format.sprintf "%a" !Xoprint.out_type oty)

  type path = out_ident
  let ocaml_of_path p = Ocaml.String (string_of_out_ident p)

  type ident = out_ident
  let ocaml_of_ident p = Ocaml.String (string_of_out_ident p)
end)
