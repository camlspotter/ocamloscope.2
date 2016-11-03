(** Sig: names and types defined in OCaml programs, extracted from *.cmi files *)

type k = KModule | KModtype | KType | KTypext | KValue | KClass | KClasstype | KConstructor | KField | KMethod
[@@deriving conv{ocaml_of}, typerep]
(** Kinds of the objects *)
    
val string_of_k : k -> string

val parse_k : string -> k option

type value_kind = SVal_prim | SVal_reg [@@deriving conv{ocaml_of}, typerep]
(** Regular value or primitive *)

(* Add ocaml_of_xxx and typerep things to some data types *)    
type rec_status    = [%import: Types.rec_status]
and  private_flag  = [%import: Asttypes.private_flag]
and  mutable_flag  = [%import: Asttypes.mutable_flag]
and  virtual_flag  = [%import: Asttypes.virtual_flag]
and  out_ident     = [%import: Outcometree.out_ident]
and  out_type      = [%import: Outcometree.out_type]
and  out_variant   = [%import: Outcometree.out_variant]
and  out_attribute = [%import: Outcometree.out_attribute]
  [@@deriving conv{ocaml_of}, typerep]

module Flatten(A : sig
  type type_expr [@@deriving conv{ocaml_of}]
  type path      [@@deriving conv{ocaml_of}]
  type ident     [@@deriving conv{ocaml_of}]
end) : sig
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

  val format : Format.formatter -> fsignature -> unit

  val the_type : res -> type_expr option
  (** Get the type part of [res] *)
end

type res =
    FModule of fmodule * rec_status
  | FModtype of fmodule option
  | FType of out_type list * ftypekind * private_flag * out_type option *
      rec_status
  | FTypextRaw of out_type list * fconstructor_arguments * out_type *
      out_type option * private_flag
  | FTypext of out_type list * out_type * out_type option * private_flag
  | FValue of out_type * value_kind
  | FRecordField of mutable_flag * out_type
  | FVariantConstructorRaw of fconstructor_arguments * out_type *
      out_type option
  | FVariantConstructor of out_type * out_type option
  | FClass of out_type list * fsignature * out_type * out_ident *
      (virtual_flag * rec_status)
  | FClassType of out_type list * fsignature * out_type * out_ident *
      (virtual_flag * rec_status)
  | FMethod of out_type
and ftypekind =
    FAbstract
  | FOpen
  | FRecord of fsignature
  | FVariant of fsignature
and fconstructor_arguments =
    FCRecord of (out_ident * mutable_flag * out_type) list
  | FCTuple of out_type list
and fsignature_item = (k * out_ident) * res
and fsignature = fsignature_item list
and fmodule =
    FFunctor of out_ident * fmodule option * fmodule
  | FSignature of fsignature
  | FUNKNOWN_ident of out_ident
  | FUNKNOWN_alias of out_ident
[@@deriving conv{ocaml_of}]
      
val format : Format.formatter -> fsignature -> unit

val the_type : res -> out_type option
(** Get the type part of [res] *)
