open Spotlib.Spot

open Asttypes
open Types
open Typedtree
open Format

open List

open Utils

open Humpdoc

module Ty = struct
  let rec module_type = function
    | Mty_signature sg -> Structure (signature sg)
    | Mty_functor (id, Some amty, mty) ->
        let amty = module_type amty in
        let mty = module_type mty in
        Functor(id, Some amty, mty)
    | Mty_functor (id, None, mty) ->
        let mty = module_type mty in
        Functor(id, None, mty)
    | Mty_ident _p -> assert false
    | Mty_alias p ->
        !!% "Alias %s@." (Path.string_of p);
        Var p
  
  and signature sg = map signature_item sg

  and signature_item = function
    | Sig_value (id, _vdesc) -> (KValue, id), Value LocNone, []
    | Sig_type (id, td, _) -> (KType, id), Type (LocNone, type_declaration td), []
    | Sig_typext (id, _ec, _) -> (KTypext, id), Typext LocNone (* really? *), []
    | Sig_module (id, md, _) -> (KModule, id), Module (LocNone, module_declaration md), []
    | Sig_modtype (_id, _mtd) -> assert false
    | Sig_class _ (* of Ident.t * class_declaration * rec_status *)
    | Sig_class_type _ (* of Ident.t * class_type_declaration * rec_status *)
        -> assert false
  
  and type_declaration td = type_kind td.type_kind
  (*
    { type_params: type_expr list;
      type_arity: int;
      type_kind: type_kind;
      type_private: private_flag;
      type_manifest: type_expr option;
      type_variance: Variance.t list;
      (* covariant, contravariant, weakly contravariant, injective *)
      type_newtype_level: (int * int) option;
      (* definition level * expansion level *)
      type_loc: Location.t;
      type_attributes: Parsetree.attributes;
      type_immediate: bool; (* true iff type should not be a pointer *)
    }
  *)
      
  and type_kind = function
    | Type_abstract -> Abstract
    | Type_open -> Open
    | Type_record (lds, _) -> Record (map label_declaration lds)
    | Type_variant cds -> Variant (map constructor_declaration cds)

  and label_declaration ld = 
    ld.ld_id, Def ld.ld_loc, []
  (*
    {
      ld_id: Ident.t;
      ld_mutable: mutable_flag;
      ld_type: type_expr;
      ld_loc: Location.t;
      ld_attributes: Parsetree.attributes;
    }
  *)
  
  and constructor_declaration cd = 
    cd.cd_id, Def cd.cd_loc, []
  (*
    {
      cd_id: Ident.t;
      cd_args: constructor_arguments;
      cd_res: type_expr option;
      cd_loc: Location.t;
      cd_attributes: Parsetree.attributes;
    }
  *)

  and module_declaration md = module_type md.md_type
    
(*  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
  }
*)

end

(*
module M = Monad.Make(struct
  type env = ((k * Longident.t) * Ident.t) list
  type 'a t = env -> 'a * env
  let return : 'a -> 'a t = fun x env -> (x,env)
  let bind : 'a t -> ('a -> 'b t) -> 'b t = fun at f env ->
    let a, env = at env in
    f a env
end)

open M
*)

let get_doc atrs = 
  let open Parsetree in
  flip filter_map atrs & function
    | ({txt="ocaml.doc"}, 
       PStr [ { pstr_desc= Pstr_eval ({ pexp_desc= Pexp_constant( Pconst_string(s, _) ) }, _) } ]) -> 
        Some s  
    | ({txt="ocaml.doc"}, _) -> assert false 
    | _ -> None

let rec structure s = concat_map structure_item s.str_items

(*
{
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}
*)

and structure_item si = match si.str_desc with
  | Tstr_eval _ -> []
  | Tstr_open _ -> []
  | Tstr_attribute _ -> []
  | Tstr_value (_, vbs) -> value_binding_list vbs
  | Tstr_type (_, tds) -> map type_declaration tds
  | Tstr_typext te -> type_extension te
  | Tstr_module mb -> [module_binding mb]
  | Tstr_include id -> include_declaration si.str_loc id
  | _ -> assert false
(*
  | Tstr_primitive vd -> [primitive vd]
  | Tstr_exception ec -> [extension_constructor None ec]
  | Tstr_recmodule mbs -> map module_binding mbs
  | Tstr_modtype mtd -> [module_type_declaration mtd]
  | Tstr_class (* of (class_declaration * string list) list *) _ -> assert false
  | Tstr_class_type (* of (Ident.t * string loc * class_type_declaration) list *) _ -> assert false
*)
    
and value_binding_list vbs =
  flip concat_map vbs & fun vb ->
    let doc = get_doc vb.vb_attributes in
    let id_locs = let_bound_idents_with_loc [vb] in
    map (fun (id, {loc}) -> (KValue, id), Value (Def loc), doc) id_locs

(*
and primitive vd = value_description vd

*)

and value_description vd = 
  (KValue, vd.val_id), Value (Def vd.val_name.loc), get_doc vd.val_attributes
    
and type_declaration td = 
  (KType, td.typ_id), Type (Def td.typ_name.loc, type_kind td.typ_kind), get_doc td.typ_attributes
    
(*
  {
    typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * variance) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attributes;
   }
*)

and type_kind = function
  | Ttype_abstract -> Abstract
  | Ttype_variant cds -> Variant (map constructor_declaration cds)
  | Ttype_record lds -> Record (map label_declaration lds)
  | Ttype_open -> Open

and constructor_declaration cd =
  (cd.cd_id, Def cd.cd_name.loc, get_doc cd.cd_attributes)
  (* {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attributes;
    }
  *)

and label_declaration ld =
  (ld.ld_id, Def ld.ld_name.loc, get_doc ld.ld_attributes)
(*
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attributes;
    }
*)

and type_extension te = map extension_constructor te.tyext_constructors
(*
  (Tyext te.tyext_path, te.tyext_txt.loc)
  :: map (extension_constructor (Some (te.tyext_path))) te.tyext_constructors
*)
  
(*  {
    tyext_path: Path.t;
    tyext_txt: t loc;
    tyext_params: (core_type * variance) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_attributes: attributes;
  }
*)

and extension_constructor ec = 
  (KTypext, ec.ext_id), Typext (Def ec.ext_loc), get_doc ec.ext_attributes
  
  (* (`Exconst (ec.ext_id, ec, p), ec.ext_name.loc) *)

(*
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type : Types.extension_constructor;
    ext_kind : extension_constructor_kind;
    ext_loc : Location.t;
    ext_attributes: attributes;
  }
*)

and module_binding mb =
  let m = module_expr mb.mb_expr in
  (KModule, mb.mb_id), (Module (Def mb.mb_name.loc, m)), get_doc mb.mb_attributes
    (* :: fold_left (accum (Some (Lident mb.mb_id.Ident.name))) env ts*)
(*
    {
     mb_id: Ident.t;
     mb_name: string loc;
     mb_expr: module_expr;
     mb_attributes: attributes;
     mb_loc: Location.t;
    }
*)

and module_expr me = module_expr_desc me me.mod_desc

(*
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }
*)

and module_expr_desc me = function
  | Tmod_structure s -> Structure (structure s)

(*
  | Tmod_ident (p, {loc}) when Path.is_global p ->
      (* not sure we can scrape here... we lose something ? *)
      let mty = Env.scrape_alias (Envaux.env_of_only_summary me.mod_env) me.mod_type in
      begin match Ty.module_type mty with
      | Structure m ->
          let ts = map (fun (id,d,_) -> id, d, Global (p,id, loc)) m in
          Structure ts
      | _ -> assert false
      end
*)
      
  | Tmod_ident (p, {loc=_}) -> Var p
      
  | Tmod_constraint (mexp, _ty_mty, Tmodtype_explicit mty, _mc) ->
      let x = module_expr mexp in
      let y = module_type mty in
      Constraint (x,y)
        
  | Tmod_constraint (mexp, ty_mty, Tmodtype_implicit, _mc) ->
      let x = module_expr mexp in
      (* not sure we can scrape here... we lose something ? *)
      let mty = Env.scrape_alias (Envaux.env_of_only_summary me.mod_env) ty_mty in
      let y = Ty.module_type mty in
      (* We do our own shadowing. Hope this is correct. *)
      (* !!% "mc: %a@." format_module_coercion mc; *)
      (* fst (shadow xs) *)
      Constraint (x, y)

  | Tmod_functor (id, {loc=_}, Some mty, mexp) ->
      let m = module_type mty in
      let m' = module_expr mexp in
      Functor (id, Some m, m')
(*
      fun env ->
        let m, _env = module_type mty env in
        let env' = fold_left (accum (Some (Lident id.Ident.name))) env xs in
        module_expr mexp env'  (* discard env? *)
*)

  | Tmod_apply (f, m, _mc) -> (* module_expr * module_expr * module_coercion *) (* TODO mc *)
      let f = module_expr f in
      let m = module_expr m in
      App (f, m)

  | _ -> assert false
(*
  | Tmod_ident of Path.t * t loc
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
    (** ME          (constraint = Tmodtype_implicit)
        (ME : MT)   (constraint = Tmodtype_explicit MT)
     *)
  | Tmod_unpack of expression * Types.module_type
*)

and module_type mty = module_type_desc mty.mty_desc

(*
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attributes;
   }
*)

and module_type_desc mtd = match mtd with
  | Tmty_signature sg -> Structure (signature sg)
  | Tmty_with (mty, _withs (* (Path.t * t loc * with_constraint) list *)) ->
      (* Tmty_with of module_type * (Path.t * t loc * with_constraint) list *)
      module_type mty (* we bravely ignore the with things *)
  | Tmty_ident (p, _) -> Var p
  | Tmty_alias (p, _) -> Var p
  | Tmty_typeof me -> module_expr me
  | _ -> assert false

(*
and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * string loc * module_type option * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc
*)
      
and signature s = concat_map signature_item s.sig_items

(*
 {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}
*)
  

and signature_item si = match si.sig_desc with
  | Tsig_open _ 
  | Tsig_attribute _ -> []
  | Tsig_value vd -> [value_description vd]
  | Tsig_type (_, tds) -> map type_declaration tds
  | Tsig_module md -> [module_declaration md]
  | Tsig_include id -> include_description si.sig_loc id
(*
  | Tsig_typext te -> type_extension te
  | Tsig_exception ec -> extension_constructor None ec >>= fun x -> return [x]
  | Tsig_recmodule mds -> mapM module_declaration mds
*)
  | _ -> assert false
(*
  | Tsig_modtype of module_type_declaration
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
*)

and module_declaration md = 
  (KModule, md.md_id), 
  Module (Def md.md_loc, module_type md.Typedtree.md_type),
  get_doc md.md_attributes

(*
    {
     md_id: Ident.t;
     md_name: string loc;
     md_type: module_type;
     md_attributes: attributes;
     md_loc: Location.t;
    }
*)

and module_type_declaration mtd =
  (KModtype, mtd.mtd_id), Modtype (Def mtd.mtd_name.loc, Option.fmap module_type mtd.Typedtree.mtd_type)
(*
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_type: module_type option;
     mtd_attributes: attributes;
     mtd_loc: Location.t;
    }
*)

and include_declaration loc id =
  let m = module_expr id.incl_mod in
  let kvs = Ty.signature id.incl_type in
  flip map kvs (fun ((kind,id as k),_v,doc) -> 
    k, Alias (Def loc, kind, id, m), doc)
    
(*
and include_declaration = module_expr include_infos

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }

and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_attributes: attribute list;
    }
*)



(*

(** {2 Core language} *)

type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attributes) list;
    pat_type: type_expr;
    mutable pat_env: Env.t;
    pat_attributes: attributes;
   }

and pat_extra =
  | Tpat_constraint of core_type
        (** P : T          { pat_desc = P
                           ; pat_extra = (Tpat_constraint T, _, _) :: ... }
         *)
  | Tpat_type of Path.t * t loc
        (** #tconst        { pat_desc = disjunction
                           ; pat_extra = (Tpat_type (P, "tconst"), _, _) :: ...}

                           where [disjunction] is a [Tpat_or _] representing the
                           branches of [tconst].
         *)
  | Tpat_unpack
        (** (module P)     { pat_desc  = Tpat_var "P"
                           ; pat_extra = (Tpat_unpack, _, _) :: ... }
         *)

and pattern_desc =
    Tpat_any
        (** _ *)
  | Tpat_var of Ident.t * string loc
        (** x *)
  | Tpat_alias of pattern * Ident.t * string loc
        (** P as a *)
  | Tpat_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Tpat_tuple of pattern list
        (** (P1, ..., Pn)

            Invariant: n >= 2
         *)
  | Tpat_construct of
      t loc * constructor_description * pattern list
        (** C                []
            C P              [P]
            C (P1, ..., Pn)  [P1; ...; Pn]
          *)
  | Tpat_variant of label * pattern option * row_desc ref
        (** `A             (None)
            `A P           (Some P)

            See {!Types.row_desc} for an explanation of the last parameter.
         *)
  | Tpat_record of
      (t loc * label_description * pattern) list *
        closed_flag
        (** { l1=P1; ...; ln=Pn }     (flag = Closed)
            { l1=P1; ...; ln=Pn; _}   (flag = Open)

            Invariant: n > 0
         *)
  | Tpat_array of pattern list
        (** [| P1; ...; Pn |] *)
  | Tpat_or of pattern * pattern * row_desc option
        (** P1 | P2

            [row_desc] = [Some _] when translating [Ppat_type _],
                         [None] otherwise.
         *)
  | Tpat_lazy of pattern
        (** lazy P *)

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attributes) list;
    exp_type: type_expr;
    exp_env: Env.t;
    exp_attributes: attributes;
   }

and exp_extra =
  | Texp_constraint of core_type
        (** E : T *)
  | Texp_coerce of core_type option * core_type
        (** E :> T           [Texp_coerce (None, T)]
            E : T0 :> T      [Texp_coerce (Some T0, T)]
         *)
  | Texp_open of override_flag * Path.t * t loc * Env.t
        (** let open[!] M in    [Texp_open (!, P, M, env)]
                                where [env] is the environment after opening [P]
         *)
  | Texp_poly of core_type option
        (** Used for method bodies. *)
  | Texp_newtype of string
        (** fun (type t) ->  *)

and expression_desc =
    Texp_ident of Path.t * t loc * Types.value_description
        (** x
            M.x
         *)
  | Texp_constant of constant
        (** 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of arg_label * case list * partial
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * (arg_label * expression option) list
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * case list * case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 -> E2
            | exception P3 -> E3

            [Texp_match (E0, [(P1, E1); (P2, E2)], [(P3, E3)], _)]
         *)
  | Texp_try of expression * case list
        (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple of expression list
        (** (E1, ..., EN) *)
  | Texp_construct of
      t loc * constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * expression option
  | Texp_record of
      (t loc * label_description * expression) list *
        expression option
  | Texp_field of expression * t loc * label_description
  | Texp_setfield of
      expression * t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_unreachable
  | Texp_extension_constructor of t loc * Path.t

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

and case =
    {
     c_lhs: pattern;
     c_guard: expression option;
     c_rhs: expression;
    }

(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: Types.class_type;
     cl_env: Env.t;
     cl_attributes: attributes;
    }

and class_expr_desc =
    Tcl_ident of Path.t * t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * string loc * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * expression option) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * string loc * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * Concr.t
    (* Visible instance variables, methods and concretes methods *)

and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: Types.class_signature;
   cstr_meths: Ident.t Meths.t;
  }

and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attributes;
  }

and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression

and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
  (** The module type constraint has been synthesized during typecheking. *)
  | Tmodtype_explicit of module_type
  (** The module type was in the source file. *)

and module_expr_desc =
    Tmod_ident of Path.t * t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type option * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
    (** ME          (constraint = Tmodtype_implicit)
        (ME : MT)   (constraint = Tmodtype_explicit MT)
     *)
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of extension_constructor
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_description
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding =
    {
     mb_id: Ident.t;
     mb_name: string loc;
     mb_expr: module_expr;
     mb_attributes: attributes;
     mb_loc: Location.t;
    }

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Path.t * module_coercion

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attributes;
   }

and module_type_desc =
    Tmty_ident of Path.t * t loc
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * string loc * module_type option * module_type
  | Tmty_with of module_type * (Path.t * t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * t loc

and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: type_expr;
    pc_env: Env.t;
    pc_loc : Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of extension_constructor
  | Tsig_module of module_declaration
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration =
    {
     md_id: Ident.t;
     md_name: string loc;
     md_type: module_type;
     md_attributes: attributes;
     md_loc: Location.t;
    }

and open_description =
    {
     open_path: Path.t;
     open_txt: t loc;
     open_override: override_flag;
     open_loc: Location.t;
     open_attributes: attribute list;
    }

and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_attributes: attribute list;
    }
*)

and include_description loc id = 
  let m = module_type id.incl_mod in
  let kvs = Ty.signature id.incl_type in
  flip map kvs (fun ((kind,id as k),_v,doc) -> 
    k, Alias (Def loc, kind, id, m), doc)

(*
and include_description = module_type include_infos

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * t loc
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * t loc

and core_type =
  { mutable ctyp_desc : core_type_desc;
      (** mutable because of [Typeclass.declare_method] *)
    mutable ctyp_type : type_expr;
      (** mutable because of [Typeclass.declare_method] *)
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attributes;
   }

and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * t loc * core_type list
  | Ttyp_object of (string * attributes * core_type) list * closed_flag
  | Ttyp_class of Path.t * t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

and package_type = {
  pack_path : Path.t;
  pack_fields : (t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : t loc;
}

and row_field =
    Ttag of label * attributes * bool * core_type list
  | Tinherit of core_type

and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attributes;
    }

and type_declaration =
  {
    typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * variance) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attributes;
   }

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attributes;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attributes;
    }

and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: t loc;
    tyext_params: (core_type * variance) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_attributes: attributes;
  }

and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type : Types.extension_constructor;
    ext_kind : extension_constructor_kind;
    ext_loc : Location.t;
    ext_attributes: attributes;
  }

and extension_constructor_kind =
    Text_decl of constructor_arguments * core_type option
  | Text_rebind of Path.t * t loc

and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: Types.class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attributes;
    }

and class_type_desc =
    Tcty_constr of Path.t * t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
  }

and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attributes;
  }

and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * variance) list;
    ci_id_name : string loc;
    ci_id_class: Ident.t;
    ci_id_class_type : Ident.t;
    ci_id_object : Ident.t;
    ci_id_typesharp : Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl : Types.class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attributes;
   }

(* Auxiliary functions over the a.s.t. *)

val iter_pattern_desc: (pattern -> unit) -> pattern_desc -> unit
val map_pattern_desc: (pattern -> pattern) -> pattern_desc -> pattern_desc

val let_bound_idents: value_binding list -> Ident.t list
val rev_let_bound_idents: value_binding list -> Ident.t list

val let_bound_idents_with_loc:
    value_binding list -> (Ident.t * string loc) list

(** Alpha conversion of patterns *)
val alpha_pat: (Ident.t * Ident.t) list -> pattern -> pattern

val mknoloc: 'a -> 'a Asttypes.loc
val mkloc: 'a -> Location.t -> 'a Asttypes.loc

val pat_bound_idents: pattern -> Ident.t list
  *)

let format_type_signature =
  let i = Ident.string_of in
  let rec signature ppf s = fprintf ppf "@[<v>%a@]" (Format.list "@ " signature_item) s
  and signature_item ppf = function
    | Sig_value (id, _) -> fprintf ppf "Value %s" (i id)
    | Sig_type (id, _td, _) -> fprintf ppf "Type %s" (i id)
    | Sig_typext (id, _ec, _es) -> fprintf ppf "Tyext %s" (i id)
    | Sig_module (id, md, _) ->
        fprintf ppf "@[<2>Module %s@ [%a]@]"
          (i id)
          module_declaration md
    | Sig_modtype (id, _md) -> fprintf ppf "Modtype %s" (i id)
    | Sig_class (id, _cd, _) -> fprintf ppf "Class %s" (i id)
    | Sig_class_type (id, _ctd, _) -> fprintf ppf "Cltype %s" (i id)
  and module_declaration ppf md = module_type ppf md.md_type
  and module_type ppf = function
    | Mty_ident p -> string ppf (Path.string_of p)
    | Mty_signature sg -> signature ppf sg
    | Mty_functor (_id, _mtyo, _mty) -> fprintf ppf "some functor"
    | Mty_alias p -> fprintf ppf "alias %s" & Path.string_of p
  in
  signature
    
let test s =
  let res = match s with
    | `Signature s -> signature s
    | `Structure s -> structure s
  in
  !!% "@[<v>%a@]@." (Format.list "@ " format) res;
  ()
(*
  !!% "@.=>@.@.";
  let s = Eval.module_ [] (Structure res) in
  begin match s with
  | Structure res ->
      !!% "@[<v>%a@]@." (Format.list "@ " format) res
  | _ -> assert false (* impos *)
  end
*)


(*
  !!% "@.type is:@.";
  !!% "%a@.@." format_type_signature s.str_type;
*)
(*
  let fs = concat_map (Flatten.flatten []) res in

  !!% "THE FLATTENED:@.";
  !!% "@[<v>%a@]@." (Format.list "@," Flatten.format) fs;
  !!% "@.";
*)

(*
  !!% "ENV: %a@.@." Xenv.Summary.format s.str_final_env;

  Env.reset_cache ();
  Envaux.reset_cache ();
*)
  
(*
  iter (fun (x,_) -> Flatten.query (let open Envaux in
                                    try env_of_only_summary s.str_final_env with Error (Module_not_found p) ->
                                      !!% "envaux: module_not_found: %s@." & Path.string_of p;
                                      exit 2;
                                    ) x) fs
*)

(*
module Flatten = struct
  open Outcometree
  module F = Hump.Make(struct type t = out_ident let ocaml_of_t = Sigext.ocaml_of_out_ident end)

  let rec value = function
    | (Value _  | Type _ | Typext | 
        
end
*)

