open Spotlib.Spot
open Utils
open List
open Types
open Outcometree
open Hump

let test = ref false
(** Test mode permits unresolved persistent idents *)  
  
module Make(A : sig
  val top_path : out_ident option
  val ambiguous_docstrings : Ocamldoc.DocSet.t
end) = struct

  let noname_ESignature_cntr = ref (-1)
      
  (** Obtain source file MD5 digest from Location.t *)
  let loc_digest =
    let cache = Hashtbl.create 17 in
    fun loc ->
      match loc.Location.loc_start.Lexing.pos_fname with
      | "" -> None
      | f ->
          Hashtbl.find_or_add (fun f ->
            match Misc.find_in_path !Config.load_path f with
            | exception _ ->
                !!% "Warning: Def not found %s@." f;
                None
            | fp ->
                Some (Digest.file fp) (* cached! *))
            cache f
  
  let def path loc doc =
    Hashcons.hump_v
    & Def { path; loc; digest = loc_digest loc; doc }
  
  let iname i = Hashcons.string i.Ident.name
  
  let func_args = Hashtbl.create 17

  let iname_func_arg i =
    match A.top_path with
    | None -> assert false
    | Some tp ->
        Hashcons.string (i.Ident.name ^ " " ^ Hump.string_of_path tp
                         ^ " " ^ string_of_int i.Ident.stamp)
        |- Hashtbl.add func_args i
    
  (* reduce the memory size in earlier stages *)
  let expr = Hashcons.hump_expr
  
  (* val path : Sig.k -> Path.t -> Hump.expr *)
  let path =
    let cache = Hashtbl.create 107 in
    let rec path k p = flip2 Hashtbl.find_or_add cache (k,p) & fun (k,p) -> 
      match p with
      | Path.Pident id when Ident.persistent id ->
          (* Tricky!
             We have 
               sig
                 module X = M
               end
             from OCaml 4.02
          *)
          if k <> Sig.KModule && k <> KModtype then begin
            !!% "persistent id but not KModule nor KModtype: %s@." id.Ident.name;
            assert false
          end;
          begin match find_global_module id with
          | None -> 
              (* EDot( expr & EVar (KModule, "NOTOP"), KModule, iname id) *)
              !!% "@[<2>Error: Failed to find module %s in@ @[%a@]@]@."
                id.Ident.name
                Format.(list "@ " string) !Config.load_path;
              criticalf "Error: Failed to find module %s in %s"
                id.Ident.name
                (String.concat " " !Config.load_path)
          | Some cmipath ->
              match Cm.guess cmipath with
              | [] ->
                  (* It is legally possible that a module is used and 
                     linked but its cmi is never installed. For example,
                     Re_cset of re.1.5.0.  
                     In that case we leave it unknown.
                  *)
                  !!% "Warning: No global path deduced for %s (Cm.guess returned empty). Let's make it unknown.@." cmipath;
                  EUnknownPath (Oide_ident (module_name cmipath))
              | (_::_::_) ->
                  !!% "Error: Ambiguous paths deduced for %s@." cmipath;
                  assert false
              | [cm] ->
                  match cm.Cm.paths with
                  | [] when !test -> EUnknownPath (Oide_dot (Oide_ident "UNRESOLVED", module_name cmipath))
                  | [] ->
                      (* Even adding the files trackable from the cmfiles of 
                         the OCamlFind packages, it may still fail here 
                         if the source is not installed by OPAM.
                      
                         ocamldoc installed w/o opam.  It fails to deduce the global package
                         name for ocaml-4.03.0/tools/depend.cmi here...
                         For this, we can just ignore the error.
                      *)
                      !!% "WARNING: No global path deduced for %s (Cm.paths=[])@." cmipath;
                      EUnknownPath (Oide_dot (Oide_ident "UNRESOLVED", module_name cmipath))
                  | p::_ -> EGVar p
          end
      | Path.Pident id ->
          let n =
            try Hashtbl.find func_args id with Not_found ->
              iname id
          in
          expr & EVar (k, n)
      | Pdot (p,n,_) ->
          EDot ( path KModule p, k, Hashcons.string n )
      | Papply (p1,p2) ->
          assert (k = KModule);
          EApply (path KModule p1, path KModule p2)
    in
    path
  
  let o_dot c id = Oide_dot (c, iname id)
  
  (* XXX We are not interested in mty but vs *)
  (* It is for include *)
  let rec get_vs = function
    | ELet (_,_,e) -> get_vs e
    | ESignature vbs -> map fst vbs
    | _ -> assert false
  
  module Doc = struct
    open Ocamldoc

    let get attrs =
      match
        filter (fun d -> not & DocSet.mem d A.ambiguous_docstrings)
        & get_doc attrs
      with
      | [] -> None
      | xs -> Some (normalize & String.concat " / " & map fst xs)
  end
  
  module Types = struct
    let warned_scrape_failures = ref []
  
    let group_signature s =
      let rec_status = function
        | Sig_value _ -> None
        | Sig_type (_,_,rs) -> Some rs
        | Sig_typext _ -> None
        | Sig_module (_,_,rs) -> Some rs
        | Sig_modtype _ -> None
        | Sig_class (_, _, rs) -> Some rs
        | Sig_class_type (_,_,rs) -> Some rs
      in
      let (^::) x xs = if x = [] then xs else x::xs in
      let rec f (rev_groups : signature list) rev_pending = function
        | [] -> rev (rev rev_pending ^:: rev_groups)
        | i::is when rec_status i = Some Trec_first ->
            f (rev rev_pending ^:: rev_groups)
              [i]
              is
        | i::is when rec_status i = Some Trec_next ->
            f rev_groups
              (i::rev_pending)
              is
        | i::is (* when rec_status i = None || rec_status i = Some Trec_not *) ->
            f ([i] :: (rev rev_pending ^:: rev_groups))
              []
              is
      in
      f [] [] s
  
  (*  Sig_value of Ident.t * value_description
    | Sig_type of Ident.t * type_declaration * rec_status
    | Sig_typext of Ident.t * extension_constructor * ext_status
    | Sig_module of Ident.t * module_declaration * rec_status
    | Sig_modtype of Ident.t * modtype_declaration
    | Sig_class of Ident.t * class_declaration * rec_status
    | Sig_class_type of Ident.t * class_type_declaration * rec_status
  *)
  
    let rec signature env sg =
      let sgs = group_signature sg in
      let rec f (env,rev_vss) = function
        | [] -> ESignature (concat & rev rev_vss)
        | sg::sgs ->
            let env = fold_left (fun env i -> Env.add_item i env) env sg in (* XXX too early? *)
            let vbs = concat_map (signature_item env) sg in
            let e = f (env, map (fun ((k,n as v),_) -> (v,EVar (k, Hashcons.string n))) vbs :: rev_vss) sgs in
            ELet (false, vbs, e)
      in
      f (env,[]) sgs
  
    and signature_item env i =
      match i with
      | Sig_value (id, vdesc) ->
          begin match vdesc.val_kind with
          | Val_prim { Primitive.prim_name } when prim_name.[0] = '%' ->
              [(KValue, iname id), expr & EValue (Prim prim_name)]
          | _ -> 
              [(KValue, iname id), expr & EValue LocNone]
          end
      | Sig_type (id, td, _rs) ->
          [(KType, iname id), expr & EType (LocNone, type_declaration td)]
      | Sig_typext (id, _ec, _) ->
          [(KTypext, iname id), expr & ETypext LocNone]
      | Sig_module (id, md, _rec_status) ->
          (* XXX no point of having EModule (LocNone ,...) *)
          [(KModule, iname id), EModule (LocNone, module_declaration env md)]
      | Sig_modtype (id, mtd) ->
          (* XXX no point of having EModtype (LocNone ,...) *)
          [(KModtype, iname id), EModtype (LocNone, modtype_declaration env mtd)]
      | Sig_class (id, cd, _rec_status) ->
          [(KClass, iname id), expr & EClass (LocNone, class_declaration cd);
           (KClasstype, iname id), expr & EClasstype (LocNone, class_declaration cd);
           (KType, iname id), expr & EType (LocNone, []);
           (KType, Hashcons.string ("#" ^ iname id)), expr & EType (LocNone, [])
          ]
      | Sig_class_type (id, ctd, _rec_status) ->
          [(KClasstype, iname id), expr & EClasstype (LocNone, class_type_declaration ctd);
           (KType, iname id), expr & EType (LocNone, []);
           (KType, Hashcons.string ("#" ^ iname id)), expr & EType (LocNone, [])
          ]
  
  (*
    and value_description _vd = SValue LocNone
  *)
  
    (*
    type value_description =
      { val_type: type_expr;                (* Type of the value *)
        val_kind: value_kind;
        val_loc: Location.t;
        val_attributes: Parsetree.attributes;
       }
    *)
  
    (*
    and value_kind =
        Val_reg                             (* Regular value *)
      | Val_prim of Primitive.description   (* Primitive *)
      | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
      | Val_self of (Ident.t * type_expr) Meths.t ref *
                    (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref *
                    string * type_expr
                                            (* Self *)
      | Val_anc of (string * Ident.t) list * string
                                            (* Ancestor *)
      | Val_unbound                         (* Unbound variable *)
  
    *)
  
    and type_declaration td = type_kind td.type_kind
  
    (*
    type type_declaration =
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
      | Type_abstract        -> []
      | Type_record (lds, _) -> map label_declaration lds
      | Type_variant cds     -> map constructor_declaration cds
      | Type_open            -> []
  
    (*
  
    and type_kind =
        Type_abstract
      | Type_record of label_declaration list  * record_representation
      | Type_variant of constructor_declaration list
      | Type_open
  
    *)
  
    and label_declaration ld = (KField, iname ld.ld_id), expr & EField LocNone
  
    (*
    and label_declaration =
      {
        ld_id: Ident.t;
        ld_mutable: mutable_flag;
        ld_type: type_expr;
        ld_loc: Location.t;
        ld_attributes: Parsetree.attributes;
      }
    *)
  
    and constructor_declaration cd = (KConstructor, iname cd.cd_id), expr & EConstructor LocNone
  
    (*
  
    and constructor_declaration =
      {
        cd_id: Ident.t;
        cd_args: constructor_arguments;
        cd_res: type_expr option;
        cd_loc: Location.t;
        cd_attributes: Parsetree.attributes;
      }
    *)
  
    (*
    and constructor_arguments =
      | Cstr_tuple of type_expr list
      | Cstr_record of label_declaration list
    *)
  
  
    (*
    type extension_constructor =
        {
          ext_type_path: Path.t;
          ext_type_params: type_expr list;
          ext_args: constructor_arguments;
          ext_ret_type: type_expr option;
          ext_private: private_flag;
          ext_loc: Location.t;
          ext_attributes: Parsetree.attributes;
        }
  
  *)
  
    and class_type = function
      | Cty_constr (_p, _tys, clty) -> (* TODO: constraint *)
  (* CR jfuruse: we should use p and tys
          let ty = Btype.newgenty (Tconstr (p, tys, ref Mnil)) in
  *)
          class_type clty
      | Cty_arrow (l, ty, clty) ->
          let oty, newty = class_type clty in
          oty,
          Btype.newgenty (Tarrow (l, ty, newty, Cok))
      | Cty_signature cs ->
          let x = class_signature cs in x,x
  
    and class_type' = function
      | Cty_constr (_p, _tys, clty) -> (* TODO: constraint *)
  (* CR jfuruse: we should use p and tys
          let ty = Btype.newgenty (Tconstr (p, tys, ref Mnil)) in
  *)
          class_type' clty
      | Cty_arrow (_l, _ty, clty) -> class_type' clty
      | Cty_signature cs -> class_signature' cs
  
  (*
    type class_type =
        Cty_constr of Path.t * type_expr list * class_type
      | Cty_signature of class_signature
      | Cty_arrow of arg_label * type_expr * class_type
  *)
  
    and class_signature cs =
      cs.csig_self
      (* we ignore cs.csig_vars *)
  
    and class_signature' cs =
      let fields, _ = Ctype.(flatten_fields & object_fields cs.csig_self) in
      flip filter_map fields & function
        | ("*dummy method*", _fk, _ty) -> None
        | (n, _fk, _ty) -> Some ((Sig.KMethod, n), expr & EMethod LocNone)
        
  (*
                 sort (fun (k1,_) (k2,_) -> compare k1 k2)
                 & Vars.bindings cs.csig_vars,
                 sort comparexpr & Concr.elements cs.csig_concr,
                 cs.csig_inher
  *)
  
  (*
    and class_signature =
      { csig_self: type_expr;
        csig_vars:
          (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
        csig_concr: Concr.t;
        csig_inher: (Path.t * type_expr list) list }
  *)
  
    and class_declaration cd =
      let oty, _newty = class_type cd.cty_type in
      let meths =
        let fields, _ = Ctype.(flatten_fields & object_fields oty) in
        flip filter_map fields & function
          | ("*dummy method*", _fk, _ty) -> None
          | (n, _fk, _ty) -> Some ((Sig.KMethod, n), expr & EMethod LocNone)
      in
      meths
  
  (*
    type class_declaration =
      { cty_params: type_expr list;
        mutable cty_type: class_type;
        cty_path: Path.t;
        cty_new: type_expr option; (* None then virtual *)
        cty_variance: Variance.t list;
        cty_loc: Location.t;
        cty_attributes: Parsetree.attributes;
      }
  *)
  
    and class_type_declaration ctd =
      let oty, _newty= class_type ctd.clty_type in
      let meths =
        let fields, _ = Ctype.(flatten_fields & object_fields oty) in
        flip filter_map fields & function
          | ("*dummy method*", _, _) -> None
          | (n, _fk, _ty) -> Some ((Sig.KMethod, n), expr & EMethod LocNone)
      in
      meths
  (*
    type class_type_declaration =
      { clty_params: type_expr list;
        clty_type: class_type;
        clty_path: Path.t;
        clty_variance: Variance.t list;
        clty_loc: Location.t;
        clty_attributes: Parsetree.attributes;
      }
  *)
  
    and module_type env mty =
      let mty = Env.scrape_alias env mty in
      let mty = Subst.(modtype identity mty) in (* hope it renames idents nicely *)
      match mty with
      | Mty_ident p ->
          if add_if_not_mem p warned_scrape_failures = `NewlyAdded then begin
            !!% "Warning: scraping failure of Mty_ident: %s@." (Path.string_of p);
            (* Dumpsource.dump_it (); *)
          end;
          expr & EUnknownPath (Hashcons.out_ident & Printtyp.tree_of_path p)
      | Mty_alias p ->
          if add_if_not_mem p warned_scrape_failures = `NewlyAdded then begin
            !!% "Warning: scraping failure of Mty_alias: %s@." (Path.string_of p);
            (* Dumpsource.dump_it (); *)
          end;
          expr & EUnknownPath (Printtyp.tree_of_path p)
      | Mty_signature sg -> signature env sg
      | Mty_functor (id, mtyo, mty) ->
          let i = iname_func_arg id in
          let env' = match mtyo with
            | None -> env
            | Some mty -> Env.add_module ~arg:true id mty env
          in
          let mtyo = Option.fmap (module_type env) mtyo in
          let mty = module_type env' mty in
          let body = match mtyo with
            | None -> mty
            | Some mty' ->
                ELet (false, [(KModule, i), ECoerce (EVar (KModule, i), mty')], mty)
          in
          EFunctor ((KModule, i), mtyo, body)
    (*
  
    type module_type =
        Mty_ident of Path.t
      | Mty_signature of signature
      | Mty_functor of Ident.t * module_type option * module_type
      | Mty_alias of Path.t
  
    *)
  
    and module_declaration env md = module_type env md.md_type
  
    (*
    and module_declaration =
      {
        md_type: module_type;
        md_attributes: Parsetree.attributes;
        md_loc: Location.t;
      }
      *)
  
    and modtype_declaration env md = Option.fmap (module_type env) md.mtd_type
  
    (*
    and modtype_declaration =
      {
        mtd_type: module_type option;  (* None: abstract *)
        mtd_attributes: Parsetree.attributes;
        mtd_loc: Location.t;
      }
  
    (* Constructor and record label descriptions inserted held in typing
       environments *)
  
    type constructor_description =
      { cstr_name: string;                  (* Constructor name *)
        cstr_res: type_expr;                (* Type of the result *)
        cstr_existentials: type_expr list;  (* list of existentials *)
        cstr_args: type_expr list;          (* Type of the arguments *)
        cstr_arity: int;                    (* Number of arguments *)
        cstr_tag: constructor_tag;          (* Tag for heap blocks *)
        cstr_consts: int;                   (* Number of constant constructors *)
        cstr_nonconsts: int;                (* Number of non-const constructors *)
        cstr_normal: int;                   (* Number of non generalized constrs *)
        cstr_generalized: bool;             (* Constrained return type? *)
        cstr_private: private_flag;         (* Read-only constructor? *)
        cstr_loc: Location.t;
        cstr_attributes: Parsetree.attributes;
        cstr_inlined: type_declaration option;
       }
  
    and constructor_tag =
        Cstr_constant of int                (* Constant constructor (an int) *)
      | Cstr_block of int                   (* Regular constructor (a block) *)
      | Cstr_extension of Path.t * bool     (* Extension constructor
                                               true if a constant false if a block*)
  
    type label_description =
      { lbl_name: string;                   (* Short name *)
        lbl_res: type_expr;                 (* Type of the result *)
        lbl_arg: type_expr;                 (* Type of the argument *)
        lbl_mut: mutable_flag;              (* Is this a mutable field? *)
        lbl_pos: int;                       (* Position in block *)
        lbl_all: label_description array;   (* All the labels in this type *)
        lbl_repres: record_representation;  (* Representation for this record *)
        lbl_private: private_flag;          (* Read-only field? *)
        lbl_loc: Location.t;
        lbl_attributes: Parsetree.attributes;
      }
    *)
  
    let signature sg =
      warned_scrape_failures := [];
      signature sg
  end
  
  module Typedtree = struct
  
    open Typedtree
  
    let env_of_only_summary env =
      try
        Envaux.env_of_only_summary env
      with
      | (Envaux.Error (Envaux.Module_not_found _ as e) as exn) ->
          (* This is a critical: some modules are compiled but the compiled
             files are gone. We really cannot continue, at least for this
             module.
          *)
          !!% "Error: %a@." Envaux.report_error e;
          !!% "  @[<2>Current load path:@ @[<v>%a@]@]@."
            Format.(list "@," string) !Config.load_path;
          raise exn
            
    let types_module_type env mty =
      let env = env_of_only_summary env in
      Types.module_type env mty
  
    let types_signature env mty =
      let env = env_of_only_summary env in
      Types.signature env mty
  
    let rec pattern c pat =
      let doc = Doc.get pat.pat_attributes in
      pattern_desc c doc pat.pat_desc (* XXX extra? *)
  
  (*
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
    | Tpat_type of Path.t * Longident.t loc
          (** #tconst        { pat_desc = disjunction
                             ; pat_extra = (Tpat_type (P, "tconst"), _, _) :: ...}
  
                             where [disjunction] is a [Tpat_or _] representing the
                             branches of [tconst].
           *)
    | Tpat_unpack
          (** (module P)     { pat_desc  = Tpat_var "P"
                             ; pat_extra = (Tpat_unpack, _, _) :: ... }
           *)
  *)
  
    and pattern_desc c doc = function
      | Tpat_any -> []
      | Tpat_var (id, {loc}) ->
          [(Sig.KValue, iname id), expr & EValue (def (o_dot c id) loc doc)]
      | Tpat_alias (p,id,{loc}) ->
          ((KValue, iname id), expr & EValue (def (o_dot c id) loc doc))
          :: pattern c p
      | Tpat_constant _ -> []
      | Tpat_tuple ps -> concat_map (pattern c) ps
      | Tpat_construct (_, _cd, ps) -> concat_map (pattern c) ps
      | Tpat_variant (_, po, _rdr) -> concat_map (pattern c) & Option.to_list po
      | Tpat_record (fs, _) -> concat_map (fun (_,_,p) -> pattern c p) fs
      | Tpat_array ps -> concat_map (pattern c) ps
      | Tpat_or (p1, p2, _) -> pattern c p1 @ pattern c p2
      | Tpat_lazy p -> pattern c p
  
  (*
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
        Longident.t loc * constructor_description * pattern list
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
        (Longident.t loc * label_description * pattern) list *
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
  
  and meth =
      Tmeth_name of string
    | Tmeth_val of Ident.t
  
  (* Value expressions for the class language *)
  *)
    and class_expr c ce =
      match class_expr_desc c ce.cl_desc with
      | Some x -> x
      | None -> Types.class_type' ce.cl_type
      
    (*
  and class_expr =
      {
       cl_desc: class_expr_desc;
       cl_loc: Location.t;
       cl_type: Types.class_type;
       cl_env: Env.t;
       cl_attributes: attributes;
      }
    *)
  
    (* We give up if we see asomething difficult *)
    and class_expr_desc c = function
      | Tcl_ident _ -> None
      | Tcl_structure cs -> Some (class_structure c cs)
      | Tcl_fun (_, _, _, ce, _) -> Some (class_expr c ce)
      | Tcl_apply (_, _) -> None
      | Tcl_let (_, _, _, ce) -> Some (class_expr c ce)
      | Tcl_constraint (ce, _, _, _meths, _concr_meths) -> Some (class_expr c ce) (* XXX need to revisit *)
          (* class_expr * class_type option * string list * string list * Concr.t *)
        
  (*    
  and class_expr_desc =
      Tcl_ident of Path.t * Longident.t loc * core_type list
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
  *)
  
    and class_structure c cs =
      let written = concat_map (class_field c) cs.cstr_fields in
      (* XXX scanning all is a bit inefficient *)
      let all = Types.class_signature' cs.cstr_type in
      let hidden =
        let written_ks = map fst written in
        flip filter all & fun (k,_) -> not & mem k written_ks
      in
      if hidden <> [] then !!% "WOW! HIDDEN METHODS FOUND!: %a@." Format.(list " " string) (map (snd *< fst) hidden);
      written @ hidden
        
    (* XXX [method x] overrides the method with the same name inherited, so we should clean it *)
  
  (*        
  and class_structure =
    {
     cstr_self: pattern;
     cstr_fields: class_field list;
     cstr_type: Types.class_signature;
     cstr_meths: Ident.t Meths.t;
    }
  *)
  
    and class_field c cf =
      let doc = Doc.get cf.cf_attributes in
      class_field_desc c doc cf.cf_loc cf.cf_desc
  
  (*
  and class_field =
     {
      cf_desc: class_field_desc;
      cf_loc: Location.t;
      cf_attributes: attributes;
    }
  
  and class_field_kind =
    | Tcfk_virtual of core_type
    | Tcfk_concrete of override_flag * expression
  *)
  
    and class_field_desc c doc _loc = function
  (*
      | Tcf_inherit (_, _, _, _, meths) ->   (* Inherited instance variables and concrete methods *)
          flip filter_map meths & (function
            | ("*dummy method*",_) -> None
            | (n,_) -> Some ((KMethod, n), EMethod (def (Oide_dot (c, n)) loc)))
  *)
      | Tcf_inherit (_, ce, _, _, _) -> class_expr c ce
      | Tcf_val _ -> []
      | Tcf_method (n, _private_flag, _class_field_kind) ->
          [(KMethod, n.txt),
           expr & EMethod (def (Oide_dot (c, n.txt)) n.loc doc )]
      | Tcf_constraint (_cty, _cty') -> []
      | Tcf_initializer _ -> []
      | Tcf_attribute _ -> []
    
  (*    
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
  *)
  
    and module_expr c me = module_expr_desc c me.mod_env me.mod_type me.mod_desc
  
  (*
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
  
  *)
  
    and module_expr_desc c env _mty = function
      | Tmod_structure str -> structure c str
      | Tmod_ident (p, _) -> path KModule p
  
      | Tmod_constraint (me, _, Tmodtype_explicit mty, _) ->
          (* module M : S *)
          let mty = module_type c mty in
          let me = module_expr c me in
          ECoerce (me, mty)
  
      | Tmod_constraint (me, _, Tmodtype_implicit, _) ->
  (* XXX this chokes ppx_type_conv
          let mty = types_module_type env mty in
          let me = module_expr c me in
            !!% "@[<2>Constrainting@ %a by@ %a@]@."
              (Ocaml.format_with ocaml_of_smodule) me
              (Ocaml.format_with ocaml_of_smodule) mty;
      (* Coerce.smodule me ~by:mty *)
          ECoerce (me, mty)
  *)
          (* We bravely ignore the above ! *)
          module_expr c me
            
      | Tmod_functor (id, {loc=_}, mto, me) ->
          let i = iname_func_arg id in
          let mto = Option.fmap (module_type (o_dot c id)) mto in
          let m = module_expr (Oide_apply (c, Oide_ident (iname id))) me in
          let body = match mto with
            | None -> m
            | Some mty' ->
                ELet (false, [(KModule, i), ECoerce (EVar (KModule, i), mty')], m)
          in
          EFunctor ((KModule, i), mto, body)
  
      | Tmod_apply (m1, m2, _) ->
          incr noname_ESignature_cntr;
          let c' = Oide_dot (c, Printf.sprintf "<unknown%d>" !noname_ESignature_cntr) in
          let m1 = module_expr c' m1 in
          let m2 = module_expr c' m2 in
          EApply (m1, m2)
  
      | Tmod_unpack (_, mty) ->
          (* SUnpack (types_module_type env mty) *)
          types_module_type env mty
  
      (*
  and module_expr_desc =
      Tmod_ident of Path.t * Longident.t loc
    | Tmod_structure of structure
    | Tmod_functor of Ident.t * string loc * module_type option * module_expr
    | Tmod_apply of module_expr * module_expr * module_coercion
    | Tmod_constraint of
        module_expr * Types.module_type * module_type_constraint * module_coercion
      (** ME          (constraint = Tmodtype_implicit)
          (ME : MT)   (constraint = Tmodtype_explicit MT)
       *)
    | Tmod_unpack of expression * Types.module_type
  *)
  
    and structure c s =
      let rec f rev_vs = function
        | [] ->ESignature (map (fun (k,n as id) -> (id, expr & EVar (k, Hashcons.string n))) & concat & rev rev_vs)
        | si::sis ->
            let zero () = f rev_vs sis in
            let one g x =
              let vb = g x in
              let e = f ([fst vb] :: rev_vs) sis in
              ELet (false, [vb], e)
            in
            let list rec_ g x = 
              let vbs = g x in
              let e = f (map fst vbs :: rev_vs) sis in
              ELet (rec_, vbs, e)
            in
            match si.str_desc with
            | Tstr_eval _
            | Tstr_attribute _
            | Tstr_open _ -> zero ()
            | Tstr_value (_rf, vbs) ->
                (* This can be recursion but not recursive over modules *)
                list false (concat_map (value_binding c)) vbs
            | Tstr_primitive vd ->
                one (value_description c) vd
            | Tstr_type (_rf, tds) ->
                (* This can be recursion but not recursive over modules *)
                list false (map (type_declaration c)) tds
            | Tstr_typext te ->
                (* This can be recursion but not recursive over modules *)
                list false (type_extension c) te
            | Tstr_exception ec ->
                (* no distinction with open type constructors *)
                one (extension_constructor c) ec
            | Tstr_module mb ->
                one (module_binding c) mb
            | Tstr_recmodule mbs ->
                list true (map (module_binding c)) mbs
            | Tstr_modtype mtd ->
                one (module_type_declaration c) mtd
            | Tstr_include id ->
                let doc = Doc.get id.incl_attributes in
                let m, mty = include_declaration c id in
                (* XXX We are not interested in mty but vs *)
                let vs = get_vs mty in
                let e = f (map (fun v -> [v]) vs @ rev_vs) sis in
                ELet (false, [(KModule, "*include*"), m],
                      fold_right (fun (k,n as id) e ->
                        ELet (false, [id, EAddAlias(def (Oide_dot(c,snd id))
                                                      si.str_loc
                                                      doc,
                                                    EDot(expr & EVar(KModule, "*include*"), k, Hashcons.string n))], e))
                        vs e)
  
            | Tstr_class xs ->
                (* Tstr_class of (class_declaration * string list (* XXX I do not know what they are for *) ) list *)
                list false (concat_map (fun (x,_) -> class_declaration c x)) xs
  
            | Tstr_class_type xs ->
                list false (concat_map (fun (_, _n, ctd) -> class_type_declaration c ctd)) xs
            (* | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list *)
      in f [] s.str_items
                
  (*
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
  *)
  
    and module_binding c mb =
      let doc = Doc.get mb.mb_attributes in
      let c' = o_dot c mb.mb_id in
      (KModule, iname mb.mb_id),
      EModule (def c' mb.mb_name.loc doc, module_expr c' mb.mb_expr)
  
  (*
  and module_binding =
      {
       mb_id: Ident.t;
       mb_name: string loc;
       mb_expr: module_expr;
       mb_attributes: attributes;
       mb_loc: Location.t;
      }
  *)
  
    and value_binding c vb =
      (* [let x = P.y] introduces an alias *)
      match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
      | Tpat_var (id, {loc}), Texp_ident (p, _, _) ->
          let doc = Doc.get vb.vb_pat.pat_attributes in
          [(Sig.KValue, iname id),
           EAddAlias (def (o_dot c id) loc doc,
                      path Sig.KValue p)]
      | _ -> pattern c vb.vb_pat
  
  (*
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
  *)
  
    and module_type c mt = module_type_desc c mt.mty_env mt.mty_type mt.mty_desc
  
  (*
  and module_type =
    { mty_desc: module_type_desc;
      mty_type : Types.module_type;
      mty_env : Env.t;
      mty_loc: Location.t;
      mty_attributes: attributes;
     }
  *)
  
    and module_type_desc c env mty = function
      | Tmty_signature s -> signature c s
  
      | Tmty_ident (p, {loc=_}) -> path KModtype p
  
      | Tmty_alias (p, {loc=_}) ->
          (* XXX module M = N  in signature *)
          (* It is not referring the module type named p but
             the module type of the module p 
          *)
          path KModule p
      | Tmty_functor (id, {loc=_}, mtyo, mty) ->
          let i = iname_func_arg id in
          let mtyo = Option.fmap (module_type c) mtyo in
          let mty = module_type c mty in
          let body = match mtyo with
            | None -> mty
            | Some mty' ->
                ELet (false, [(KModule, i), ECoerce (EVar (KModule, i), mty')], mty)
          in
          EFunctor((KModule, i), mtyo, body)
  
      | Tmty_with (mty', _) ->
          (* mty == mty' with ... *)
          (* mty must be the subtype of mty'
             See http://caml.inria.fr/mantis/view.php?id=5514
                 http://caml.inria.fr/mantis/view.php?id=7337
          *)
          (* XXX We should look into the with_constraint otherwise
             we lose some information here *)
          let mty' = module_type c mty' in
          let mty = types_module_type env mty in
          EWith (mty, mty')
      | Tmty_typeof m -> module_expr c m
  
  (*
  and module_type_desc =
      Tmty_ident of Path.t * Longident.t loc
    | Tmty_signature of signature
    | Tmty_functor of Ident.t * string loc * module_type option * module_type
    | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
    | Tmty_typeof of module_expr
    | Tmty_alias of Path.t * Longident.t loc
  
  and primitive_coercion =
    {
      pc_desc: Primitive.description;
      pc_type: type_expr;
      pc_env: Env.t;
      pc_loc : Location.t;
    }
  *)
  
    and signature c sg =
      let rec f rev_vs = function
        | [] ->
            ESignature (
              map (fun (k,n as id) -> (id, expr & EVar (k, Hashcons.string n)))
              & concat & rev rev_vs
            )
        | si::sis ->
            (* XXX dupe at [structure] *)
            let zero () = f rev_vs sis in
            let one g x = 
              let vb = g x in
              let e = f ([fst vb] :: rev_vs) sis in
              ELet (false, [vb], e)
            in
            let list rec_ g x = 
              let vbs = g x in
              let e = f (map fst vbs :: rev_vs) sis in
              ELet (rec_, vbs, e)
            in
            match si.sig_desc with
            | Tsig_open _ 
            | Tsig_attribute _ -> zero ()
            | Tsig_value vd ->
                one (value_description c) vd
            | Tsig_type (_rf, tds) ->
                (* It is recursive, but no recursion over module *) 
                list false (map (type_declaration c)) tds
            | Tsig_typext te ->
                list false (type_extension c) te
            | Tsig_exception ec ->
                one (extension_constructor c) ec
            | Tsig_module md ->
                one (module_declaration c) md
            | Tsig_recmodule mds ->
                list true (map (module_declaration c)) mds
            | Tsig_modtype mtd ->
                one (module_type_declaration c) mtd
            | Tsig_include id ->
                let doc = Doc.get id.incl_attributes in
                (* XXX We are not interested in mty but vs *)
                let m, mty = include_description c id in
                let vs = get_vs mty in
                let e = f (map (fun v -> [v]) vs @ rev_vs) sis in
                ELet (false, [(KModule, "*include*"), m],
                      fold_right (fun (k,n as id) e ->
                        ELet (false, [id, EAddAlias(def (Oide_dot(c,snd id))
                                                      si.sig_loc
                                                      doc,
                                                    EDot (expr & EVar (KModule, "*include*"), k, Hashcons.string n))], e))
                        vs e)
            | Tsig_class cds ->
                list false (concat_map (class_description c)) cds
            | Tsig_class_type ctds ->
                list false (concat_map (class_type_declaration c)) ctds
      in f [] sg.sig_items
  
  (*
  and signature = {
    sig_items : signature_item list;
    sig_type : Types.signature;
    sig_final_env : Env.t;
  }
  *)
  
  (*
  and signature_item =
    { sig_desc: signature_item_desc;
      sig_env : Env.t; (* BINANNOT ADDED *)
      sig_loc: Location.t }
  *)
  
  (*
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
  
  *)
    and module_declaration c md =
      let doc = Doc.get md.md_attributes in
      ((KModule, iname md.md_id),
       EModule (def (o_dot c md.md_id) md.md_name.loc doc,
                module_type c md.md_type))
  
  (*
      {
       md_id: Ident.t;
       md_name: string loc;
       md_type: module_type;
       md_attributes: attributes;
       md_loc: Location.t;
      }
  *)
  
    and module_type_declaration c mtd =
      let doc = Doc.get mtd.mtd_attributes in
      let c' = o_dot c mtd.mtd_id in
      (KModtype, iname mtd.mtd_id),
      EModtype (def c' mtd.mtd_name.loc doc, Option.fmap (module_type c') mtd.mtd_type)
  
  (*
  and module_type_declaration =
      {
       mtd_id: Ident.t;
       mtd_name: string loc;
       mtd_type: module_type option;
       mtd_attributes: attributes;
       mtd_loc: Location.t;
      }
  
  and open_description =
      {
       open_path: Path.t;
       open_txt: Longident.t loc;
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
  
    and include_description c id =
      let mty = types_signature id.incl_mod.mty_env id.incl_type in
      let m = module_type c id.incl_mod in
      m, mty
  
  (*
  and include_description = module_type include_infos
  *)
  
    and include_declaration c id =
      let mty = types_signature id.incl_mod.mod_env id.incl_type in
      let m = module_expr c id.incl_mod in
      m, mty
  
  
  (*
  
  and with_constraint =
      Twith_type of type_declaration
    | Twith_module of Path.t * Longident.t loc
    | Twith_typesubst of type_declaration
    | Twith_modsubst of Path.t * Longident.t loc
  
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
    | Ttyp_constr of Path.t * Longident.t loc * core_type list
    | Ttyp_object of (string * attributes * core_type) list * closed_flag
    | Ttyp_class of Path.t * Longident.t loc * core_type list
    | Ttyp_alias of core_type * string
    | Ttyp_variant of row_field list * closed_flag * label list option
    | Ttyp_poly of string list * core_type
    | Ttyp_package of package_type
  
  and package_type = {
    pack_path : Path.t;
    pack_fields : (Longident.t loc * core_type) list;
    pack_type : Types.module_type;
    pack_txt : Longident.t loc;
  }
  
  and row_field =
      Ttag of label * attributes * bool * core_type list
    | Tinherit of core_type
  *)
  
    and value_description c vd =
      let doc = Doc.get vd.val_attributes in
      match vd.val_val.val_kind with
      | Val_prim { Primitive.prim_name } when prim_name.[0] = '%' ->
          (KValue, iname vd.val_id), 
        expr & EValue (Aliased (def (o_dot c vd.val_id) vd.val_name.loc doc,
                                Prim prim_name))
      | _ -> 
          (KValue, iname vd.val_id),
          expr & EValue (def (o_dot c vd.val_id) vd.val_name.loc doc)
  
  (*
  and value_description =
    { val_id: Ident.t;
      val_name: string loc;
      val_desc: core_type;
      val_val: Types.value_description;
      val_prim: string list;
      val_loc: Location.t;
      val_attributes: attributes;
      }
  *)
  
    and type_declaration c td =
      let doc = Doc.get td.typ_attributes in
      let stk = type_kind c td.typ_kind in
      (KType, iname td.typ_id),
      expr & EType (def (o_dot c td.typ_id) td.typ_name.loc doc, stk)
  
  (*
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
  *)
  
    and type_kind c = function (* Todo: Think about manifested case *)
      | Ttype_abstract -> []
      | Ttype_open -> []
      | Ttype_variant cds -> map (constructor_declaration c) cds
      | Ttype_record lds -> map (label_declaration c) lds
  
  
  (*
  and type_kind =
      Ttype_abstract
    | Ttype_variant of constructor_declaration list
    | Ttype_record of label_declaration list
    | Ttype_open
  *)
  
    and label_declaration c ld =
      let doc = Doc.get ld.ld_attributes in
      (KField, iname ld.ld_id),
      expr & EField (def (o_dot c ld.ld_id) ld.ld_name.loc doc)
  
  (*
  and label_declaration =
      {
       ld_id: Ident.t;
       ld_name: string loc;
       ld_mutable: mutable_flag;
       ld_type: core_type;
       ld_loc: Location.t;
       ld_attributes: attributes;
      }
  *)
  
    and constructor_declaration c cd =
      let doc = Doc.get cd.cd_attributes in
      (KConstructor, iname cd.cd_id),
      expr & EConstructor (def (o_dot c cd.cd_id) cd.cd_name.loc doc)
  
  (*
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
  *)
  
    and type_extension c te = map (extension_constructor c) te.tyext_constructors
  
  (*
  and type_extension =
    {
      tyext_path: Path.t;
      tyext_txt: Longident.t loc;
      tyext_params: (core_type * variance) list;
      tyext_constructors: extension_constructor list;
      tyext_private: private_flag;
      tyext_attributes: attributes;
    }
  *)
  
    and extension_constructor c ec =
      let doc = Doc.get ec.ext_attributes in
      (KTypext, iname ec.ext_id),
      expr & ETypext (def (o_dot c ec.ext_id) ec.ext_name.loc doc)
  
  (*
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
    | Text_rebind of Path.t * Longident.t loc
  *)
  
    and class_type c ct = 
      match class_type_desc c ct.cltyp_desc with
      | Some x -> x
      | None -> Types.class_type' ct.cltyp_type
      
  (*
  and class_type =
      {
       cltyp_desc: class_type_desc;
       cltyp_type: Types.class_type;
       cltyp_env: Env.t;
       cltyp_loc: Location.t;
       cltyp_attributes: attributes;
      }
  *)
  
    and class_type_desc c = function
      | Tcty_constr _ -> None (* of Path.t * Longident.t loc * core_type list *)
      | Tcty_signature csg -> Some (class_signature c csg)
      | Tcty_arrow (_, _, ct) -> Some (class_type c ct)
      
  (*    
  and class_type_desc =
      Tcty_constr of Path.t * Longident.t loc * core_type list
    | Tcty_signature of class_signature
    | Tcty_arrow of arg_label * core_type * class_type
  *)
  
    and class_signature c csg = concat_map (class_type_field c) csg.csig_fields
  
  (*        
  and class_signature = {
      csig_self : core_type;
      csig_fields : class_type_field list;
      csig_type : Types.class_signature;
    }
  *)
  
    and class_type_field c ctf =
      let doc = Doc.get ctf.ctf_attributes in
      class_type_field_desc c doc ctf.ctf_loc ctf.ctf_desc
  
  (*    
  and class_type_field = {
      ctf_desc: class_type_field_desc;
      ctf_loc: Location.t;
      ctf_attributes: attributes;
    }
  *)
  
    and class_type_field_desc c doc loc = function
      | Tctf_inherit clty -> class_type c clty
      | Tctf_val _ -> []
      | Tctf_method (n, _, _, _) -> (* (string * private_flag * virtual_flag * core_type) *)
          [(KMethod, n),
           expr & EMethod (def (Oide_dot (c, n)) loc doc)]
      | Tctf_constraint _ -> [] (* (core_type * core_type) *)
      | Tctf_attribute _ -> []
          
  (*    
  and class_type_field_desc =
    | Tctf_inherit of class_type
    | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
    | Tctf_method of (string * private_flag * virtual_flag * core_type)
    | Tctf_constraint of (core_type * core_type)
    | Tctf_attribute of attribute
  *)
    and class_declaration c ci =
      let doc = Doc.get ci.ci_attributes in
      let c' = Oide_dot (c, ci.ci_id_name.txt) in
      let meths = class_expr c' ci.ci_expr in (* XXX not sure at all!. XXX default case is wrong! *)
      [ (KClass, ci.ci_id_name.txt),
        expr & EClass (def c' ci.ci_id_name.loc doc, meths)
      ; (KClasstype, ci.ci_id_name.txt),
        expr & EClasstype (def c' ci.ci_id_name.loc doc, meths)
      ; (KType, ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ; (KType, "#" ^ ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ]
  
  (*      
  and class_declaration =
    class_expr class_infos
  *)
        
    and class_description c ci =
      let doc = Doc.get ci.ci_attributes in
      let c' = Oide_dot (c, ci.ci_id_name.txt) in
      let meths = class_type c' ci.ci_expr in (* XXX not sure at all!. XXX default case is wrong! *)
      [ (KClass, ci.ci_id_name.txt),
        expr & EClass (def c' ci.ci_id_name.loc doc, meths)
      ; (KClasstype, ci.ci_id_name.txt),
        expr & EClasstype (def c' ci.ci_id_name.loc doc, meths)
      ; (KType, ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ; (KType, "#" ^ ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ]
      
  (*
  and class_description =
    class_type class_infos
  *)
  
    and class_type_declaration c ci =
      let doc = Doc.get ci.ci_attributes in
      let c' = Oide_dot (c, ci.ci_id_name.txt) in
      let meths = class_type c' ci.ci_expr in (* XXX not sure at all!. XXX default case is wrong! *)
      [ (KClasstype, ci.ci_id_name.txt),
        expr & EClasstype (def c' ci.ci_id_name.loc doc, meths)
      ; (KType, ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ; (KType, "#" ^ ci.ci_id_name.txt),
        expr & EType (def c' ci.ci_id_name.loc doc, [])
      ]
  
  (*
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
  
  *)
  
  end
end

let types_signature =
  let module M = Make(struct
    let top_path = None
    let ambiguous_docstrings = Ocamldoc.DocSet.empty
  end) in
  Reset.typing ();
  M.Types.signature

let signature p s =
  let module M = Make(struct
    let top_path = Some p
    let ambiguous_docstrings =
      let ambs =
        snd
        & Ocamldoc.partition_ok_and_ambiguous
        & Ocamldoc.extract_signature s
      in
      Ocamldoc.warn_ambiguous ambs;
      ambs
  end) in
  Reset.typing ();
  M.Typedtree.signature p s

let structure p s =
  let module M = Make(struct
    let top_path = Some p
    let ambiguous_docstrings =
      let ambs =
        snd
        & Ocamldoc.partition_ok_and_ambiguous
        & Ocamldoc.extract_structure s
      in
      Ocamldoc.warn_ambiguous ambs;
      ambs
  end) in
  Reset.typing ();
  M.Typedtree.structure p s
