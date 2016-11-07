(* Force linking of Mtype:

  if false then ignore (Mtype.scrape Env.empty (Mty_signature []));

is not good since the expression is removed by ocamlopt including the reference
to Mtype.
    
*)
let () = ignore (Mtype.strengthen == Mtype.strengthen)
    

open Spotlib.Spot
open Asttypes
open Types
open Utils
open List
open Sig

type type_expr = Types.type_expr
let ocaml_of_type_expr ty = Ocaml.String (Format.sprintf "%a" Printtyp.type_scheme ty)
    
(** First step, scrape everything.

   Idents and Paths are scraped as they are.
*)
module Scrape = struct
  type k = 
      | KValue     
      | KType      
      | KTypext
      | KModule    
      | KModtype
      | KClass
      | KClassType
    [@@deriving conv{ocaml_of}]
  
  type res = 
    | SModule of smodule * rec_status
    | SModtype of smodule option
    | SType of
        type_expr list (* TODO variance *)
      * stypekind
      * private_flag
      * type_expr option (* manifest *)
      * rec_status
    | STypext of
        Path.t (* Target type *)
      * type_expr list (* params *)
      * sconstructor_arguments 
      * type_expr option (* return type *)
      * private_flag
    | SValue of type_expr * value_kind
    | SClass of
        type_expr list (* params TODO variance *)
      * type_expr (* object type: method-type list *) 
      * type_expr (* [new c]'s type *)
      * Path.t (* class path? *)
      * virtual_flag
      * rec_status
    | SClassType of
        type_expr list (* params TODO variance *)
      * type_expr (* object type: method-type list *) 
      * type_expr (* [new c]'s type *)
      * Path.t (* class path? *)
      * virtual_flag
      * rec_status

  and stypekind =
    | SAbstract
    | SOpen
    | SRecord of (Ident.t * mutable_flag * type_expr) list
    | SVariant of (Ident.t
                   * sconstructor_arguments
                   * type_expr option (* GADT return type *) ) list
  
  and sconstructor_arguments =
    | SCRecord of (Ident.t * mutable_flag * type_expr) list
    | SCTuple of type_expr list
  
  and ssignature = ((k * Ident.t) * res) list
  
  and smodule =
    | SFunctor of Ident.t * smodule option * smodule
    | SSignature of ssignature
    | SUNKNOWN_ident of Path.t (** scrape failed Mty_ident *)
    | SUNKNOWN_alias of Path.t (** scrape failed Mty_alias *)
  
  [@@deriving conv{ocaml_of}]

  let warned_scrape_failures = ref []

  let rec signature env sg = rev & snd & flip2 fold_left (env,[]) sg & fun (env,rev_is) i ->
    let env, i = signature_item env i in
    env,i::rev_is 
  
  and signature_item env i =
    let env = Env.add_item i env in
    env,
    match i with
    | Sig_value (id, vdesc) ->
        (KValue, id), value_description vdesc
    | Sig_type (id, td, rec_status) ->
        (KType, id), type_declaration td rec_status 
    | Sig_typext (id, ec, _ext_status) ->
        (KTypext, id), extension_constructor ec
    | Sig_module (id, md, rec_status) ->
        (KModule, id), module_declaration env md rec_status
    | Sig_modtype (id, mtd) ->
        (KModtype, id), modtype_declaration env mtd
    | Sig_class (id, cd, rec_status) ->
        (KClass, id), class_declaration cd rec_status
    | Sig_class_type (id, ctd, rec_status) ->
        (KClassType, id), class_type_declaration ctd rec_status
        
  and value_description vd = SValue (vd.val_type, value_kind vd.val_kind)
  
  (*
  type value_description =
    { val_type: type_expr;                (* Type of the value *)
      val_kind: value_kind;
      val_loc: Location.t;
      val_attributes: Parsetree.attributes;
     }
  *)
  
  and value_kind = function
    | Val_reg    -> SVal_reg
    | Val_prim _ -> SVal_prim
    | _ -> assert false
  
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
  
  and type_declaration td rec_status =
    SType ( td.type_params, (* TODO variance td.type_variance *)
            type_kind td.type_kind,
            td.type_private,
            td.type_manifest,
            rec_status
          )
           
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
    | Type_abstract        -> SAbstract
    | Type_record (lds, _) -> SRecord (map label_declaration lds)
    | Type_variant cds     -> SVariant (map constructor_declaration cds)
    | Type_open            -> SOpen
  
  (*
      
  and type_kind =
      Type_abstract
    | Type_record of label_declaration list  * record_representation
    | Type_variant of constructor_declaration list
    | Type_open
  
  *)
  
  and label_declaration ld = ld.ld_id, ld.ld_mutable, ld.ld_type

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
  
  and constructor_declaration cd =
    (cd.cd_id, constructor_arguments cd.cd_args, cd.cd_res)
  
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
  
  and constructor_arguments = function
    | Cstr_tuple tys  -> SCTuple tys
    | Cstr_record lds -> SCRecord (map label_declaration lds)
  
  (*    
  and constructor_arguments =
    | Cstr_tuple of type_expr list
    | Cstr_record of label_declaration list
  *)
  
  and extension_constructor ec =
    STypext ( ec.ext_type_path
            , ec.ext_type_params
            , constructor_arguments ec.ext_args
            , ec.ext_ret_type
            , ec.ext_private
            )
      
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

(*          
  type class_type =
      Cty_constr of Path.t * type_expr list * class_type
    | Cty_signature of class_signature
    | Cty_arrow of arg_label * type_expr * class_type
*)

  and class_signature cs = cs.csig_self
    (* we ignore cs.csig_vars *)

(*
               sort (fun (k1,_) (k2,_) -> compare k1 k2)
               & Vars.bindings cs.csig_vars,
               sort compare & Concr.elements cs.csig_concr,
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

  and class_declaration cd rec_status = 
    let oty, newty = class_type cd.cty_type in
    SClass ( cd.cty_params,            (* TODO variance *)
             oty,
             newty,
             cd.cty_path,
             (if cd.cty_new = None then Virtual else Concrete),
             rec_status)
      
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

  and class_type_declaration ctd rec_status =
    let virt =
      let sign = Ctype.signature_of_class_type ctd.clty_type in
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.csig_self) in
      List.exists
        (fun (lab, _, _ty) ->
          not (lab = Btype.dummy_method || Concr.mem lab sign.csig_concr))
        fields
      || Vars.fold (fun _ (_,vr,_) b -> vr = Virtual || b) sign.csig_vars false
    in
    let oty, newty= class_type ctd.clty_type in
    SClassType (ctd.clty_params,  (* TODO variance *)
                (* class_type ctd.clty_type,*)
                oty,
                newty,
                ctd.clty_path,
                (if virt then Virtual else Concrete),
                rec_status)
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
        begin match add_if_not_mem p warned_scrape_failures with
        | `NewlyAdded -> 
            !!% "Warning: scraping failure of Mty_ident: %s@." (Path.string_of p);
            (* Dumpsource.dump_it (); *)
        | _ -> ()
        end;
        SUNKNOWN_ident p
    | Mty_alias p ->
        begin match add_if_not_mem p warned_scrape_failures with
        | `NewlyAdded ->
            !!% "Warning: scraping failure of Mty_alias: %s@." (Path.string_of p);
            (* Dumpsource.dump_it (); *)
        | _ -> ()
        end;
        SUNKNOWN_alias p
    | Mty_signature sg -> SSignature (signature env sg)
    | Mty_functor (id, mtyo, mty) ->
        let env' = match mtyo with
          | None -> env
          | Some mty -> Env.add_module ~arg:true id mty env
        in
        let mtyo = Option.fmap (module_type env) mtyo in
        let mty = module_type env' mty in
        SFunctor (id, mtyo, mty)
  (*
  
  type module_type =
      Mty_ident of Path.t
    | Mty_signature of signature
    | Mty_functor of Ident.t * module_type option * module_type
    | Mty_alias of Path.t
  
  *)
  
  and module_declaration env md rec_status =
    SModule (module_type env md.md_type, rec_status)
  
  (*
  and module_declaration =
    {
      md_type: module_type;
      md_attributes: Parsetree.attributes;
      md_loc: Location.t;
    }
    *)

  and modtype_declaration env md = SModtype (Option.fmap (module_type env) md.mtd_type)

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

  let format = Ocaml.format_with ocaml_of_ssignature
  
  let signature sg =
    warned_scrape_failures := []; 
    signature sg
end
  
(** Second step: Flatten

    Flatten the tree structure.  Types, idents and paths are kept as they are
*)
module Flatten = struct

  include Sig.Flatten(struct
    type type_expr = Types.type_expr
    let ocaml_of_type_expr = ocaml_of_type_expr
    type path = Path.t
    let ocaml_of_path = Path.ocaml_of_t
    type ident = Ident.t
    let ocaml_of_ident = Ident.ocaml_of_t
  end)
    
  open Scrape
  open Path

  let rec stypekind dty = function
    | SAbstract -> FAbstract
    | SOpen -> FOpen
    | SRecord fields ->
        FRecord (flip map fields & fun (id, mf, ty) ->
          (Sig.KField, Pident id),
          FRecordField(
            mf,
            Btype.newgenty (Tarrow (Nolabel, dty, ty, Cok))))
    | SVariant cs ->
        FVariant (flip map cs & fun (id, scas, tyo) ->
          let ty = tyo // dty in
          (KConstructor, Pident id),
          FVariantConstructorRaw(
            sconstructor_arguments scas,
            ty,
            tyo))

  and sconstructor_arguments = function
    | SCRecord id_mf_ty_list -> FCRecord id_mf_ty_list
    | SCTuple tys -> FCTuple tys
          
  and smodule = function
    | SSignature ssg ->
        let fsg, items = ssignature ssg in
        let x = FSignature fsg in
        x, items
    | SFunctor (id, smo, sm) ->
        let fm, items = smodule sm in
        let x = FFunctor (id, Option.fmap (fst *< smodule) smo, fm) in
        x, items
    | SUNKNOWN_ident p -> FUNKNOWN_ident p, []
    | SUNKNOWN_alias p -> FUNKNOWN_alias p, []

  and ssignature kid_res_list =
    let x_items_list = flip map kid_res_list & function
       | (Scrape.KValue, id), SValue (ty, skv) ->
           let x = (Sig.KValue, Pident id), FValue (ty, skv) in
           x, [x]
       | (KType, id), SType (pars, stk, pf, tyo, r) ->
           let dty = Btype.newgenty (Tconstr (Pident id, pars, ref Mnil)) in
           let ftk = stypekind dty stk in
           let x = (Sig.KType, Pident id), FType (pars, ftk, pf, tyo, r) in
           x, x :: begin match ftk with
                   | FRecord rs | FVariant rs -> rs
                   | FAbstract | FOpen -> []
                   end
       | (KTypext, id), STypext (path, pars, scas, tyo, pf) ->
          (* [path] is a type name, so be careful of predef and globals *)
           let ty = Btype.newgenty (Tconstr (path, pars, ref Mnil)) in
           let dty = tyo // ty in
           let x = (Sig.KTypext, Pident id), FTypextRaw (pars, sconstructor_arguments scas, dty, tyo, pf) in
          x, [x]
       | (KModule, id), SModule (sm, r) ->
          let fm, items = smodule sm in
          let x = (Sig.KModule, Pident id), FModule (fm, r) in
          x, x :: items     
       | (KModtype, id), SModtype smo ->
           (* CR jfuruse: We do not flatten members of modtype. It is ok? *)
           let fmo  = match smo with
             | None -> None
             | Some sm -> let fm, _items = smodule sm in Some fm
           in
           let x = (Sig.KModtype, Pident id), FModtype fmo in
           x, [x]
       | (KClass, id), SClass (params, oty, newty, path, vf, rec_status) ->
           let meths =
             let fields, _ = Ctype.(flatten_fields & object_fields oty) in
             flip filter_map fields & fun (n, _fk, ty) ->
               if n = "*dummy method*" then None
               else Some ((Sig.KMethod, Pident (Ident.create n)), FMethod ty)
           in
           let x = (Sig.KClass, Pident id), FClass (params, meths, newty, path, (vf, rec_status)) in
           x, x :: meths
       | (KClassType, id), SClassType (params, oty, newty, path, vf, rec_status) ->
           (* CR jfuruse: we do not count members of class type *)
           let meths =
             let fields, _ = Ctype.(flatten_fields & object_fields oty) in
             flip filter_map fields & fun (n, _fk, ty) ->
               if n = "*dummy method*" then None
               else Some ((Sig.KMethod, Pident (Ident.create n)), FMethod ty)
           in
           let x = (Sig.KClasstype, Pident id), FClassType (params, meths, newty, path, (vf, rec_status)) in
           x, [x]
       | kid, res ->
           !!% "@[<2>ssignature is not implemented for %a :@ %a@]@."
             (Ocaml.format_with [%derive.ocaml_of: k * Utils.Ident.t]) kid
             (Ocaml.format_with [%derive.ocaml_of: Scrape.res]) res;
           assert false
    in
    map fst x_items_list,
    concat_map snd x_items_list
end
  
let pdot c id = match c with
  | None -> Path.Pident id
  | Some p -> Pdot (p,id.Ident.name,0)

let papply c id = match c with
  | None -> assert false
  | Some p -> Path.Papply (p, Pident id)

(** Scan Ident.t's and record their global access Path.t.
    (Some Ident.t's are not reallay accessible by their global access Path.t
    since they may be hidden by signatures)
*)
module Scan_ids = struct
  open Flatten
  open Path
    
  module Make(A : sig val tbl : (Ident.t, Path.t) Hashtbl.t end) = struct
    include A

    (* Register [id] is Globally accessible as [p] *)
    let add id p = 
      match Hashtbl.find_opt tbl id with
      | None -> Hashtbl.add tbl id p
      | Some _ ->
(*          !!% "Warning double binding of %s@." id.Ident.name;
*)
          ()
        
    let rec fsignature c s = iter (fsignature_item c) s
  
    and fsignature_item c ((_k,p), v) =
      let id = match p with
        | Pident id -> id
        | _ -> assert false
      in
      let p = pdot c id in
      add id p;
      let c' = Some p in
      match v with
      | FModule (m,_) -> fmodule c' m
      | FType (_, ftk, _, _, _) -> ftypekind c ftk
      | FClass (_, fsg, _newty, _p, (_vf(* _tyo, *), _r)) -> (* CR jfuruse: p is a type?!?!? *)
          fsignature c' fsg;
      (* We really need context extension for modtype and class type? *)
      | FModtype (Some m) -> fmodule c' m
      | FClassType (_, fsg, _newty, _p, (_vf, _r)) -> (* CR jfuruse: p is a type?!?!? *)
          fsignature c' fsg
      | _ -> ()
  
    and fmodule c = function
      | FFunctor (id, None, m) ->
          (* c(id) for body *)
          add id (Pident id);
          fmodule (Some (papply c id)) m
      | FFunctor (id, Some mty, m) ->
          (* c.id for arg,  c.(id) for body*)
          add id (Pident id);
          fmodule (Some (pdot c id)) mty;
          fmodule (Some (papply c id)) m
      | FSignature s -> fsignature c s
      | FUNKNOWN_ident _ | FUNKNOWN_alias _ -> ()

    and ftypekind c = function
      | FAbstract -> ()
      | FOpen -> ()
      | FRecord fsg | FVariant fsg -> fsignature c fsg
  end

  (* [p] is the global access path to the module *)
  let scan po s =
    let module M = Make(struct let tbl = Hashtbl.create 107 end) in
    M.fsignature po s;
    M.tbl

  let rec rewrite tbl = function
    | Pident id      -> Hashtbl.find tbl id
    | Pdot (p, n, i) -> Pdot(rewrite tbl p, n, i)
    | Papply(p1, p2) -> Papply (rewrite tbl p1, rewrite tbl p2)

  let rewrite tbl p =
    try rewrite tbl p with Not_found ->
      !!% "Path %s was not found in rewrite table@." (Path.string_of p);
      raise Not_found
end

(** Replace Path.t in the given flattened signature by
    their global access paths.

    Path.t and Ident.t => out_ident
    type_expr => out_type
*)
module Globalized = struct

  module F = Flatten

  module Print = struct
    open Format
    open Outcometree

    let simplif_type path ty =
      let rec f t = match t with
        | Otyp_abstract -> t
        | Otyp_open -> t
        | Otyp_alias (t, s) -> Otyp_alias (f t, s)
        | Otyp_arrow (s, t1, t2) -> Otyp_arrow (s, f t1, f t2)
        | Otyp_class (b, i, ts) -> Otyp_class (b, path i, map f ts)
        | Otyp_constr (i, ts) -> Otyp_constr (path i, map f ts)
        | Otyp_manifest (t1, t2) -> Otyp_manifest (f t1, f t2)
        | Otyp_object (xs, bo) -> Otyp_object (map (fun (s,t) -> (s,f t)) xs, bo)
        | Otyp_record fs -> Otyp_record (map (fun (s,b,t) -> (s,b,f t)) fs)
        | Otyp_stuff _ -> t
        | Otyp_sum xs ->
            Otyp_sum (map (fun (s,ts,topt) -> (s,map f ts,Option.fmap f topt)) xs)
        | Otyp_tuple ts -> Otyp_tuple (map f ts)
        | Otyp_var (b,s) -> Otyp_var (b,s)
        | Otyp_variant (b, ov, b', sso) ->
            Otyp_variant (b, variant ov, b', sso)
        | Otyp_poly (ss, t) -> Otyp_poly (ss, f t)
        | Otyp_module (s, ss, ts) -> Otyp_module (s, ss, map f ts)
        | Otyp_attribute (t, a) -> Otyp_attribute (f t, a)
      and variant = function
        | Ovar_fields fs ->
            Ovar_fields (map (fun (s,b,ts) -> (s,b,map f ts)) fs)
        | Ovar_name (i, ts) -> Ovar_name (path i, map f ts)
      in
      f ty
      
    module Make(A : sig val path_simplifier : out_ident -> out_ident end) = struct
      let simplif_type = simplif_type A.path_simplifier

      (* We cannot do partial application since sprintf has side-effect inside! *)
      let string_of_ident x = sprintf "%a" Xoprint.print_ident x
      let string_of_type x = sprintf "%a" !Xoprint.out_type & simplif_type x 
  
      let rec fmodule rec_ (* recursively prints internals *) = function
        | FSignature _ when not rec_ -> Omty_signature [Osig_ellipsis]
        | FSignature fs -> Omty_signature (fsignature rec_ fs)
        | FFunctor (id, fmo, fm') ->
            Omty_functor (string_of_ident id, Option.fmap (fmodule rec_) fmo, fmodule rec_ fm')
        | FUNKNOWN_ident path -> Omty_ident path
        | FUNKNOWN_alias path -> Omty_alias path
  
      and fsignature rec_ fs = filter_map (fsignature_item rec_) fs
  
      and rec_status = function
        | Trec_not -> Orec_not
        | Trec_first -> Orec_first
        | Trec_next -> Orec_next
  
      and ftypekind rec_ = function
        | FAbstract -> Otyp_abstract
        | FOpen -> Otyp_open

        | FRecord fs when rec_ -> 
            Otyp_record (flip map fs & function
              | ((_,path), FRecordField (mf, Otyp_arrow ("", _, t))) ->
                  let n = match path with
                    | Oide_ident n -> n
                    | Oide_dot (_,n) -> n
                    | Oide_apply _ -> assert false
                  in
                  (n, mf = Mutable, simplif_type t)
              | _ -> assert false)
        | FRecord _fs -> Otyp_stuff "{ ... }"
              
        | FVariant fs when rec_ -> 
            Otyp_sum (flip map fs & function
              | ((_,path), FVariantConstructor (Otyp_arrow ("", t, _), rto)) ->
                  let n = match path with
                    | Oide_ident n -> n
                    | Oide_dot (_,n) -> n
                    | Oide_apply _ -> assert false
                  in
                  (n,
                   map simplif_type 
                   begin match t with
                   | Otyp_record _ -> [t]
                   | Otyp_tuple ts -> ts
                   | _ -> assert false
                   end, 
                   Option.fmap simplif_type rto)
              | ((_,path), FVariantConstructor (_, rto)) -> (* 0-ary *)
                  (string_of_ident path,
                   [], 
                   Option.fmap simplif_type rto)
              | _, f ->
                  !!% "%a@." (Ocaml.format_with [%derive.ocaml_of : Sig.res]) f;
                  assert false)
        | FVariant _fs -> Otyp_stuff "| ..."
            
  
      and fsignature_item rec_ ((_k,path), res) = match res with
        | FModule (fm, r) ->
            Some (Osig_module (string_of_ident path, fmodule rec_ fm, rec_status r))
        | FModtype None -> 
            Some (Osig_modtype (string_of_ident path, Omty_abstract))
        | FModtype (Some fm) -> 
            Some (Osig_modtype (string_of_ident path, fmodule rec_ fm))
        | FType (tys, ftk, pf, tyo, r) -> 
            let otd = 
              { otype_name = string_of_ident path
              ; otype_params = 
                  map (function
                    | Otyp_var (_, a) -> a, (true, true) (* TODO *)
                    | _ -> "?", (true, true)) tys
              ; otype_type = begin
                let fty = ftypekind rec_ ftk in
                simplif_type &
                Option.fmap (fun t -> Otyp_manifest (t,fty)) tyo // fty
              end
              ; otype_private = pf 
              ; otype_immediate = false (* TODO *)
              ; otype_cstrs = [] (* TODO constraints *)
              }
            in
            Some (Osig_type (otd, rec_status r))
        | FTypextRaw _ -> assert false
        | FTypext (ts, t', rto, pf) ->
            let args, rt  = match t' with
              | Otyp_arrow ("", ts, rt) ->
                  begin match ts with
                  | Otyp_tuple ts -> ts, rt
                  | Otyp_record _ -> [ts], rt
                  | _ -> assert false
                  end
              | _ -> assert false
            in
            let oext_type_name = match rt with
              | Otyp_constr (oi, _) -> string_of_ident oi
              | _ -> assert false
            in
            let oec =
              { oext_name        = string_of_ident path
              ; oext_type_name
              ; oext_type_params = map string_of_type ts
              ; oext_args        = map simplif_type args
              ; oext_ret_type    = Option.fmap simplif_type rto
              ; oext_private     = pf }
            in
            Some (Osig_typext (oec, Oext_first)) (* /Oext_next/Oext_exception*)
  
        | FValue (t, sk) ->
            Some (Osig_value { oval_name = string_of_ident path
                             ; oval_type = simplif_type t
                             ; oval_attributes = []
                             ; oval_prims = match sk with
                                            | SVal_prim -> ["dummy"]  (* will not be printed *)
                                            | SVal_reg -> []
                             })
        | FRecordField _ -> None
        | FVariantConstructorRaw _ -> assert false
        | FVariantConstructor _ -> None
        | FMethod _ -> None
        | FClass (tys, _fs, t, _p, (vf, r)) -> 
            let clt = 
              let rec f = function
                | Otyp_arrow (s, t1, t2) -> Octy_arrow (s, t1, f t2)
                | Otyp_object (meths, _ (* CR jfuruse: todo None: closed, Some true: _.., Some false: .. *) ) ->
                    (* CR jfuruse: we should use fs instead of t? *)
                    Octy_signature (None (* CR jfuruse: self_ty*), 
                                    flip map meths & fun (s,t) ->
                                      Ocsg_method (s, true (*?*), true(*?*), t))
                | Otyp_alias (t, a) ->
                    (* not sure... *)
                    begin match f t with
                    | Octy_signature (None, xs) ->
                        Octy_signature (Some (Otyp_var (false, a)), xs)
                    | _ -> assert false
                    end
                | t -> 
                    !!% "?!?!: %s@." (string_of_type t);
                    assert false
              in
              f & simplif_type t
            in
            Some (Osig_class (vf = Virtual,
                              string_of_ident path,
                              map (fun t -> string_of_type t, (true, true) (* TODO *)) tys,
                              clt,
                              rec_status r))
        | FClassType (pars, _fs, t, _p, (vf, r)) ->
            let clt = 
              let rec f = function
                | Otyp_arrow (s, t1, t2) -> Octy_arrow (s, t1, f t2)
                | Otyp_object (meths, _ (* CR jfuruse: todo None: closed, Some true: _.., Some false: .. *) ) ->
                    (* CR jfuruse: we should use fs instead of t? *)
                    Octy_signature (None (* CR jfuruse: self_ty*), 
                                    flip map meths & fun (s,t) ->
                                      Ocsg_method (s, true (*?*), true(*?*), t))
                | Otyp_alias (t, a) ->
                    (* not sure... *)
                    begin match f t with
                    | Octy_signature (None, xs) ->
                        Octy_signature (Some (Otyp_var (false, a)), xs)
                    | _ -> assert false
                    end
                | t -> 
                    !!% "?!?!: %s@." (string_of_type t);
                    assert false
              in
              f & simplif_type t
            in
            Some (Osig_class_type (vf = Virtual,
                                   string_of_ident path,
                                   map (fun t -> string_of_type t, (true, true) (* TODO *)) pars,
                                   clt,
                                   rec_status r))
    end

    let path_simplifier k p =
      let rec get_package = function
        | Oide_ident s when s.[0] = '{' -> Some s
        | Oide_ident _ -> None
        | Oide_dot( i, _ ) -> get_package i
        | Oide_apply( i, _ ) -> get_package i
      in
      let package = get_package p in

      (* Prefix requires special handling for methods, since
         they have paths like M.N.O.classname.methodname
      *)
      let prefix = match p with
        | Oide_dot (Oide_dot (i, _), _) when k = KMethod -> Some i
        | Oide_dot (i, _) -> Some i
        | _ -> None
      in
      let rec f = function
        | p when Some p = prefix -> None
        | Oide_ident s when Some s = package -> None
        | Oide_ident _ as i -> Some i
        | Oide_dot( i, x ) ->
            begin match f i with
            | None -> Some (Oide_ident x)
            | Some i -> Some (Oide_dot ( i, x ))
            end
        | Oide_apply( i, x ) as i0 -> 
            begin match f i with
            | None -> (* strange *) Some i0
            | Some i -> Some (Oide_apply ( i, x ))
            end
      in
      fun q ->
        match f q with
        | Some x -> x
        | None -> q

    let fsignature_item rec_ ppf ((k,path), res as si) =
      let module M = Make(struct let path_simplifier = path_simplifier k path end) in
      match M.fsignature_item rec_ si with
      | Some x -> !Xoprint.out_sig_item ppf x
      | None ->
          let print_ident = Xoprint.print_ident in
          let out_type ppf = !Xoprint.out_type ppf *< M.simplif_type in
          match res with
          | FRecordField (Mutable, t) -> 
              fprintf ppf "@[<2>field mutable %a :@ @[%a@]@]"
                print_ident path
                out_type t
          | FRecordField (Immutable, t) -> 
              fprintf ppf "@[<2>field %a :@ @[%a@]@]"
                print_ident path
                out_type t
          | FVariantConstructor (t, _rto) ->
              fprintf ppf "@[<2>constr %a :@ @[%a@]@]"
                print_ident path
                out_type t
          | FMethod t ->
              fprintf ppf "@[<2>method %a :@ @[%a@]@]"
                print_ident path
                out_type t
          | _ -> assert false

  end

  module Make(A : sig
    val rewrite : Path.t -> Path.t
  end) = struct

    include Xprinttyp.Make(A)

    let conv_ty = tree_of_type_scheme
      
    let fconstructor_arguments = function
      | F.FCRecord id_mf_ty_list ->
          Otyp_record (map (fun (id, mf, ty) ->
            id.Ident.name, mf = Mutable, conv_ty ty) id_mf_ty_list)
      | F.FCTuple tys -> Otyp_tuple (map conv_ty tys)
  
    let rec res x = match x with
      | F.FModule (fm,r) -> FModule (fmodule fm, r)
      | F.FModtype fmo -> FModtype (Option.fmap fmodule fmo)
      | F.FType (tys, ftk, pf, tyo, r) ->
          FType (map conv_ty tys, ftypekind ftk, pf, Option.fmap conv_ty tyo, r)
      | F.FTypext _ -> assert false
      | F.FTypextRaw (tys, fcas, ty', rto, pf) ->
          FTypext (map conv_ty tys,
                   (Otyp_arrow ("", fconstructor_arguments fcas, conv_ty ty')),
                   Option.fmap conv_ty rto,
                   pf)
      | F.FValue (ty, svk) -> FValue (conv_ty ty, svk)
      | F.FRecordField (mf, ty) -> FRecordField (mf, conv_ty ty)
      | F.FVariantConstructor _ -> assert false
      | F.FVariantConstructorRaw (fcas, t, rto) ->
          begin match fconstructor_arguments fcas with
          | Otyp_tuple [] ->
              FVariantConstructor (conv_ty t, Option.fmap conv_ty rto)
          | _ -> 
              FVariantConstructor (Otyp_arrow ("", fconstructor_arguments fcas, conv_ty t), Option.fmap conv_ty rto)
          end
      | F.FClass (tys, fsg, newty, p, (vf, (*tyo, *) r)) -> (* CR jfuruse: p is a type?!?!? *)
          FClass (map conv_ty tys, fsignature fsg, conv_ty newty,
                  tree_of_path p, (vf, r))
      | F.FClassType (tys, fsg, newty, p, (vf, r)) -> (* CR jfuruse: p is a type?!?!? *)
          FClassType (map conv_ty tys, fsignature fsg, conv_ty newty,
                      tree_of_path p, (vf, r))
      | F.FMethod ty -> FMethod (conv_ty ty)
  
    and ftypekind = function
      | FAbstract -> FAbstract
      | FOpen -> FOpen
      | FRecord sg -> FRecord (fsignature sg)
      | FVariant sg -> FVariant (fsignature sg)
  
    and fsignature sg = map (fun ((k,p), r) -> (k, tree_of_path p), res r) sg
  
    and fmodule = function
      | FSignature sg -> FSignature (fsignature sg)
      | FFunctor (id, fmo, fm) ->
          FFunctor (Oide_ident id.Ident.name, Option.fmap fmodule fmo, fmodule fm)
      | FUNKNOWN_ident p -> FUNKNOWN_ident (tree_of_path p)
      | FUNKNOWN_alias p -> FUNKNOWN_alias (tree_of_path p)
  end

end

include Globalized

module Rewrite = struct
  open Spotlib.Spot
  open Utils
  open List
  open Path
  
  let notfound = Ident.create "NOTOP"
  
  let warned_rewrite_idents = ref []
  let warned_paths = ref []
  let warned_rewrite_cmis = ref []
      
  let rec rewrite f p = match p with
    | Pident id when Ident.persistent id ->
        let p = match find_global_module id with
          | None -> 
              if add_if_not_mem (id.Ident.name, !Config.load_path) warned_rewrite_idents = `NewlyAdded then
                !!% "@[<2>Warning: rewrite: persistent module %s is not found.@ Load path is@ [ @[%a@] ]@]@." id.Ident.name Format.(list ";@ " string) !Config.load_path;
              (* XXX We fail to find Location for camlp4 *)
              Pident notfound
          | Some p ->
              match Cm.guess p with
              | [] ->
                  if add_if_not_mem p warned_paths = `NewlyAdded then
                    !!% "Warning: package_path: package of %s was not found@." p;
                  Pident notfound
              | [{Cm.ocamlfinds=[]} as cm] ->
                  (* Unreachable 
                     ocaml-4.03.0//tools/depend.ml is used by ocamldoc but
                     never linked to any OCamlFind package (I believe)
                  *)
                  if add_if_not_mem cm.Cm.cmi warned_rewrite_cmis = `NewlyAdded then
                    !!% "Warning: rewrite: module %s is never linked to any OCamlFind packages.@." cm.Cm.cmi;
                  Pident notfound
              | [{Cm.ocamlfinds} as cm] ->
                  begin try
                    Path.Pident (Ident.create & Packpath.make & map snd ocamlfinds)
                  with
                  | e ->
                      !!% "@[<2>Error: %s:@ @[%a@]@]@."
                        p
                        (Ocaml.format_with [%derive.ocaml_of: Cm.t] ) cm;
                      raise e
                  end
              | _ -> assert false
        in
        Pdot(p, id.Ident.name, 0)
    | Pident id when Ident.is_predef id ->
        (* We do not rewrite predef types.  If we replace option => prdef.option.
           Otherwise the printer of optional arguments print things strangely like
           ?label:<hidden> -> ...
        *)
        p
    | Pident _ -> f p
    | Pdot(p, n, i) -> Pdot(rewrite f p, n, i)
    | Papply(p1,p2) -> Papply(rewrite f p1, rewrite f p2)
end
  
let globalize tbl fsig =
  let rewrite = Rewrite.rewrite (Scan_ids.rewrite tbl) in
  let module G = Globalized.Make(struct let rewrite = rewrite end) in
  G.fsignature fsig

let scrape top sg =
  Reset.typing ();
  let env = Env.initial_unsafe_string in
  let res = Scrape.signature env sg in
  let fsig, items = Flatten.ssignature res in
  let tbl = Scan_ids.scan top fsig in

  (* Adding the top module itself *)
  let self =
    flip Option.fmap top & fun top ->
      let top_name = match top with
        | Pident id -> Ident.name id
        | Pdot (_, s, _) -> s
        | _ -> assert false
      in
      Ident.create_persistent top_name 
  in
  let items = match top, self with
    | Some top, Some id ->
        Hashtbl.add tbl id top;
        ((KModule, Path.Pident id), Flatten.FModule (Flatten.FSignature fsig, Trec_not)) :: items
    | None, None -> items
    | _ -> assert false
  in
  
  let items = globalize tbl items in
  !!% "Got %d items@." & length items;

  (* Dupe check *)
  (* XXX We never see a failure... We do not probably need this *)
  let items, dups = uniq_dup_sorted compare & sort compare items in
  if dups <> [] then !!% "DUPS! @[%a@]@." Sig.format (map fst dups);
  (* Dupe check, done *)

  items

let test top sg =
  Reset.typing (); (* also need to call reset_cache_toplevel? *)
  let env = Env.initial_unsafe_string in
  !!% "scraping...@.";
  let res = Scrape.signature env sg in
  !!% "%a@." Scrape.format res;

  !!% "@.Flattening...@.";
  let fsig, items = Flatten.ssignature res in
  !!% "%a@.@." Flatten.format items;

  let tbl = Scan_ids.scan top fsig in
  let items = globalize tbl items in
  !!% "%a@.@." Sig.format items;
  ()
