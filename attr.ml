(** Iterator over the OCaml attributes *)

(*   Only works with 4.03.0. Need to carefully check when porting to 
   newer OCaml releases.
*)

open List
open Typedtree
  
module Make(A : sig val f : attributes -> unit end) = struct
  open A
  open TypedtreeIter

  module Iter = struct
    include DefaultIteratorArgument

    (* The following attributes have no corresponding enter_xxx: 
        typedtree.mli:341:     mb_attributes: attributes;
        typedtree.mli:415:     md_attributes: attributes;
        typedtree.mli:434:     open_attributes: attribute list;
        typedtree.mli:442:     incl_attributes: attribute list;
        typedtree.mli:526:     ld_attributes: attributes;
        typedtree.mli:536:     cd_attributes: attributes;
    *)

    let enter_value_description vd = f vd.val_attributes
    let enter_type_extension tyext = f tyext.tyext_attributes
    let enter_extension_constructor ext = f ext.ext_attributes
    let enter_pattern pat = f pat.pat_attributes
    let enter_expression exp = f exp.exp_attributes
    let enter_module_type_declaration mtd = f mtd.mtd_attributes
    let enter_module_type mty = f mty.mty_attributes
    let enter_module_expr mod_ = f mod_.mod_attributes
    let enter_class_expr cl = f cl.cl_attributes
    let enter_class_declaration ci = f ci.ci_attributes
    let enter_class_description ci = f ci.ci_attributes
    let enter_class_type_declaration ci = f ci.ci_attributes
    let enter_class_type cltyp = f cltyp.cltyp_attributes
    let enter_class_type_field ctf = f ctf.ctf_attributes
    let enter_core_type ctyp = f ctyp.ctyp_attributes
    let enter_class_field cf = f cf.cf_attributes

    let enter_binding vb = f vb.vb_attributes

    let enter_type_declaration typ =
      f typ.typ_attributes;
      match typ.typ_kind with
      | Ttype_abstract | Ttype_open -> ()
      | Ttype_variant cds -> iter (fun cd -> f cd.cd_attributes) cds
      | Ttype_record lds -> iter (fun ld -> f ld.ld_attributes) lds

    let enter_structure_item str =
      match str.str_desc with
      | Tstr_module mb -> f mb.mb_attributes
      | Tstr_recmodule mbs -> iter (fun mb -> f mb.mb_attributes) mbs
      | Tstr_open op -> f op.open_attributes
      | Tstr_include incl -> f incl.incl_attributes
      | _ -> ()

    let enter_signature_item sg =
      match sg.sig_desc with
      | Tsig_module md -> f md.md_attributes
      | Tsig_recmodule mds -> iter (fun md -> f md.md_attributes) mds
      | Tsig_open op -> f op.open_attributes
      | Tsig_include incl -> f incl.incl_attributes
      | _ -> ()

  end

  include MakeIterator(Iter)
end

