
open Spotlib.Spot
open List
open Outcometree

module String = struct
  let cache = Hashtbl.create 107
  let reset () = Hashtbl.clear cache
  let hcons (s : string) = Hashtbl.find_or_add id cache s
end

module Lexing = struct
  open Lexing

  let cache = Hashtbl.create 107
  let reset () = Hashtbl.clear cache
  let position p =
    try Hashtbl.find cache p with Not_found ->
      let p' = { p with pos_fname = String.hcons p.pos_fname } in
      Hashtbl.add cache p' p';
      p'
end

module Location = struct
  open Location

  let cache = Hashtbl.create 107
  let reset () = Hashtbl.clear cache
  let t l =
    try Hashtbl.find cache l with Not_found ->
      let l' = { l with loc_start = Lexing.position l.loc_start;
                        loc_end   = Lexing.position l.loc_end;
               }
      in
      Hashtbl.add cache l' l';
      l'
end

module Out_ident = struct
  let cache = Hashtbl.create 107
  let reset () = Hashtbl.clear cache
  let rec hcons i =
    let f = function
      | Oide_apply(i1,i2) -> Oide_apply(hcons i1, hcons i2)
      | Oide_dot (i,s) -> Oide_dot(hcons i, String.hcons s)
      | Oide_ident s -> Oide_ident (String.hcons s)
    in
    try Hashtbl.find cache i with Not_found ->
      let i = f i in
      Hashtbl.add cache i i;
      i
end

module Out_type = struct
  let cache = Hashtbl.create 107
  let cache_out_variant = Hashtbl.create 107
  let cache_out_attribute = Hashtbl.create 107
  let reset () =
    Hashtbl.clear cache;
    Hashtbl.clear cache_out_variant;
    Hashtbl.clear cache_out_attribute
  let rec hcons t =
    let f t = match t with
      | Otyp_abstract -> t
      | Otyp_open -> t
      | Otyp_alias (t, s) -> Otyp_alias (hcons t, String.hcons s)
      | Otyp_arrow (s, t1, t2) -> Otyp_arrow (String.hcons s, hcons t1, hcons t2)
      | Otyp_class (b, i, ts) ->
          Otyp_class (b, Out_ident.hcons i, map hcons ts)
      | Otyp_constr (i, ts) ->
          Otyp_constr (Out_ident.hcons i, map hcons ts)
      | Otyp_manifest (t1, t2) -> Otyp_manifest (hcons t1, hcons t2)
      | Otyp_object (meths, bo) ->
          Otyp_object (map (fun (s,t) -> String.hcons s, hcons t) meths, bo)
      | Otyp_record fields ->
          Otyp_record (map (fun (s,b,t) -> String.hcons s, b, hcons t) fields)
      | Otyp_stuff s -> Otyp_stuff (String.hcons s)
      | Otyp_sum xs ->
          Otyp_sum (map (fun (s,ts,top) ->
            String.hcons s, map hcons ts, Option.fmap hcons top) xs)
      | Otyp_tuple ts -> Otyp_tuple (map hcons ts)
      | Otyp_var (b, s) -> Otyp_var (b, String.hcons s)
      | Otyp_variant (b1, v, b2, sso) ->
          Otyp_variant (b1, out_variant_hcons v, b2,
                        Option.fmap (map String.hcons) sso)
      | Otyp_poly (ss,t) ->
          Otyp_poly (map String.hcons ss, hcons t)
      | Otyp_module (s, ss, ts) ->
          Otyp_module (String.hcons s, map String.hcons ss, map hcons ts)
      | Otyp_attribute (t, a) ->
          Otyp_attribute (hcons t, out_attribute_hcons a)
    in
    try Hashtbl.find cache t with Not_found ->
      let t = f t in
      Hashtbl.add cache t t;
      t

  and out_variant_hcons v =
    let f v = match v with
      | Ovar_fields fields ->
          Ovar_fields (map (fun (s,b,ts) ->
            String.hcons s, b, map hcons ts) fields)
      | Ovar_typ ty -> Ovar_typ (hcons ty)
    in
    try Hashtbl.find cache_out_variant v with Not_found ->
      let v = f v in
      Hashtbl.add cache_out_variant v v;
      v

  and out_attribute_hcons a =
    let f { oattr_name= n } = { oattr_name = String.hcons n } in
    try Hashtbl.find cache_out_attribute a with Not_found ->
      let a = f a in
      Hashtbl.add cache_out_attribute a a;
      a
end

module Hump_v = struct
  let cache = Hashtbl.create 107
  let reset () = Hashtbl.clear cache

  let rec hump_v v =
    try Hashtbl.find cache v with Not_found ->
      let v'  = match v with
        | Hump.Def { kind; path= p; loc= l; digest= dopt; doc } ->
            Hump.Def { kind
                     ; path= Out_ident.hcons p
                     ; loc= Location.t l
                     ; digest= Option.fmap String.hcons dopt
                     ; doc= Option.fmap String.hcons doc
                     }
        | Hump.Prim n -> Prim (String.hcons n)
        | Aliased (v1, v2) -> Aliased (hump_v v1, hump_v v2)
        | Coerced (v1, v2) -> Coerced (hump_v v1, hump_v v2)
        | LocNone -> LocNone
      in
      Hashtbl.add cache v' v';
      v'
end
  
module Hump = struct
  open Hump

  let cache_e = Hashtbl.create 107

  (* XXX unfortunately almost no gain observed.
     Should we also try hash-consing Signatures? 
  *)

  let expr x =
    let f = function
      | EVar(k,s) -> Some (lazy (EVar (k, String.hcons s)))
      | EType(x, []) -> Some (lazy (EType (x, [])))
      | ETypext x -> Some (lazy (ETypext x))
      | EValue x -> Some (lazy (EValue x))
      | EConstructor x -> Some (lazy (EConstructor x))
      | EField x -> Some (lazy (EField x))
      | EMethod x -> Some (lazy (EMethod x))
      | EUnknownPath p -> Some (lazy (EUnknownPath (Out_ident.hcons p)))
      | _ -> None
    in
    match f x with
    | None -> x
    | Some y ->
        try Hashtbl.find cache_e x with Not_found ->
          let y = Lazy.force y in
          Hashtbl.add cache_e y y;
          y

  let reset () =
    Hashtbl.clear cache_e
end
  
module Sig = struct
  let ty = Out_type.hcons

  open Sig

  let rec res = function
    | FModule (m,r) -> FModule(fmodule m, r)
    | FModtype (mo) -> FModtype(Option.fmap fmodule mo)
    | FType (ts, ftk, pf, tyo, r) ->
        FType(map ty ts,
              ftypekind ftk,
              pf,
              Option.fmap ty tyo,
              r)
    | FTypextRaw _ -> assert false
    | FTypext(ts, t', rto, pf) ->
        FTypext(map ty ts, ty t', Option.fmap ty rto, pf)
    | FValue (t, svk) -> FValue (ty t, svk)
    | FRecordField (mf, t) -> FRecordField(mf, ty t)
    | FVariantConstructorRaw _ -> assert false
    | FVariantConstructor (t, rto) ->
        FVariantConstructor (ty t, Option.fmap ty rto)
    | FClass (ts, fsig, t, path, (vf, r)) ->
        FClass (map ty ts, fsignature fsig, ty t, Out_ident.hcons path, (vf, r))
    | FClassType (ts, fsig, t, p, (vf, r)) ->
        FClassType (map ty ts, fsignature fsig, ty t, Out_ident.hcons p, (vf, r))
    | FMethod t -> FMethod (ty t)

  and fsignature xs =
    mapi (fun i ((k,p),r) -> 
      let res = ((k,Out_ident.hcons p), res r) in
      if (i+1) mod 1000 = 0 then !!% "%d sigitems done@." (i+1);
      res
    ) xs
      
(*
  and fconstructor_arguments = function
    | FCRecord fields ->
        FCRecord (map (fun (id, mf, t) -> Out_ident.hcons id, mf, ty t) fields)
    | FCTuple ts -> FCTuple (map ty ts)
*)

  and fmodule = function
    | FFunctor (id, fmo, fm) ->
        FFunctor (Out_ident.hcons id, Option.fmap fmodule fmo, fmodule fm)
    | FSignature fsig -> FSignature (fsignature fsig)
    | FUNKNOWN_ident p -> FUNKNOWN_ident (Out_ident.hcons p)
    | FUNKNOWN_alias p -> FUNKNOWN_alias (Out_ident.hcons p)

  and ftypekind = function
    | (FAbstract | FOpen as x) -> x
    | FRecord fsig -> FRecord (fsignature fsig)
    | FVariant fsig -> FVariant (fsignature fsig)
end

module Alias = struct
  let cache = Hashtbl.create 107

  let hcons x =
    try Hashtbl.find cache x with Not_found ->
      let x = match x with
        | Data.Primitive p -> Data.Primitive (String.hcons p)
        | Path (k,p) -> Path (k, Out_ident.hcons p)
      in
      Hashtbl.add cache x x;
      x

  let reset () =
    Hashtbl.clear cache
end
  
let string     = String.hcons
let out_ident  = Out_ident.hcons
let out_type   = Out_type.hcons
let fsignature = Sig.fsignature
let location_t = Location.t
let hump_v     = Hump_v.hump_v
let hump_expr  = Hump.expr

let sigfile {Data.SigFile.name; packs; stamp} =
  let name = String.hcons name in 
  let packs = flip map packs & fun (o,fsig) ->
    (Out_ident.hcons o, fsignature fsig)
  in
  let stamp = map string stamp in
  { Data.SigFile.name; packs; stamp }
  
let humpfile {Data.HumpFile.name; humps; stamp } =
  let name = String.hcons name in 
  let humps = flip map humps & fun (o,e) ->
    (Out_ident.hcons o, Hump.expr e)
  in
  let stamp = map string stamp in
  { Data.HumpFile.name; humps; stamp }
  
let dat_t {Data.Dat.name; items; stamp; version } =
  let name = String.hcons name in 
  let items = flip map items & fun ((k,i),(res,io,vo)) ->
    ((k,Out_ident.hcons i),
     (Sig.res res, Option.fmap Alias.hcons io, Option.fmap Hump_v.hump_v vo))
  in
  let stamp = map string stamp in
  let version = Option.fmap string version in
  { Data.Dat.name; items; stamp; version }
  
let reset () =
  String.reset ();
  Out_ident.reset ();
  Out_type.reset ();
  Lexing.reset ();
  Location.reset ();
  Hump.reset ();
  Hump_v.reset ();
  Alias.reset ()
