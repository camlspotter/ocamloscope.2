open Spotlib.Spot
open List
open Cohttp_lwt_unix

open Tyxml
module H = Html

(*
let to_ocaml = Html.(a ~a:[a_href "ocaml.org"] [pcdata "OCaml!"])
let%html to_ocaml = "<a href='ocaml.org'>OCaml!</a>"
*)

let html_to_string html =
  let b = Buffer.create 1000 in
  let ppf = Format.formatter_of_buffer b in
  Html.pp () ppf html;
  Buffer.contents b

let%html oc_header = {|
  <head>
    <title>OCamlOScope.2 server</title>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="style.css">
  </head>
|}
    
let respond ~status html =
  let headers = Cohttp.Header.of_list ["Content-type", "text/html"] in
  Server.respond_string ~headers ~status ~body:(html_to_string html) ()

let spans ?a s = H.span ?a [ H.pcdata s ]

let query_form pspec (v : string) =
  let open Query.PackageSpec in
  let packs = match pspec with
    | All_but xs -> xs
    | Just xs -> xs
    | Vanilla xs -> xs
  in
  let mk_option v p =
    if p pspec then [H.a_value v; H.a_selected ()]
    else [H.a_value v]
  in
  let open Html in
  [%html {| <div class="query">
              <span class="logo">OC&#x1f441;</span>
              <form action="/" method="get"> |}
                [ pcdata "Query: "; input ~a:[ a_input_type `Text; a_name "q"; a_value v] ()
                ; input ~a:[ a_input_type `Submit; a_style "visibility: hidden;"] () 
                ; br ()
                ; pcdata " Packages: "
                ; select ~a: [ a_name "packtype" ]
                    [ option ~a:(mk_option "vanilla" (function Vanilla _ -> true | _ -> false)) (pcdata "Vanilla and")
                    ; option ~a:(mk_option "allbut" (function All_but _ -> true | _ -> false)) (pcdata "All but")
                    ; option ~a:(mk_option "just" (function Just _ -> true | _ -> false)) (pcdata "Just")
                    ]
                ; pcdata " "
                ; input ~a:[ a_input_type `Text; a_name "packs"; a_value (String.concat " " packs)] () ]
         {|   </form>
            </div> |}
  ]

let hpath p = spans ~a:[H.a_class ["path"]] & Format.sprintf "%a" Xoprint.print_ident p

let fsignature_item i =
  let open Data.DB in
  let open Sig in
  let open Outcometree in
  let module M = Sigext.Print.Make(struct let path_simplifier = Sigext.Print.path_simplifier i.kind i.path end) in

  let string_of_type ty = Format.sprintf "%a" !Xoprint.out_type (M.simplif_type ty) in
  let htype ty = spans ~a:[H.a_class ["type"]] & string_of_type ty in

  let htyparams ?(cls=false) = function
    | [] -> []
    | [x] -> [ htype x; spans " " ]
    | xs when cls -> spans ~a:[H.a_class ["type"]] "[" :: intersperse (spans ", ") (map htype xs) @ [ spans "] " ]
    | xs -> spans ~a:[H.a_class ["type"]] "(" :: intersperse (spans ", ") (map htype xs) @ [ spans ") " ]
  in
    
  let ((k,p),res) = fsignature_item i in
  let hk = spans ~a:[H.a_class ["kind"]] (Sig.string_of_k k ^ " ") in
  let hp = hpath p in
  H.div ~a: [ H.a_class [ "item" ] ] &
  match res with
  | FValue (ty, _) -> [ hk; hp; spans " : "; htype ty ]
  (* | FValue (ty, SVal_prim) -> [ spans "external"; hp; spans " : "; htype ty ] *)
  | FModule _ -> [ hk; hp; spans " : "; spans "sig ... end" ]
  | FModtype None -> [ hk; hp ]
  | FModtype (Some _) -> [ hk; hp; spans " = "; spans "sig ... end" ]
  | FType (pars, tk, pf, aliaso, rs) -> (* Trec_not or other *)
      hk
      :: (match rs with Trec_not -> [ spans "norec" ] | _ -> [])
      @ htyparams pars
      @ [ hp ]
      @ (match aliaso with None -> [] | Some ty -> [ spans " = "; htype ty ])
      @ (match tk with
         | FAbstract -> []
         | FOpen -> [ spans " = "; spans ".." ]
         | FRecord fsig ->
             let field = function
               | ((_, p), FRecordField (Immutable, ty)) ->
                   H.span [ hpath p
                          ; spans " : "
                          ; htype ty
                          ]
               | ((_, p), FRecordField (Mutable, ty)) ->
                   H.span [ spans "mutable "
                          ; hpath p
                          ; spans " : "
                          ; htype ty
                          ]
               | _ -> assert false
             in
             spans " = "
             :: (if pf = Private then [ spans "private" ] else [])
             @ [ spans "{ "; H.div (map field fsig); spans " }" ]
         | FVariant fsig ->
             let constr = function
               | ((_, p), FVariantConstructor (ty, None)) ->
                   begin match ty with
                   | Otyp_arrow (_, ty, _) ->
                       H.span [ spans " | "; hpath p; spans " of "; htype ty ] 
                   | _ ->
                       H.span [ spans " | "; hpath p] 
                   end
               | ((_, p), FVariantConstructor (ty, Some _)) -> (* XXX I guess the return type is integrated into ty already *)
                   H.span [ spans " | "; hpath p; spans " : "; htype ty ] 
               | _ -> assert false
             in
             spans " = "
             :: (if pf = Private then [ spans "private" ] else [])
             @ [ H.div (map constr fsig); ]
        )
  | FRecordField (Immutable, ty) -> [ hk; hp; spans " : "; htype ty ]
  | FRecordField (Mutable, ty) -> [ hk; spans "mutable"; hp; spans " : "; htype ty ]
  
  | FVariantConstructor (ty, None) -> [ hk; hp; spans " : "; htype ty ] 
  | FVariantConstructor (ty, Some _) -> [ hk; hp; spans " : "; htype ty ] (* XXX I guess the retun type is integrated into ty already *)

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
        | Otyp_constr (oi, _) -> hpath oi
        | _ -> assert false
      in
      hk
      :: htyparams ts
      @ [ oext_type_name; spans "+=" ]
      @ (if pf = Private then [ spans "private" ] else [])
      @ [ hp ]
      @ (match rto with
         | None ->
             begin match args with
             | [] -> []
             | _ -> [ spans " of "; htype (Otyp_tuple args) ]
             end
         | Some rty ->
             begin match args with
             | [] -> [ spans " : "; htype rty ]
             | _ -> [ spans " : "; htype (Otyp_arrow ("", Otyp_tuple args, rty)) ]
             end)
  | FClass (tys, _fs, t, _p, (vf, _r)) -> 
(*
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
        f & M.simplif_type t
      in
*)
      hk
      :: (if vf = Virtual then [ spans "virtual" ] else [])
      @ htyparams ~cls:true tys
      @ [hp] (* XXX how abstract class is encoded?? *)
      @ [ spans " : "; htype t ]

  | FClassType (pars, _fs, t, _p, (vf, _r)) ->
(*
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
        f & M.simplif_type t
      in
*)
      hk
      :: (if vf = Virtual then [ spans "virtual" ] else [])
      @ htyparams ~cls:true pars
      @ [hp] (* XXX how abstract class is encoded?? *)
      @ [ spans " = "; htype t ]

  | FMethod ty -> [ hk; hp; spans " : "; htype ty ]
  | FTypextRaw _
  | FVariantConstructorRaw _ -> assert false
    
let print_summary (sum : ( (Sig.k * Data.alias)
                           * int
                           * (int * Data.DB.item * 'trace1 * 'trace2) list list ) list) =
  let group i ((k, alias), _dist, xss) =
    let nonaliased, aliased = partition (fun (_,i,_,_) -> i.Data.DB.alias = None) & flatten xss in
    let nonaliased = match nonaliased with
      | [] ->
          H.div [
            match alias with
            | Data.Primitive n ->
                H.div [ spans ~a:[H.a_class ["kind"]] "primitive "
                      ; spans n
                      ]
            | Path p ->
                H.div [ spans ~a:[H.a_class ["kind"]] (Sig.string_of_k k ^ " ")
                      ; hpath p
                      ]
          ]
      | _ ->
          H.div & map (fun (_,i,_,_) -> fsignature_item i) nonaliased
    in
    let aliased = match aliased with
      | [] -> []
      | _ -> [ H.div ~a:[H.a_class ["aliased"]] & map (fun (_,i,_,_) -> fsignature_item i) aliased ]
    in
    H.div ~a:[ H.a_class [ "group" ^ string_of_int (i mod 2) ] ]
      & [ nonaliased ]
        @ aliased
        @ [ H.br () ]
  in
  mapi group sum
    
let query data qs =
  let open Html in
  let pspec =
    match assoc_opt "packtype" qs, assoc_opt "packs" qs with
    | (Some ["vanilla"] | None), Some [s] ->
        Query.PackageSpec.Vanilla (String.split (function ' ' -> true | _ -> false) s)
    | Some ["just"], Some [s] ->
        Query.PackageSpec.Just (String.split (function ' ' -> true | _ -> false) s)
    | Some ["allbut"], Some [s] ->
        Query.PackageSpec.All_but (String.split (function ' ' -> true | _ -> false) s)
    | _ -> Query.PackageSpec.Vanilla []
  in
  match assoc_opt "q" qs with
  | None -> 
      respond ~status:(`Code 200)
      & html oc_header
      & body & [ query_form pspec "" ]

  | Some [qstr] ->
      begin match Query.parse qstr with
      | [] ->
          respond ~status:`Bad_request
          & html oc_header
          & body & [ query_form pspec qstr ]
      | qs -> 
          let res = Exn.catch_ & fun () -> Query.query data pspec qs in
          match res with  
          | `Ok res ->
(*
              let str = 
                let open Format in
                let b = Buffer.create 1000 in
                let ppf = formatter_of_buffer b in
                Summary.group_and_print ppf res;
                fprintf ppf "That's all@.";
                Buffer.contents b
              in
              respond ~status:`OK
              & html oc_header
              & body & query_form qstr @ [pre [pcdata str]]
*)
              respond ~status:`OK
              & html oc_header
              & body & query_form pspec qstr :: print_summary (Summary.group res)
          | `Error (`Exn e) -> 
              let str =
                let trace = Exn.get_backtrace () in
                !% "Uncaught exception: %s\nBacktrace:\n%s\n" (Exn.to_string e) trace
              in
              respond ~status:`Internal_server_error
              & html oc_header
              & body & query_form pspec qstr :: [p [pcdata str]]
      end
  | Some _ ->
      respond ~status:`Bad_request
      & html oc_header
      & body & query_form pspec "" (* XXX *) :: [p [pcdata "Illegal comma separated query"]]

let lazy_file p =
  let open Unix in
  let mtime = ref 0.0 in
  let s = ref "" in
  fun () ->
    let st = stat p in
    if !mtime = st.st_mtime then !s
    else begin
      mtime := st.st_mtime; 
      s := from_Ok & File.to_string p;
      !s
    end

let style_css = lazy_file "style.css"

let server port data =
  let callback _conn req _body =
    let uri = Request.uri req in
    match Uri.path uri with
    | "/style.css" ->
        (* XXX need to check last-modified thing *)
        let headers = Cohttp.Header.of_list ["Content-type", "text/css"] in
        Server.respond_string ~headers ~status:(`Code 200) ~body:(style_css ()) ()
      (* "/favicon.ico" *)
    | "/" -> 
        let qs = Uri.query uri in
        (* let meth = req |> Request.meth |> Code.string_of_method in *) (* we do not care *)
        (* let headers = req |> Request.headers |> Header.to_string in *) (* we do not care *)
        (*
        body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
             uri meth headers body))
        >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        *)
        (* query data Query.PackageSpec.vanilla qs *)
        query data qs
    | _ ->
        prerr_endline & Uri.path uri;
        respond ~status:(`Code 404)
        & H.html oc_header  
        & H.body [ query_form (Query.PackageSpec.Vanilla []) "" ]
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = 
  let port = ref 80 in
  let data_dir = ref "../out" in
  Arg.parse 
    [ "--port", Arg.Int (fun x -> port := x), "port (default is 80)"
    ; "--data-dir", Arg.String (fun s -> data_dir := s), "data dir (default \"../out\")" 
    ]
    (fun _ -> failwith "does not take any anonymous arguments")
    "server --port x --data_dir d";
  let data = Data.DB.unsafe_load (!data_dir ^/ "all.all") in
  ignore (Lwt_main.run (server !port data))


