open Xjs

let server = "https://5822cdcc.ngrok.io"
(* let server = "http://localhost:8080" *)

let query () =
  (* XXX we should catch errors *)
  let q        = Js.to_string & (Coerce.input & getById "q")        ##.value in
  let packtype = Js.to_string & (Coerce.select & getById "packtype") ##.value in
  let packs    = Js.to_string & (Coerce.input & getById "packs")    ##.value in
  let url = (Printf.sprintf "%s/?%s"
                        server
                        (Url.encode_arguments [ "q", q
                                              ; "packtype", packtype
                                              ; "packs", packs])) in
  let insert = getById "insert" in
  remove_children insert;
  let wait = getById "wait" in
  wait##setAttribute (js "style") (js "display: block;");
  XmlHttpRequest.get url
  >>= fun hf ->
  wait##setAttribute (js "style") (js "display: none;");
  insert##.innerHTML := js hf.content;
  Lwt.return ()
    
let form () =
  let form = Coerce.form & getById "form" in
  form##.onsubmit := (Dom.handler (fun _ ->
    catch_and_alert_ (fun () -> ignore & query ());
    Js._false))
    
let start _ =
  form ();
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler start
