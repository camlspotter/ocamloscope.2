open Xjs

let server = "https://5822cdcc.ngrok.io"
(* let server = "http://localhost:8080" *)

let form () =
  let q        = Coerce.input & getById "q" in
  let packtype = Coerce.select & getById "packtype" in
  let packs    = Coerce.input & getById "packs" in
  let _form     = Coerce.form & getById "form" in
  let submit   = Coerce.input & getById "submit" in
  let insert   = getById "insert" in
  let wait     = getById "wait" in

  let query () =
    let url =
      let q        = Js.to_string q##.value in
      let packtype = Js.to_string packtype##.value in
      let packs    = Js.to_string packs##.value in
      Printf.sprintf "%s/?%s"
        server
        (Url.encode_arguments [ "q"        , q
                              ; "packtype" , packtype
                              ; "packs"    , packs])
    in
    submit##.disabled := Js._true;
    remove_children insert;
    wait##setAttribute (js "style") (js "display: block;");
    XmlHttpRequest.get url

    >>= fun hf ->

    submit##.disabled := Js._false;
    wait##setAttribute (js "style") (js "display: none;");
    insert##.innerHTML := js hf.content;
    Lwt.return ()
  in
    
(*
  form##.onsubmit := (Dom.handler (fun _ ->
    catch_and_alert_ (fun () -> ignore & query ());
    Js._false));
*)
    
  let query_in_url () =
    let open Url in
    match url_of_string & Js.to_string Dom_html.window##.location##.href with
    | Some (Http {hu_arguments=xs} | Https {hu_arguments=xs} | File {fu_arguments=xs}) ->
        let assoc_and_fill k input =
          match List.assoc k xs with
          | exception Not_found -> ()
          | s -> input##.value := js s
        in
        assoc_and_fill "q" q;
        assoc_and_fill "packtype" packtype;
        assoc_and_fill "packs" packs;
        if Js.to_string q##.value <> "" then ignore & query ()
    | _ -> ()
  in
  query_in_url ()

let start _ =
  catch_and_alert_ form;
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler start
