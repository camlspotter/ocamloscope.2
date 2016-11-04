let (&) = (@@)
let (>>=) = Lwt.bind
let js = Js.string
let to_option = Js.Opt.to_option   
let catch_ f = try `Ok (f ()) with e -> `Error (`Exn e)  
    
(** Dom *)

let remove_children x =
  List.iter (Dom.removeChild x) & Dom.list_of_nodeList x##.childNodes

(** Dom_html *)

module Html = Dom_html

let alert s = Html.window##alert (js s)
let alert_js s = Html.window##alert s

let catch_and_alert_ f =
  try f () with e -> alert (Printexc.to_string e)

exception No_element_id of string
  
let getById n = match Html.getElementById n with
  | exception Not_found -> raise (No_element_id n)
  | x -> x

let getByIdIn n xml = match to_option & xml##getElementById (js n) with
  | None -> raise (No_element_id n)
  | Some xml -> xml

exception Content_is_not_xml

let get_content_xml hf = match hf.XmlHttpRequest.content_xml () with
  | None ->
      alert (hf.XmlHttpRequest.content);
      raise Content_is_not_xml
  | Some xml -> xml

module Coerce = struct
  exception Coercion_failure of string

  let coerce_or_fail n f x =
    match to_option & f x with
    | None -> raise (Coercion_failure n)
    | Some x -> x

  let form x = coerce_or_fail "form" Dom_html.CoerceTo.form x
  let input x = coerce_or_fail "input" Dom_html.CoerceTo.input x
  let select x = coerce_or_fail "select" Dom_html.CoerceTo.select x
end
