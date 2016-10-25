open Js

val ( & ) : ('a -> 'b) -> 'a -> 'b
(** Same as [(@@)] *)
  
val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
(** Same as [Lwt.bind] *)

val js : string -> js_string t
(** Same as [string] *)
  
val to_option : 'a Opt.t -> 'a option
(** Same as [Opt.to_option *)
  
val catch_ : (unit -> 'a) -> [> `Error of [> `Exn of exn ] | `Ok of 'a ]
(** Catch exception *)

val remove_children : #Dom.node t -> unit
(** Remove all the children of the given node *)

val alert : string -> unit
(** Alert dialog *)
  
val catch_and_alert_ : (unit -> unit) -> unit
(** Execute the function and show an alert dialog if an exception is caught *)

exception No_element_id of string

val getById : string -> Dom_html.element t
(** Get the node of the given name in the entire page.
    If not found, [No_element_id n] is raised.
 *)
  
val getByIdIn :
  string ->
  < getElementById : js_string t -> 'a Opt.t meth; .. > t ->
  'a
(** Get the node of the given name in the node 
    If not found, [No_element_id n] is raised.
*)

exception Content_is_not_xml

val get_content_xml :
  string XmlHttpRequest.generic_http_frame -> Dom.element Dom.document t
(** Get the XML content of the response.  If there is no XML content,
    it raises [Content_is_not_xml].
*)
  
module Coerce : sig

  exception Coercion_failure of string

  val form : #Dom_html.element Js.t -> Dom_html.formElement Js.t
  (** Coercion check to a form. If fail, it raises [Coercion_of_failure "form"].
  *)

  val input : #Dom_html.element Js.t -> Dom_html.inputElement Js.t
  val select : #Dom_html.element Js.t -> Dom_html.selectElement Js.t
end
  
