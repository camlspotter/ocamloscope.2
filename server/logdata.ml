open Spotlib.Spot
open Ocaml_conv.Default

module Client = struct

  open Conduit_lwt_unix

  type t = string [@@deriving conv{ocaml}]
      
  let source = function
    | TCP tcp_flow -> Ipaddr.to_string tcp_flow.ip
    | Domain_socket _domain_flow -> "domainsocket"
    | Vchan vchan_flow -> Printf.sprintf "vchan.%d" vchan_flow.domid

end
  
type t =
  | Server_started of { port : int }
  | Request of { from : Client.t;
                 query : (string * string list) list }
  | Response of { from : Client.t;
                  query : (string * string list) list; 
                  status : int;
                  nitems : int option;
                  query_time : float option;
                  render_time : float option;
                  response_time : float }
  | Strange_path of { from : Client.t;
                      path : string }
  | Note of string
  | Crash of { exn : string; trace : string }
[@@deriving conv{ocaml}]
                    
let log (t : t) : unit =
  Lwt_log.ign_notice
    (Format.sprintf "%a"
       (fun ppf ->
         (* Surpress new lines *)
         Format.pp_set_margin ppf max_int;
         Ocaml.format_with [%derive.ocaml_of: t] ppf) t)


