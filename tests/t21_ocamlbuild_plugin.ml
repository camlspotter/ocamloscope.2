(* reproduction of failure around ocamlbuild's ocamlbuild_plugin.ml *)

module type PATHNAME = sig
  type t
end

module Qathname : sig
  include PATHNAME
  val x : int
end = struct
  type t = unit
  let x = 42
end
  
module type PLUGIN = sig
  module Pathname : PATHNAME
end
  
module M : PLUGIN with module Pathname = Qathname
= struct
  module Pathname = Qathname
end
