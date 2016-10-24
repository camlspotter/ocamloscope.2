[@@@ocaml.warning "-a"]

module type T = sig
  module S : sig val x : int end
end
  
module Make(M : sig val x : int end) : T = struct
  module S = M
end

  
