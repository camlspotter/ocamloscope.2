[@@@ocaml.warning "-a"]

module Make(M : sig val x : int end) = struct
  module S = M (* XXX bug: Make(M).S.x has LocNone now *)
end

  
