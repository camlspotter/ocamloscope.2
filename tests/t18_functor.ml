module type S = sig
  val x : int
end

module F(A : S) = struct
  let y = A.x
end

module M : sig
  module G(B : S) : sig val z : int end
end = struct
  module G (B : sig val x : int end) = struct let z = B.x end
end
  
