module F(A : sig val x : int end) = struct
  let y = A.x
  module A = A
end
  
  
