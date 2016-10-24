module rec A : sig
  module B : sig end
end = struct
  module B = A.B
end
