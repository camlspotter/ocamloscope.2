module F(A : sig val x : int end) = struct
  include A
end

module M = F(struct
  let x = 1
end)

include M
