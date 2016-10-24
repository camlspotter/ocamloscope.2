module type S = functor (A : sig type t val x : t end) -> sig
  type t = A.t
  val x : A.t
  type u = A.t
  val y : u
end

module Make : S = functor (A: sig type t val x : t end) -> struct
  include A
  type u = A.t
  let y = A.x
end

module B = struct type t = Foo let x = Foo end

module X = Make(B)

module Y = Make(struct type t = bool let x = true end)
  
