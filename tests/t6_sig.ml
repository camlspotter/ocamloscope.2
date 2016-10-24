module M : sig
  type t = int
  val x : int
  val y : int
  val z : int
end = struct
  type t = int
  type u = float
  let z = 1
  let y = 2
  let x = 3
  let y = 4
  let z = 5
end
  
