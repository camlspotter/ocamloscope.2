module M : sig
  val x : int
  module N : sig
    val z : int
  end
end = struct
  let x = 1
  let y = 2
  module N = struct
    let z = 3
    let w = 4
  end
end
