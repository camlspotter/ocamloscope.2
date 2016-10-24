module M = struct
  type t = int
  let x = 1 (* 0 : shadowed *)
  let y = 2 (* 1 *)
  let z = 3 (* 2 : shadowed *)
  let x = 4 (* 3 *)
  let z = 5 (* 4 *)
end

