module M = struct
  let x = 1
  module I = struct
    let y = 1
  end
end
module N = M
module O = N
module P = O
