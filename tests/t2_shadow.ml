type a = C
type b = C

let x = 1 (* funny, this is shadowed and inaccessible, but it is still in the signature *)
let () = print_int x  

let x = 2

