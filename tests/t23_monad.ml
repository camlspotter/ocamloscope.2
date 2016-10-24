(* extract crashes due to stack overflow 

   verified it crashes also with this extracted code

   Conclusion: functor arguments need to be alpha-converted!
*)

module type T2 = sig
  module S : sig end
end

module Make2(M : sig end) : T2 = struct
  module S = M
end
  
module Make1(M : sig end) = struct (* if I change M by N, it passes *)
  module M2 : sig end = M
  module M3 = Make2(M2)
end

module X = Make1(struct end)
