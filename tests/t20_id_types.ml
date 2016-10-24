(* To reproduce a Coerce issue around 
   OCaml's middle_end/base_types/id_types.{ml,mli}
*)

[@@@ocaml.warning "-a"]

module type Thing = sig
  type t
  val x : t
end
  
module M : sig

  (** Fully qualified identifiers *)
  module type UnitId = sig
    module Compilation_unit : Thing
    type u
    val z : unit
  end
  
  module MUnitId :
    functor (Compilation_unit : Thing) ->
      UnitId with module Compilation_unit := Compilation_unit

end = struct

  module type UnitId = sig
    module Compilation_unit : Thing
    type u
    val z : unit
  end
  
  module MUnitId(Compilation_unit:Thing) = struct
    type u
    let z = ()
  end

end
  

  
