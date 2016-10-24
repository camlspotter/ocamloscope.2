[@@@ocaml.warning "-a"]

class virtual t = object (self)
  method m = self#n + 1 (* XXX method n should exist but not in the hump *)
end
