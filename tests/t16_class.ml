module M : sig
  class c : object
    method x : int
  end
end = struct
  class c = object
    method x = 1
  end
end

class q = object
  method x = 1
end
