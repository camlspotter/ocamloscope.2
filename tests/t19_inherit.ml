module M : sig

  class type progressEventTarget = object ('self)
    method onloadstart : int
  end
  
  class type fileReader = object ('self)
    method readAsArrayBuffer : int
    method onloadstart : int
  end

end = struct

  class type progressEventTarget = object ('self)
    method onloadstart : int
  end
  
  class type fileReader = object ('self)
    method readAsArrayBuffer : int
    inherit progressEventTarget
  end
            
end

