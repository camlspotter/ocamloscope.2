module K = struct
  type s
end

module M1 = struct
  type t = Bar of K.s
end

module M2 : module type of M1 = struct
  type t = Bar of K.s
end


module Temporal = struct
  module Datetime = struct
    type t = { x : int }
  end
end

module DatetimeCopy = Temporal.Datetime
