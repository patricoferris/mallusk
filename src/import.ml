module Fmt = struct
  include Fmt

  let pp_color c = Fmt.(styled (`Fg c) string)
end
