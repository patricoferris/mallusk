module Fmt = struct
  include Fmt

  let pp_color c = Fmt.(styled (`Fg c) string)
end

let pwd () = Eio_unix.run_in_systhread ~label:"getpwd" Unix.getcwd
let chdir f = Eio_unix.run_in_systhread ~label:"chdir" (fun () -> Unix.chdir f)
