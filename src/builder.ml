open Lwt.Infix
module Native_sandbox = Obuilder.Native_sandbox
module Docker_sandbox = Obuilder.Docker_sandbox
module Docker_store = Obuilder.Docker_store
module Docker_extract = Obuilder.Docker_extract
module Archive_extract = Obuilder.Archive_extract
module Store_spec = Obuilder.Store_spec

type t = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> t

let ( / ) = Filename.concat

let create_builder _env store_spec conf =
  store_spec >>= fun (Store_spec.Store ((module Store), store)) ->
  let module Builder =
    Obuilder.Builder (Store) (Native_sandbox) (Docker_extract)
  in
  Native_sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Builder ((module Builder), builder)

let log tag msg =
  match tag with
  | `Heading -> Fmt.epr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.epr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output ->
      output_string stdout msg;
      flush stdout

let build (Builder ((module Builder), builder)) from cmd =
  let spec =
    let open Obuilder_spec in
    stage ~from [ run cmd ]
  in
  let ctx = Obuilder.Context.v ~log ~src_dir:"/" () in
  Builder.build builder ctx spec
