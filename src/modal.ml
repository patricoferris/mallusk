open! Import

type mode = { fs : [ `R | `RW ]; net : [ `On | `Off ] }

type t =
  | History of string option
  | Cd of string
  | Exec of string * string list
  | Mode_fs of [ `R | `RW ]
  | Mode_net

let name = "modal"
let doc = "A modal shell backing on to ZFS and RUNC"
let default_mode = { fs = `R; net = `Off }

let pp_mode ppf mode =
  let pp_fs ppf = function
    | `R -> Fmt.pp_color `Magenta ppf "r"
    | `RW -> Fmt.pp_color `Magenta ppf "rw"
  in
  let pp_net ppf = function
    | `On -> Fmt.pp_color `Magenta ppf "on"
    | `Off -> Fmt.pp_color `Magenta ppf "off"
  in
  Fmt.pf ppf "< fs:%a | net:%a >" pp_fs mode.fs pp_net mode.net

type state = {
  items : History.t list;
  from : [ `Build of string | `Image of string ];
}

type ctx = {
  mode : mode;
  history : History.Manager.t;
  builder : Builder.t;
  state : state;
}

let state_of_ctx c = c.state

type args = Builder.Native_sandbox.config

let args = Builder.Native_sandbox.cmdliner

let prompt ctx =
  Fmt.pr "%a [%a] %a\n$ %!" (Fmt.pp_color `Yellow) "cshell"
    (Fmt.pp_color `Green) (pwd ()) pp_mode ctx.mode

let state_to_json { items; from } =
  let items = History.to_json items in
  let from =
    match from with
    | `Build s -> `A [ `String "build"; `String s ]
    | `Image i -> `A [ `String "image"; `String i ]
  in
  `O [ ("items", items); ("from", from) ]

let state_of_json = function
  | `O _ as assoc ->
      let items = Ezjsonm.find assoc [ "items" ] |> History.of_json in
      let from =
        match Ezjsonm.find assoc [ "from" ] with
        | `A [ `String "build"; `String s ] -> `Build s
        | `A [ `String "image"; `String s ] -> `Image s
        | _ -> failwith "Unrecognised image or build from!"
      in
      { items; from }
  | _ -> failwith "Failed to deserialise the state of modal shell"

let default_image =
  "ghcr.io/osgeo/gdal:ubuntu-small-3.6.3@sha256:bfa7915a3ef942b4f6f61223ee57eadbb469d6fb4a5fbf562286d1473f15eaab"

let init_ctx ?previous args env =
  let state =
    match previous with
    | Some s -> s
    | None -> { items = []; from = `Image default_image }
  in
  let history = History.Manager.v state.items in
  let _, store =
    Obuilder.Store_spec.to_store (`Zfs (None, "obuilder-zfs", false))
  in
  let builder =
    Lwt_eio.run_lwt @@ fun () -> Builder.create_builder env store args
  in
  { history; mode = default_mode; builder; state }

let run ~env:_ ({ history = history_mgr; mode; builder; state } as ctx) =
  function
  | History query ->
      Fmt.pr "%a\n%!" History.pp (History.Manager.search history_mgr query);
      Ok ctx
  | Cd dir ->
      chdir dir;
      Ok ctx
  | Mode_fs fs -> Ok { ctx with mode = { ctx.mode with fs } }
  | Mode_net ->
      Ok
        {
          ctx with
          mode =
            { ctx.mode with net = (if ctx.mode.net = `On then `Off else `On) };
        }
  | Exec (exec, ls) -> (
      match mode.fs with
      | `R ->
          Fmt.pr "TODO\n";
          Ok ctx
      | `RW -> (
          let (Builder ((module B), builder)) = builder in
          let network = if mode.net = `On then [ "host" ] else [] in
          let spec =
            let open Obuilder_spec in
            stage ~from:state.from
              [ run ~network "%s %s" exec (String.concat " " ls) ]
          in
          let context = Obuilder.Context.v ~log:Builder.log ~src_dir:"." () in
          let result =
            Lwt_eio.run_lwt @@ fun () -> B.build builder context spec
          in
          match result with
          | Ok build ->
              Ok { ctx with state = { ctx.state with from = `Build build } }
          | Error (`Failed (m, _)) -> Error (`Msg m)
          | Error `Cancelled -> Error (`Msg "cancelled")
          | Error (`Msg _) as e -> e))

(* Does OCaml have a good [string -> string list] command line
   parser? *)
let command_line command =
  (* match Morbig.parse_string "cshell" command with *)
  String.split_on_char ' ' command
  |> List.filter (fun v -> not (String.equal "" v))

let of_line line =
  match command_line line with
  | "cd" :: [ dir ] -> Cd dir
  | [ "history" ] -> History None
  | "history" :: [ query ] -> History (Some query)
  | "mode" :: [ "read" ] -> Mode_fs `R
  | "mode" :: [ "write" ] -> Mode_fs `RW
  | "mode" :: [ "net" ] -> Mode_net
  | exec :: rest -> Exec (exec, rest)
  | [] -> assert false
