open! Import

type mode = { fs : [ `R | `RW ]; net : [ `On | `Off ] }

type t =
  | History of string option
  | Cd of string
  | Exec of string * string list
  | Mode_fs of [ `R | `RW ]
  | Mode_net
  | Build_hash
  | Undo
  | Diff

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
  builds : string list;
}

type ctx = {
  mode : mode;
  history : History.Manager.t;
  builder : Builder.t;
  store : Obuilder.Store_spec.store;
  state : state;
}

let state_of_ctx c = c.state

type args = Builder.Native_sandbox.config

let args = Builder.Native_sandbox.cmdliner

let prompt ctx =
  Fmt.pr "%a [%a] %a\n$ %!" (Fmt.pp_color `Yellow) "mallusk"
    (Fmt.pp_color `Green) (pwd ()) pp_mode ctx.mode

let state_to_json { items; from; builds } =
  let items = History.to_json items in
  let from =
    match from with
    | `Build s -> `A [ `String "build"; `String s ]
    | `Image i -> `A [ `String "image"; `String i ]
  in
  `O
    [
      ("items", items);
      ("from", from);
      ("builds", `A (List.map (fun v -> `String v) builds));
    ]

let state_of_json = function
  | `O _ as assoc ->
      let items = Ezjsonm.find assoc [ "items" ] |> History.of_json in
      let builds =
        Ezjsonm.find assoc [ "builds" ] |> Ezjsonm.get_list Ezjsonm.get_string
      in
      let from =
        match Ezjsonm.find assoc [ "from" ] with
        | `A [ `String "build"; `String s ] -> `Build s
        | `A [ `String "image"; `String s ] -> `Image s
        | _ -> failwith "Unrecognised image or build from!"
      in
      { items; from; builds }
  | _ -> failwith "Failed to deserialise the state of modal shell"

let default_image =
  "ghcr.io/osgeo/gdal:ubuntu-small-3.6.3@sha256:bfa7915a3ef942b4f6f61223ee57eadbb469d6fb4a5fbf562286d1473f15eaab"

let init_ctx ?previous args env =
  let state =
    match previous with
    | Some s -> s
    | None -> { items = []; from = `Image default_image; builds = [] }
  in
  let history = History.Manager.v state.items in
  let _, store =
    Obuilder.Store_spec.to_store (`Zfs (None, "obuilder-zfs", false))
  in
  let builder =
    Lwt_eio.run_lwt @@ fun () -> Builder.create_builder env store args
  in
  let store = Lwt_eio.run_lwt @@ fun () -> store in
  { history; mode = default_mode; builder; store; state }

let () =
  match Sys.getenv_opt "MALLUSK_DEBUG" with
  | Some _ ->
      Fmt_tty.setup_std_outputs ();
      Logs.set_level (Some Info);
      Logs.Src.set_level Obuilder.log_src (Some Info);
      Logs.set_reporter (Logs_fmt.reporter ())
  | None -> ()

let run ~env ({ history = history_mgr; mode; builder; state; store = _ } as ctx)
    = function
  | History query ->
      Fmt.pr "%a\n%!" History.pp (History.Manager.search history_mgr query);
      Ok ctx
  | Build_hash ->
      Fmt.pr "%s\n%!" (match state.from with `Build s -> s | `Image i -> i);
      Ok ctx
  | Cd dir ->
      chdir dir;
      Ok ctx
  | Diff ->
      let () =
        match (ctx.state.from, ctx.state.builds) with
        | _, [] | `Image _, _ | `Build _, [ _ ] -> Fmt.pr "No diff\n%!"
        | `Build c, _ :: p :: _ ->
            (* Horrible Hack -- we need a better Store abstraction *)
            let ds1 = Fmt.str "obuilder-zfs/result/%s@snap" c in
            let ds2 = Fmt.str "obuilder-zfs/result/%s@snap" p in
            let diff =
              Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
                [ "zfs"; "diff"; ds2; ds1 ]
            in
            String.split_on_char '\n' diff
            |> List.filter_map (fun v ->
                   match Astring.String.find_sub ~sub:"rootfs" v with
                   | Some i ->
                       (* Remove "rootfs" *)
                       let i = i + 6 in
                       let diff_kind = String.make 1 v.[0] in
                       let sub = String.sub v i (String.length v - i) in
                       let sub = if sub = "" then "/" else sub in
                       Some (diff_kind ^ " " ^ sub)
                   | _ -> None)
            |> String.concat "\n" |> Fmt.pr "%s\n%!"
      in
      Ok ctx
  | Mode_fs fs -> Ok { ctx with mode = { ctx.mode with fs } }
  | Mode_net ->
      Ok
        {
          ctx with
          mode =
            { ctx.mode with net = (if ctx.mode.net = `On then `Off else `On) };
        }
  | Undo ->
      let from, builds =
        match ctx.state.builds with
        | [] -> (`Image default_image, [])
        | [ b ] -> (`Build b, [])
        | b :: x :: bs ->
            if ctx.state.from = `Build b then (`Build x, bs) else (`Build b, bs)
      in
      Ok { ctx with state = { ctx.state with builds; from } }
  | Exec (exec, ls) -> (
      match mode.fs with
      | `R ->
          Fmt.pr "Read mode not implemented, try `mode write`\n%!";
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
              let builds = build :: ctx.state.builds in
              Ok
                {
                  ctx with
                  state = { ctx.state with from = `Build build; builds };
                }
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
  | [ "build-hash" ] -> Build_hash
  | [ "undo" ] -> Undo
  | [ "diff" ] -> Diff
  | exec :: rest -> Exec (exec, rest)
  | [] -> assert false
