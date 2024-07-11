open! Import
module Posix = Eio_posix.Low_level

let pwd () = Eio_unix.run_in_systhread ~label:"getpwd" Unix.getcwd
let chdir f = Eio_unix.run_in_systhread ~label:"chdir" (fun () -> Unix.chdir f)

module Cshell_Action = struct
  type t =
    | History of string option
    | Cd of string
    | Exec of string * string list

  type ctx = History.Manager.t
  type state = History.t list

  let prompt _ctx =
    Fmt.pr "%a [%a]\n$ %!" (Fmt.pp_color `Yellow) "cshell" (Fmt.pp_color `Green)
      (pwd ())

  let state_to_json = History.to_json
  let state_of_json = History.of_json

  let init_ctx ?previous _env =
    let state = match previous with Some lst -> lst | None -> [] in
    History.Manager.v state

  let run ~ctx:history_mgr ~env = function
    | History query ->
        Fmt.pr "%a\n%!" History.pp (History.Manager.search history_mgr query);
        Ok (history_mgr.items, history_mgr)
    | Cd dir ->
        chdir dir;
        Ok (history_mgr.items, history_mgr)
    | Exec (exec, ls) ->
        let proc = Eio.Stdenv.process_mgr env in
        let stdin = Eio.Stdenv.stdin env in
        let stdout = Eio.Stdenv.stdout env in
        let stderr = Eio.Stdenv.stderr env in
        let clock = Eio.Stdenv.clock env in
        let history_item = History.v ~clock (exec :: ls) in
        let mgr = History.Manager.add history_mgr history_item in
        Eio.Process.run ~stdin ~stdout ~stderr proc (exec :: ls);
        Ok (mgr.items, mgr)

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
    | exec :: rest -> Exec (exec, rest)
    | [] -> assert false
end

(* Load PID into BPF map*)
let _before_link obj =
  let pid = Unix.getpid () |> Signed.Long.of_int in
  let global_map = Libbpf.bpf_object_find_map_by_name obj Bpf.map in
  Libbpf.bpf_map_update_elem ~key_ty:Ctypes.int ~val_ty:Ctypes.long global_map 0
    pid

module Make (A : S.Action) = struct
  let shell_loop () =
    Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
    Eio_posix.run @@ fun env ->
    let home = Xdg.create ~env:Sys.getenv_opt () |> Xdg.home_dir in
    let cshell_dir = Eio.Path.(env#fs / home / ".cshell") in
    (try Eio.Path.mkdir ~perm:0o755 cshell_dir
     with Eio.Exn.Io (Eio.Fs.(E (Already_exists _)), _) -> ());
    let _history_file = Eio.Path.(cshell_dir / "cshell_history") in
    let ctx_file = Eio.Path.(cshell_dir / "cshell_ctx.json") in
    let previous =
      match Eio.Path.kind ~follow:true ctx_file with
      | `Not_found -> None
      | `Regular_file ->
          Some
            (Eio.Path.load ctx_file |> Ezjsonm.value_from_string
           |> A.state_of_json)
      | _ -> Fmt.failwith "Malformed %s" (Eio.Path.native_exn ctx_file)
    in
    let ctx = ref (A.init_ctx ?previous env) in
    let for_action_stdin = Eio.Buf_read.of_flow ~max_size:max_int env#stdin in
    while true do
      A.prompt !ctx;
      let line = Eio.Buf_read.line for_action_stdin in
      match A.run ~ctx:!ctx ~env (A.of_line line) with
      | Ok (state, new_ctx) ->
          Eio.Path.save ~create:(`If_missing 0o755) ctx_file
            (A.state_to_json state |> Ezjsonm.value_to_string);
          ctx := new_ctx
      | Error (`Msg m) -> failwith m
      | Error (`Exit i) -> Fmt.failwith "Exited with %i" i
    done
end

module Cshell = Make (Cshell_Action)

let () = Cshell.shell_loop ()
