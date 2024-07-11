module Cshell_Action = struct
  type t =
    | History of string option
    | Cd of string
    | Exec of string * string list

  type ctx = History.Manager.t
  type state = History.t list

  let prompt _ctx =
    Fmt.pr "%a [%a]\n$ %!" (pp_color `Yellow) "cshell" (pp_color `Green)
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
