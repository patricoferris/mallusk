open! Import

module T = struct
  type t = {
    timestamp : string;
    command : string list;
    inputs : string list;
    outputs : string list;
  }

  let pp_item ppf t =
    Fmt.pf ppf "[%a] %a" (Fmt.pp_color `Cyan) t.timestamp (Fmt.pp_color `Yellow)
      (String.concat " " t.command)

  let v ~clock ?(inputs = []) ?(outputs = []) command =
    let timestamp =
      Eio.Time.now clock |> Ptime.of_float_s |> Option.get |> Ptime.to_rfc3339
    in
    { timestamp; command; inputs; outputs }

  let item_to_json item =
    `O
      [
        ("timestamp", `String item.timestamp);
        ("command", `A (List.map (fun s -> `String s) item.command));
        ("inputs", `A (List.map (fun s -> `String s) item.inputs));
        ("outputs", `A (List.map (fun s -> `String s) item.outputs));
      ]

  let item_of_json = function
    | `O _ as assoc ->
        let timestamp =
          Ezjsonm.find assoc [ "timestamp" ] |> Ezjsonm.get_string
        in
        let command =
          Ezjsonm.find assoc [ "command" ]
          |> Ezjsonm.get_list Ezjsonm.get_string
        in
        let inputs =
          Ezjsonm.find assoc [ "inputs" ] |> Ezjsonm.get_list Ezjsonm.get_string
        in
        let outputs =
          Ezjsonm.find assoc [ "outputs" ]
          |> Ezjsonm.get_list Ezjsonm.get_string
        in
        { timestamp; command; inputs; outputs }
    | _ -> failwith "Expected an object for a shell item"

  let to_json history = `A (List.map item_to_json history)

  let of_json = function
    | `A lst -> List.map item_of_json lst
    | _ -> failwith "Expected a list of shell history items"

  let pp ppf v = Fmt.pf ppf "%a" Fmt.(list pp_item) v
end

module Search = Search.Tfidf.Mono (Search.Uids.String) (T)
include T

module Manager = struct
  (* Probably need a notion of sessions *)
  type item = t
  type t = { items : item list; search : Search.t }

  let v items =
    let search = Search.empty () in
    Search.add_index search (fun (item : item) ->
        String.concat " " item.command);
    Search.add_index search (fun (item : item) -> String.concat " " item.inputs);
    Search.add_index search (fun (item : item) ->
        String.concat " " item.outputs);
    List.iter (fun item -> Search.add_document search item.timestamp item) items;
    { items; search }

  let add t item =
    let items = item :: t.items in
    Search.add_document t.search item.timestamp item;
    { t with items }

  let search t = function
    | None -> t.items
    | Some query -> Search.search t.search query
end
