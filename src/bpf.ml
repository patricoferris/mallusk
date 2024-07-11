open Libbpf

let file = [%blob "monitor.bpf.o"]
let program_names = [ "check_writes" ]
let map = "globals"

(* Load PID into BPF map*)
(* let before_link obj =
   let pid = Unix.getpid () |> Signed.Long.of_int in
   let global_map = bpf_object_find_map_by_name obj map in
   bpf_map_update_elem ~key_ty:Ctypes.int ~val_ty:Ctypes.long global_map 0 pid *)

let monitor ~tmp ~before_link fn =
  let filename = Eio.Path.(tmp / Filename.temp_file "cshell-" ".bpf.o") in
  Eio.Path.save ~create:(`If_missing 0o644) filename file;
  let obj_path = Eio.Path.native_exn filename in
  with_bpf_object_open_load_link ~obj_path ~program_names ~before_link
    (fun obj link ->
      let exitting = ref true in
      let sig_handler = Sys.Signal_handle (fun _ -> exitting := false) in
      Sys.(set_signal sigint sig_handler);
      Sys.(set_signal sigterm sig_handler);

      Printf.printf
        "Successfully started! Please run `sudo cat \
         /sys/kernel/debug/tracing/trace_pipe` to see output of the BPF \
         programs.\n\
         %!";

      (* Loop until Ctrl-C is called *)
      while !exitting do
        fn obj link
      done)
