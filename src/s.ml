module type Action = sig
  type t
  (** Possible actions to take resulting from a command line *)

  type ctx
  (** A piece of state that is mapped through the execution of your
      shell. *)

  type state
  (** State to persist and restore *)

  val prompt : ctx -> unit
  (** A shell prompt *)

  val state_to_json : state -> Ezjsonm.value
  (** Serialise a context to JSON *)

  val state_of_json : Ezjsonm.value -> state
  (** Deserialise a context from JSON *)

  val init_ctx : ?previous:state -> Eio_posix.stdenv -> ctx
  (** Initialisation of a context, perhaps from persisted state. *)

  val of_line : string -> t
  (** [of_line cmdline] converts [cmdline] into an action *)

  val run :
    ctx:ctx ->
    env:Eio_posix.stdenv ->
    t ->
    (state * ctx, [ `Msg of string | `Exit of int ]) result
  (** [run ~ctx ~env action] runs [action] and returns successfully or with some error *)
end
