module type Action = sig
  type t
  (** Possible actions to take resulting from a command line *)

  val name : string
  (** The name of this shell *)

  val doc : string
  (** Short documentation comment for this shell *)

  type ctx
  (** A piece of state that is mapped through the execution of your
      shell. *)

  type state
  (** State to persist and restore *)

  type args
  (** Arguments from the command line *)

  val args : args Cmdliner.Term.t
  (** Cmdliner term for the arguments *)

  val prompt : ctx -> unit
  (** A shell prompt *)

  val state_to_json : state -> Ezjsonm.value
  (** Serialise a context to JSON *)

  val state_of_json : Ezjsonm.value -> state
  (** Deserialise a context from JSON *)

  val init_ctx : ?previous:state -> args -> Eio_posix.stdenv -> ctx
  (** Initialisation of a context, perhaps from persisted state. *)

  val of_line : string -> t
  (** [of_line cmdline] converts [cmdline] into an action *)

  val state_of_ctx : ctx -> state
  (** Store state alongside your context and this will be persisted
      between runs *)

  val run :
    env:Eio_posix.stdenv ->
    ctx ->
    t ->
    (ctx, [ `Msg of string | `Exit of int ]) result
  (** [run ~ctx ~env action] runs [action] and returns successfully or with some error *)
end
