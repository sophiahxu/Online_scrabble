exception Exit
(** Raised when the GUI should be closed. *)

val start : unit -> unit
(** [start ()] initializes and starts the application. Raises: [Exit]
    when a key is pressed. *)
