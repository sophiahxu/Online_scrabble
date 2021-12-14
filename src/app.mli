exception Exit
(** Raised when the GUI should be closed. *)

val start : unit -> unit
(** [start ()] initializes and starts the application. Raises: [Exit]
    when a key is pressed. *)

val start_end : unit -> unit
(** [start_end ()] initializes and starts the application at a point in
    the game close to the end. Raises: [Exit] when a key is pressed. *)
