type info
(**[info] represents gameplay information while the game is in state
   [Active]*)

(**[t] represents the state of the game*)
type t =
  | Loading_initial
  | Initial
  | Loading_active
  | Active of info
  | Loading_complete
  | Complete

val init_info : unit -> info
(**[init_info ()] initializes the gameplay information for the start of
   the game*)
