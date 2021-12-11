type t
(**Represents a scrabble challenge.*)

val init : unit -> t
(**[init ()] initializes the challenge gameplay mode.*)

val click : int -> int -> t -> t
(**[click x y c] returns un updated challenge [c] according to the [x]
   and [y] components of the click input.*)

val draw : t -> unit
(**[draw c] draws the challenge [c] to the screen.*)

val loser : t -> int option
(**[loser c] returns the number of the player that lost in the challenge
   [c].*)

val finished : t -> bool
(**[finished c] is whether the challenge mode is finished.*)

(*val init : int (**[init] starts the challenge mode by drawing a
  challenge box in place of the key, and asks what player is
  challenging. [init] returns the integer of the player that lost the
  challenge*)*)
