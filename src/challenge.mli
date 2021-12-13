type t
(**Represents a scrabble challenge.*)

val init : int -> t
(**[init int] initializes the challenge gameplay mode, with player [int]
   being the challenged player.*)

val click : int -> int -> t -> t
(**[click x y c] returns updated challenge [c] according to the [x] and
   [y] components of the click input.*)

val draw : t -> unit
(**[draw c] draws the challenge [c] to the screen.*)

val loser : t -> int option
(**[loser c] returns the number of the player that lost in the challenge
   [c].*)

val finished : t -> bool
(**[finished c] is whether the challenge mode is finished.*)
