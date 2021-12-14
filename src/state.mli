(**[input] represents GUI player input.*)
type input =
  | Clicked of int * int
  | Enter
  | Z

type t
(**[info] represents the scrabble game state*)

val init : unit -> t
(**[init ()] initializes the scrabble game state for the start of the
   game*)

val init_end : unit -> t
(**[init_end ()] initializes the scrabble game state for clos to the end
   of the game*)

val init_draw : t -> unit
(**[init_draw state] draws the initial state of the game.*)

val update : input option -> t -> t
(**[update input state] is an updated [state] depending on the GUI
   [input] given.*)

val draw : t -> input option -> unit
(**[draw state input] draws all of the current scrabble game components
   in [state] to the GUI depending on [state] and whether there is
   [input].*)

val game_over : t -> bool
(**[game_over state] is true with the game is over, else it is false.*)
