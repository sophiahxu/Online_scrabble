val init : unit
(**[init] starts the challenge mode by drawing a challenge box in place
   of the key, and asks what player is challenging*)

val which_player : int
(**[which_player] returns the player number that initiated the challenge
   Requires: the user clicked on the correct player that initiated the
   challenge*)

val look_up : bool -> unit
(**[look_up status] displays instructions, telling the challenging
   player to look up the challenged word in a dictionary. Prompt will
   only be displayed when [status] is true*)
