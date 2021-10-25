exception Exit
(** Raised when the GUI should be closed. *)

val update : State.t -> unit
(**[update state] updates the game each frame depending on the game
   state.

   [Loading_initial]: This is the very first state in the game. It draws
   the button and a welcoming message to the screen that displays
   throughout game state [Initial].

   [Initial]: This is the state when the application first opens. It is
   a paused state, waiting for the player to start the game. It displays
   a play button and welcoming message on the screen. The application
   remains in this state so long as the player never presses a key and
   never presses the play button.

   [Loading_active]: This is the state that creates the game board and
   shows it on the screen. The application switches to this state if the
   state was [Initial] in the previous frame, and the player pressed the
   play button. This state only lasts one animation frame before
   switching to [Active].

   [Active]: This is a session of normal gameplay. Players take turns
   placing tiles on the scrabble board. The game terminates when there
   are no tiles left, and the player with the highest score wins.

   [Loading_complete]: This is the state that clears the screen and
   draws the components to be viewed on the screen during game state
   [Complete].

   [Complete]: The game is over and there is winner.*)
