open Graphics
open Game_board
open State

exception Exit

(**[game_state] represents the state of the application*)
type app_state =
  | Loading_initial
  | Initial
  | Loading_active
  | Active of State.t
  | Loading_complete
  | Complete

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

   [Loading_active]: This is the state that initializes the scrabble
   game state and draws all of the initial game components to the GUI.
   The application switches to this state if the state was [Initial] in
   the previous frame, and the player pressed the play button. This
   state only lasts one animation frame before switching to [Active].

   [Active]: This is a session of normal gameplay. Players take turns
   placing tiles on the scrabble board. The game terminates when there
   are no tiles left, and the player with the highest score wins.

   [Loading_complete]: This is the state that clears the screen and
   draws the components to be viewed on the screen during game state
   [Complete].

   [Complete]: The game is over and there is winner.*)
let rec update (st : app_state) =
  match st with
  | Loading_initial -> loading_initial_update ()
  | Initial -> initial_update ()
  | Loading_active -> loading_active_update ()
  | Active state -> active_update state
  | Loading_complete -> loading_complete_update ()
  | Complete -> complete_update ()

(**[loading_initial_update st] updates the game during game state
   [Loading_initial].*)
and loading_initial_update () =
  open_graph "";
  resize_window 900 625;
  set_window_title "Scrabble";

  set_color 0x8282E8;
  let vertices = [| (350, 275); (350, 350); (550, 350); (550, 275) |] in
  draw_poly vertices;
  fill_poly vertices;

  set_color black;
  moveto 300 250;
  draw_string "Let's Play Scrabble! Click the Play Button to Start.";
  moveto 390 225;
  draw_string "Press Any Key to Exit";

  moveto 440 305;
  draw_string "PLAY";
  update Initial

(**[initial_update st] updates the game during game state [Initial].*)
and initial_update () =
  let sta = wait_next_event [ Button_down; Key_pressed ] in
  if sta.keypressed then raise Exit;
  if sta.button then
    if
      sta.mouse_x > 350 && sta.mouse_x < 550 && sta.mouse_y > 275
      && sta.mouse_y < 350
    then update Loading_active
    else initial_update ()

(**[loading_active_update st] updates the game during game state
   [Loading_active].*)
and loading_active_update () =
  clear_graph ();
  Game_board.make_board ();
  update (Active (State.init ()))

(**[active_update st] updates the game during game state [Active].*)
and active_update st =
  (* Terminate the active state when the game is over. *)
  if State.game_over st = true then update Loading_complete
  else
    let status1 = wait_next_event [ Key_pressed; Button_down ] in
    (* Raise Exit when key pressed. *)
    if status1.keypressed then raise Exit
    else if status1.button then (
      (* Wait for mouse to not be clicked to prevent double clicking. *)
      let status2 = wait_next_event [ Button_up ] in
      (* Update state according to where mouse clicked. *)
      let state = State.click status2.mouse_x status2.mouse_y st in
      (* Draw the updated state. *)
      State.draw state;
      (* Recurse on the updated state. *) active_update state)

(**[loading_complete_update st] updates the game during game state
   [Loading_complete].*)
and loading_complete_update st = ()

(**[complete_update st] updates the game during game state [Complete].*)
and complete_update st = ()

let start () = update Loading_initial