open Graphics
open State

exception Exit

(**[start] represents where the scrabble game should be initialized.*)
type start =
  | Default
  | End

(**[game_state] represents the state of the application*)
type app_state =
  | Loading_initial of start
  | Initial of start
  | Loading_active of start
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
  | Loading_initial strt -> loading_initial_update strt
  | Initial strt -> initial_update strt
  | Loading_active strt -> loading_active_update strt
  | Active state -> active_update state
  | Loading_complete -> loading_complete_update ()
  | Complete -> complete_update ()

(**[loading_initial_update st] updates the game during game state
   [Loading_initial].*)
and loading_initial_update strt =
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
  draw_string "Press 'Esc' to Exit";

  moveto 440 305;
  draw_string "PLAY";
  update (Initial strt)

(**[initial_update strt] updates the game during game state [Initial].*)
and initial_update strt =
  let sta = wait_next_event [ Button_down; Key_pressed ] in
  if sta.keypressed then
    if sta.key = '\027' then raise Exit else initial_update strt;
  if sta.button then
    if
      sta.mouse_x > 350 && sta.mouse_x < 550 && sta.mouse_y > 275
      && sta.mouse_y < 350
    then update (Loading_active strt)
    else initial_update strt

(**[loading_active_update strt] updates the game during game state
   [Loading_active] and initalizes the game to the starting point
   indicated by [strt].*)
and loading_active_update strt =
  clear_graph ();
  match strt with
  | Default ->
      let sta = State.init () in
      State.init_draw sta;
      update (Active sta)
  | End ->
      let sta = State.init_end () in
      State.init_draw sta;
      update (Active sta)

(**[active_update st] updates the game during game state [Active].*)
and active_update st =
  (* Terminate the active state when the game is over. *)
  if State.game_over st = true then update Loading_complete
  else
    let status1 = wait_next_event [ Key_pressed; Button_down; Poll ] in

    (* Raise Exit when key pressed. *)
    let inpt =
      if status1.keypressed then
        match read_key () with
        | '\027' -> raise Exit
        | '\r' -> Some Enter
        | 'z' -> Some Z
        | _ -> None
      else if status1.button then
        (* Wait for mouse to not be clicked to prevent double
           clicking. *)
        let status2 = wait_next_event [ Button_up ] in
        Some (Clicked (status2.mouse_x, status2.mouse_y))
      else None
    in
    let s = State.update inpt st in
    State.draw s inpt;
    active_update s

(**[loading_complete_update st] updates the game during game state
   according to [st] [Loading_complete].*)
and loading_complete_update st =
  clear_graph ();
  let draw_star x y r color =
    set_color color;
    (*trig values*)
    let cos72 = 0.3090170 in
    let sin72 = 0.9510565 in
    let cos18 = sin72 in
    let sin18 = cos72 in
    let cos54 = 0.5877853 in
    let sin54 = 0.8090170 in
    (*r and r'*)
    let rfloat = float_of_int r in
    let r'float = rfloat *. sin18 /. sin54 in
    (*triangle a*)
    let a0 =
      [|
        (-.rfloat *. cos18, rfloat *. sin18);
        (rfloat *. cos18, rfloat *. sin18);
        (0., -.r'float);
      |]
    in
    (*triangle b*)
    let b0 =
      [|
        (-.rfloat *. cos54, -.rfloat *. sin54);
        (r'float *. cos18, -.r'float *. sin18);
        (0., rfloat);
      |]
    in
    (*triangle c*)
    let c0 =
      [|
        (rfloat *. cos54, -.rfloat *. sin54);
        (-.r'float *. cos18, -.r'float *. sin18);
        (0., rfloat);
      |]
    in
    (*turn arrays to int*int arrays and translate them to center*)
    let translate (a, b) =
      ( (Float.round a |> int_of_float) + x,
        (Float.round b |> int_of_float) + y )
    in
    let a = Array.map translate a0 in
    let b = Array.map translate b0 in
    let c = Array.map translate c0 in
    fill_poly a;
    fill_poly b;
    fill_poly c
  in

  draw_star 450 400 180 0x0000FF;
  draw_star 450 400 160 0x8282E8;
  complete_update ()

(**[complete_update ()] updates the game during game state [Complete].*)
and complete_update () =
  let sta = wait_next_event [ Key_pressed ] in
  if sta.keypressed then
    if sta.key = '\027' then raise Exit else complete_update ()

let start () = update (Loading_initial Default)

let start_end () = update (Loading_initial End)