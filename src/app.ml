open Graphics
open Board
open State

exception Exit

let rec update (st : State.t) =
  match st with
  | Loading_initial -> loading_initial_update st
  | Initial -> initial_update st
  | Loading_active -> loading_active_update st
  | Active _ -> active_update st
  | Loading_complete -> loading_complete_update st
  | Complete -> complete_update st

(**[loading_initial_update st] updates the game during game state
   [Loading_initial].*)
and loading_initial_update st =
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
and initial_update st =
  let sta = wait_next_event [ Button_down; Key_pressed ] in
  if sta.keypressed then raise Exit;
  if sta.button then
    if
      sta.mouse_x > 350 && sta.mouse_x < 550 && sta.mouse_y > 275
      && sta.mouse_y < 350
    then update Loading_active
    else update st

(**[loading_active_update st] updates the game during game state
   [Loading_active].*)
and loading_active_update st =
  clear_graph ();
  Game_board.make_board ();
  update (Active (State.init_info ()))

(**[active_update st] updates the game during game state [Active].*)
and active_update st =
  let sta = wait_next_event [ Key_pressed ] in
  if sta.keypressed then raise Exit

(**[loading_complete_update st] updates the game during game state
   [Loading_complete].*)
and loading_complete_update st = ()

(**[complete_update st] updates the game during game state [Complete].*)
and complete_update st = ()

let start () = update Loading_initial