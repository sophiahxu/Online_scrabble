open Graphics
open Game

exception Exit
(** Raised when the GUI should be closed. *)

(** [init ()] initializes the game with a welcoming message and button
    allows the player to play the game. *)
let init () =
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
  draw_string "PLAY"

(** [event_loop st] takes the player to the next screen if the button is
    pressed. Raises: [Exit] if a key is pressed. *)
let event_loop st =
  if st.keypressed then raise Exit;
  if st.button then
    if
      st.mouse_x > 350 && st.mouse_x < 550 && st.mouse_y > 275
      && st.mouse_y < 350
    then begin
      clear_graph ();
      Game_board.make_board ()
    end

(** [main ()] initializes and maintains the home screen and switches to
    the play screen when the button is pressed. If a key is pressed, the
    GUI closes. *)
let main () =
  init ();
  loop_at_exit [ Key_pressed; Button_down ] event_loop

let () = main ()