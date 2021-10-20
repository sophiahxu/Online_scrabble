open Graphics
open Game

exception Exit

let init () =
  open_graph "";

  moveto 150 200;
  draw_string "Let's Play Scrabble! Click the Play Button to Start";
  set_text_size 35;
  set_color yellow;

  set_window_title "SCRABBLE";
  let vertices = [| (250, 250); (250, 350); (350, 300) |] in
  draw_poly vertices;
  fill_poly vertices

let event_loop st =
  if st.keypressed then raise Exit;
  if st.button then
    if
      st.mouse_x > 250 && st.mouse_x < 350 && st.mouse_y > 250
      && st.mouse_y < 350
    then Game_board.make_board ()

let main () =
  init ();
  loop_at_exit [ Key_pressed; Button_down ] event_loop

let () = main ()