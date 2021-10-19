open Graphics

exception Exit

let main () =
  open_graph "";
  try
    moveto 150 200;
    draw_string "Let's Play Scrabble! Click the Play Button to Start";
    set_text_size 35;
    set_color yellow;

    set_window_title "SCRABBLE";
    let vertices = [| (250, 250); (250, 350); (350, 300) |] in
    draw_poly vertices;
    fill_poly vertices;
    let cursor = wait_next_event [ Button_down ] in
    if
      cursor.mouse_x > 250 && cursor.mouse_x < 350
      && cursor.mouse_y > 250 && cursor.mouse_y < 350
    then if cursor.button then open_graph "";
<<<<<<< HEAD
=======
    
>>>>>>> grace
    while true do
      let st = wait_next_event [ Key_pressed ] in
      if st.keypressed then raise Exit
    done
  with
  | Exit -> ()

let () = main ()