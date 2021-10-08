open Graphics

exception Exit

let main () =
  open_graph "";
  try
    while true do
      let st = wait_next_event [ Key_pressed ] in
      if st.keypressed then raise Exit
    done
  with
  | Exit -> ()

let () = main ()