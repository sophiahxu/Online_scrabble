open Game

(** [main_end ()] starts the game at a time close to the end of a
    scrabble game, and it closes the game when a key is pressed. *)
let main_end () =
  try App.start_end () with
  | App.Exit -> ()

let () = main_end ()