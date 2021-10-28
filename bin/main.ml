open Game

(** [main ()] starts the game, and it closes the game when a key is
    pressed. *)
let main () =
  try App.start () with
  | App.Exit -> ()

let () = main ()