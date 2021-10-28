open Graphics
open Game

(** [main ()] initializes and maintains the home screen and switches to
    the play screen when the button is pressed. If a key is pressed, the
    GUI closes. *)
let main () =
  try App.start () with
  | Game.App.Exit -> ()

let () = main ()