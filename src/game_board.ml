open Graphics

exception Exit
(** Raised when the GUI should be closed. *)

(**[graph] opens the graph that will contain all of the other components 
of the board and screen. It also initializes the color to black.*)
let graph = 
  open_graph ""

let do_nothing = ()

(**[draw_row w h num] draws a row of [num] rectangles of width [w] and height 
[h]. The current point 
Requires: [w], [h], [num] > 0.*)
let rec draw_row w h num x y = 
  if num <= 0 then do_nothing
  else 
    draw_rect x y w h;
    if num > 0 then draw_row w h (num - 1) (x + w) y;
    moveto x y

(**[draw_grid w h x_num y_num] draws a grid of rectangles that
are each width [w] and height [h]. The overall grid is [x_num] rectangles 
long and [y_num] rectangles tall. 
Requires: [w], [h], [x_num], [y_num] > 0. *)
let rec draw_grid w h x_num y_num x y= 
  if y_num <= 0 then do_nothing
  else
    draw_row w h x_num x y;
    if y_num > 0 then draw_grid w h x_num (y_num - 1) x (y + h);
    moveto x y

let event_loop st =
  if st.keypressed then raise Exit

let make_board () = 
  graph;
  set_color black;
  draw_grid 25 25 15 15 200 60;
  (*draws the main board*)
  draw_grid 30 30 7 1 280 10;
  (*draws the tiles*)
  draw_grid 100 180 1 1 40 230;
  draw_grid 100 180 1 1 40 30;
  (*draws player score boxes*)

  moveto 68 385;
  draw_string "Player 1";
  moveto 68 185;
  draw_string "Player 2";
  (*write player titles*)
  loop_at_exit [Key_pressed] event_loop