open Graphics

(**[graph] opens the graph that will contain all of the other components 
of the board and screen.*)
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
    draw_row w h (num - 1) (x + w) y;
    do_nothing
  

(**[draw_grid w h x_num y_num] draws a grid of rectangles that
are each width [w] and height [h]. The overall grid is [x_num] rectangles 
long and [y_num] rectangles tall. 
Requires: [w], [h], [x_num], [y_num] > 0. *)
let rec draw_grid w h x_num y_num x y= 
  if y_num <= 0 then do_nothing
  else
    draw_row w h x_num x y;
    draw_grid w h x_num (y_num - 1) x (y + h);
    moveto x y

let make_board () = 
  graph;
  draw_grid 20 20 15 15 10 10