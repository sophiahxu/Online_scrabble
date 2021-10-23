open Graphics
open Board

exception Exit
(** Raised when the GUI should be closed. *)

(**[graph l h] opens a graph with length [l] and height [h]. 
Requires: l, h < 0. *)
let graph l h = 
  open_graph (" " ^ string_of_int l ^ "x" ^ string_of_int h) 

(**[draw_row w h num x y] draws a row of [num] rectangles of width [w] and 
height [h]. The lower left corner of the row begins at the coordinates
([x], [y])
Requires: [w], [h] > 0. [num], [x], [y] >= 0.*)
let rec draw_row w h num x y = 
  if num <= 0 then ()
  else 
    draw_rect x y w h;
    if num > 0 then draw_row w h (num - 1) (x + w) y

(**[color_grid lst l h] fills in the grid at the locations and with the colors
described in [lst]. Each unit of the grid has length [l] and height [h].*)
let rec color_grid tiles s =
  match tiles with 
  | [] -> ()
  | h :: t -> 
    match (color h) with 
    | Some x ->  set_color x;
    fill_rect (tile_x h) (tile_y h) s s;
    color_grid t s
    | None -> color_grid t s
    
let rec grid board_tiles side = 
  match board_tiles with 
  | [] -> ()
  | h :: t -> 
    draw_rect (tile_x h) (tile_y h) side side;
    grid t side

(**[alphabet_key x1 x2 top margin] draws the 26 letters of the alphabet
in the box for the key. The letters are arranged in two rows, one with an
x-value of [x1] and the other with [x2]. The top of the key box has the 
value [top] and [margin] is the space between the entries for different
letters. 
Requires: x1 < x2. x1, x2, top, margin > 0.*)
let alphabet_key x1 x2 top margin = 
  moveto x1 top;
  draw_string "Key";

  moveto x1 (top - margin);
  draw_string "A = 1";
  moveto x1 (top - 2 * margin);
  draw_string "B = 3";
  moveto x1 (top - 3 * margin);
  draw_string "C = 3";
  moveto x1 (top - 4 * margin);
  draw_string "D = 2";
  moveto x1 (top - 5 * margin);
  draw_string "E = 1";
  moveto x1 (top - 6 * margin);
  draw_string "F = 4";
  moveto x1 (top - 7 * margin);
  draw_string "G = 2";
  moveto x1 (top - 8 * margin);
  draw_string "H = 4";
  moveto x1 (top - 9 * margin);
  draw_string "I = 1";
  moveto x1 (top - 10 * margin);
  draw_string "J = 8";
  moveto x1 (top - 11 * margin);
  draw_string "K = 5";
  moveto x1 (top - 12 * margin);
  draw_string "L = 1";
  moveto x1 (top - 13 * margin);
  draw_string "M = 3";

  moveto x2 (top - margin);
  draw_string "N = 1";
  moveto x2 (top -  2 * margin);
  draw_string "O = 1";
  moveto x2 (top - 3 * margin);
  draw_string "P = 3";
  moveto x2 (top - 4 * margin);
  draw_string "Q = 10";
  moveto x2 (top - 5 * margin);
  draw_string "R = 1";
  moveto x2 (top - 6 * margin);
  draw_string "S = 1";
  moveto x2 (top - 7 * margin);
  draw_string "T = 1";
  moveto x2 (top - 8 * margin);
  draw_string "U = 1";
  moveto x2 (top - 9 * margin);
  draw_string "V = 4";
  moveto x2 (top - 10 * margin);
  draw_string "W = 4";
  moveto x2 (top - 11 * margin);
  draw_string "X = 8";
  moveto x2 (top - 12 * margin);
  draw_string "Y = 4";
  moveto x2 (top - 13 * margin);
  draw_string "Z = 10"

(**[color_key x top margin] draws the key for the colors on the board, where
this portion of the writing has the x-valueof [x], starts at the top line
with y-value [top], and [margin] is the space between the different entries. 
Requires: [x], [top], [margin] > 0.*)
let color_key x top margin = 
  moveto x top;
  draw_string "Dark blue = triple letter";

  moveto x (top - margin);
  draw_string "Light blue = triple letter score"

(**[key l h] draws the key for the board, using ratios dependent on the 
length [l] and height [h] of the overall board. 
Requires: [l], [h] > 0. *)
let key l h = 
  let left = l * 1 / 25 in 
  let bottom = h * 1 / 20 in 
  draw_rect left bottom (l * 1 / 5) (h * 9 / 10) ;

  let x1 = left + l / 50 in 
  let x2 = left + (l / 10) + l / 50 in 
  let margin = h * 1/25 in 
  let top = h * 23 / 25 in 

  alphabet_key x1 x2 top margin;
  moveto x1 (h * 9 / 25); 
  lineto (left + l / 5 - l / 50) (h * 9 / 25);
  let top = h * 7 / 25 in 
  color_key x1 top margin 

(**[player_boxes l h num] draws [num] player boxes depending on the length
[l] and height [h] of the entire board. 
Requires: [l], [h] > 0. 2 <= [num] <= 4.*)
let rec player_boxes l h num = 
  if num = 0 then () 
  else  
    let x = l * 13 / 50 in 
    let y = (h * 1 / 6) * (4 - num)  + (h * 3 / 10) in 
    draw_rect x y (l * 1 / 5) (h * 3 / 20);
    moveto (x + l / 50) (y + h * 3 / 25);
    draw_string ("Player " ^ string_of_int num);
    if num > 0 then player_boxes l h (num - 1)

(** [event_loop st] tracks any events that occur on the game board and exits
when a key is pressed. 
Raises: [Exit] if a key is pressed. *)
let event_loop st =
  if st.keypressed then raise Exit

(**[make board ()] sets up the initial board game, which includes the Scrabble
board itself, a space for the current player's letters, a button to click to
draw letters, and two spaces for player scores to be displayed. If a key is
pressed, the GUI closes. *)
let make_board () = 
  let b_length = 800 in (*must be a multiple of 100*)
  let b_height = 600 in (*must be a multiple of 100*)

  let board = board_setup b_length b_height in 
  graph b_length b_height ;
  (*sets up graph*)

  grid (tiles board) (side board);
  color_grid (tiles board) (side board);
  (*draws colored grid*)

  (*colorful b_length b_height; *)

  set_color black;
  draw_row (b_length * 1 / 15) (b_length * 1 / 15) 7 (b_length * 57/100)
   (b_height * 1 / 20);
  (*draws the tiles*) 

  key b_length b_height;
  (*draws key*)

  let num = 4 in 
  (*number of players*)
  player_boxes b_length b_height num;
  (*draws player score boxes*)

  draw_circle (b_length * 18/ 50) (b_height * 3 / 20) (b_height / 10); 
  moveto (b_length * 34 / 100) (b_height * 3 / 20);
  draw_string "DRAW";
  moveto (b_length * 31 / 100) (b_height * 6 / 50);
  draw_string "98 tiles left";
  (*draw bag to get new letters*)
  loop_at_exit [Key_pressed] event_loop