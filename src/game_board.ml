open Graphics

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

(**[draw_grid w h x_num y_num x y] draws a grid of rectangles that
are each width [w] and height [h]. The overall grid is [x_num] rectangles 
long and [y_num] rectangles tall. The lowerleft corner of the entire grid is 
at ([x], [y]). 
Requires: [w], [h], [x_num], [y_num] > 0. [x], [y] >= 0.*)
let rec draw_grid w h x_num y_num x y= 
  if y_num <= 0 then ()
  else
    draw_row w h x_num x y;
    if y_num > 0 then draw_grid w h x_num (y_num - 1) x (y + h)

(**[triple_word h l x y] is an array containing information about the locations
and colors that need to be filled in with the color for the triple word
score tiles. Each tile is of height [h] and length [l], and the lower left
corner of the entire board is at ([x], [y]). Each entry in the array is 
of the format ((x : int), (y : int), color : int). *)
let triple_word h l x y = 
  let color = 0xFF0000 in 
  let row0 = y in 
  let row7 = y + 7 * h in 
  let row14 = y + 14 * h in 
  let col0 = x in 
  let col7 = x + 7 * l in 
  let col14 = x + 14 * l in 

  [((col0, row0), color); ((col0, row7), color); 
  ((col0, row14), color); ((col7, row0), color);
  ((col7, row14), color); ((col14, row0), color);
  ((col14, row7), color); ((col14, row14), color);]

  (**[triple_letter h l x y] is an array containing information about the 
  locations and colors that need to be filled in with the color for the triple 
  letter score tiles. Each tile is of height [h] and length [l], and the lower
  left corner of the entire board is at ([x], [y]). Each entry in the array is 
  of the format ((x : int), (y : int), color : int). *)
let triple_letter h l x y = 
  let color = 0x0000FF in 
  let row1 = y + h in 
  let row5 = y + 5 * h in 
  let row9 = y + 9 * h in 
  let row13 = y + 13 * h in 

  let col1 =  x + l in 
  let col5 = x + 5 * l in 
  let col9 = x + 9 * l in 
  let col13 = x + 13 * l in 

  [((col5, row1), color); ((col9, row1), color); ((col1, row5), color); 
  ((col5, row5), color);  ((col9, row5), color);
  ((col13, row5), color); ((col5, row9), color); ((col1, row9), color); 
  ((col9, row9), color); ((col13, row9), color); ((col5, row13), color); 
  ((col9, row13), color);]

  (**[double_word h l x y] is an array containing information about the 
  locations and colors that need to be filled in with the color for the double 
  word score tiles. Each tile is of height [h] and length [l], and the lower
  left corner of the entire board is at ([x], [y]). Each entry in the array is 
  of the format ((x : int), (y : int), color : int). *)
  let double_word h l x y = 
    let color = 0xE88282 in 
    let row1 = y + h in 
    let row2 = y + 2 * h in 
    let row3 = y + 3 * h in 
    let row4 = y + 4 * h in 
    let row7 = y + 7 * h in 
    let row10 = y + 10 * h in 
    let row11 = y + 11 * h in 
    let row12 = y + 12 * h in 
    let row13 = y + 13 * h in 
  
    let col1 =  x + l in 
    let col2 = x + 2 * l in 
    let col3 = x + 3 * l in 
    let col4 = x + 4 * l in 
    let col7 = x + 7 * l in 
    let col10 = x + 10 * l in 
    let col11 = x + 11 * l in 
    let col12 = x + 12 * l in 
    let col13 = x + 13 * l in 
  
    [((col1, row1), color); ((col13, row1), color); ((col2, row2), color);
    ((col12, row2), color); ((col3, row3), color); ((col11, row3), color);
    ((col4, row4), color); ((col10, row4), color); ((col7, row7), color);
    ((col4, row10), color); ((col10, row10), color); ((col3, row11), color);
    ((col11, row11), color); ((col2, row12), color); 
    ((col12, row12), color);
    ((col1, row13), color); ((col13, row13), color);]

(**[double_letter h l x y] is an array containing information about the 
  locations and colors that need to be filled in with the color for the double 
  letter score tiles. Each tile is of height [h] and length [l], and the lower
  left corner of the entire board is at ([x], [y]). Each entry in the array is 
  of the format ((x : int), (y : int), color : int). *)
let double_letter h l x y = 
  let color = 0x8282E8 in 
  let row0 = y in 
  let row2 = y + 2 * h in 
  let row3 = y + 3 * h in 
  let row6 = y + 6 * h in 
  let row7 = y + 7 * h in 
  let row8 = y + 8 * h in 
  let row11 = y + 11 * h in 
  let row12 = y + 12 * h in 
  let row14 = y + 14 * h in 

  let col0 = x in 
  let col2 = x + 2 * l in 
  let col3 = x + 3 * l in 
  let col6 = x + 6 * l in 
  let col7 = x + 7 * l in 
  let col8 = x + 8 * l in 
  let col11 = x + 11 * l in 
  let col12 = x + 12 * l in 
  let col14 = x + 14 * l in

  [((col3, row0), color); ((col11, row0), color);
  ((col6, row2), color); ((col8, row2), color);
  ((col0, row3), color); ((col7, row3), color);
  ((col14, row3), color); ((col2, row6), color);
  ((col6, row6), color); ((col8, row6), color);
  ((col12, row6), color); ((col3, row7), color);
  ((col11, row7), color); ((col2, row8), color);
  ((col6, row8), color); ((col8, row8), color);
  ((col12, row8), color); ((col0, row11), color);
  ((col7, row11), color); ((col14, row11), color);
  ((col6, row12), color); ((col8, row12), color);
  ((col3, row14), color); ((col11, row14), color);]

(**[color_list h l x y] is an array of the necessary locations and colors 
used to fill in the grid made up of squares of height [h] and length [l], 
where the lower left corner of the grid is at [x] [y]. Each entry in the array 
is of the format ((x : int), (y : int), color : int). *)
let color_list h l x y = 
  triple_word h l x y @ double_letter h l x y @ double_word h l x y @ 
  triple_letter h l x y

(**[color_grid lst l h] fills in the grid at the locations and with the colors
described in [lst]. Each unit of the grid has length [l] and height [h].*)
let rec color_grid lst l h = 
  match lst with 
  | [] -> () 
  | (((x, y), col) :: t) -> 
    set_color col; 
    fill_rect x y h l ; 
    color_grid t l h 

(**[grid l h] draws the grid of rectangles with squares that are (2/50) of 
[l]. The lower left corner of this grid is also dependent on [l] and [h]. 
Requires: l, h > 0.*)
let grid l h = 
  let length = l * 2 / 50 in 
  let height = length in 
  let start_x = l * 1 / 2 in 
  let start_y = h * 1 / 5 in 
  set_color black;
  draw_grid length height 15 15 start_x start_y;
  (*draws lines of board without colors*)

  let colors = color_list length height start_x start_y in 
  color_grid colors length height
  (*adds colors*) 

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

  graph b_length b_height ;
  (*sets up graph*)

  grid b_length b_height;
  (*draws colored grid*)

  set_color black;
  draw_grid (b_length * 1 / 15) (b_length * 1 / 15) 7 1 (b_length * 57/100)
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