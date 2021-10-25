open Graphics
open Board

exception Exit
(** Raised when the GUI should be closed. *)

(**[graph l h] opens a graph with length [l] and height [h]. 
Requires: l, h > 0. *)
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

(**[color_grid tiles side] adds color to the rectangles as described by 
[tiles] by using the colors and locations in this list. Each rectangle that
is colored has a length and width of [side]. 
Requires: [tiles] is a valid list of tiles that can be colored. 
[side] > 0.*)
let rec color_grid tiles side =
  match tiles with 
  | [] -> ()
  | h :: t -> 
    match (color h) with 
    | Some x ->  set_color x;
    fill_rect (tile_x h) (tile_y h) side side;
    color_grid t side
    | None -> color_grid t side
    
(**[grid tiles side] draws the rectangles described by [tiles] 
using [side] as the length and width of each rectangle.
Requires: [tiles] is a valid list of tiles that can be drawn. 
[side] > 0.*)
let rec grid tiles side = 
  match tiles with 
  | [] -> ()
  | h :: t -> 
    draw_rect (tile_x h) (tile_y h) side side;
    grid t side

(*(**[alphabet_key x1 x2 top margin] draws the 26 letters of the alphabet
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
  color_key x1 top margin *)

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

(*[color_key color phrase x y w h] draws a rectangle with lower left corner 
at ([x],[y]) with width [w] and height [h] and colors it with [color]. 
[phrase] is displayed to the right of the rectangle*)
let color_key color phrase x y w h =
  set_color color; 
  draw_rect x y w h ;
  fill_rect x y w h;
  moveto (x + w) (y + h/2);
  set_color black;
  draw_string phrase

(*[letter_key letter number] prints a string with lower left corner at ([x],[y]) 
that associates [letter] with [number]*)
let letter_key letter number x y= 
  moveto x y;
  draw_string (Printf.sprintf "%s - %s" letter number)


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

  set_color black;
  draw_row (b_length * 1 / 15) (b_length * 1 / 15) 7 (b_length * 57/100)
   (b_height * 1 / 20);
  (*draws the tiles*) 

  draw_rect 30 30 125 565;
  moveto 50 575;
  draw_string "Key";
  color_key 0xFF0000 " -3x Word" 50 540 25 25;
  color_key 0x0000FF " -3x Letter" 50 510 25 25;
  color_key 0xE88282 " -2x Word" 50 480 25 25;
  color_key 0x8282E8 " -2x Letter" 50 450 25 25;
  letter_key "A" "1" 50 430;
  letter_key "B" "3" 50 415;
  letter_key "C" "3" 50 400;
  letter_key "D" "2" 50 385;
  letter_key "E" "1" 50 370;
  letter_key "F" "4" 50 355;
  letter_key "G" "2" 50 340;
  letter_key "H" "4" 50 325;
  letter_key "K" "5" 50 310;
  letter_key "L" "1" 50 295;
  letter_key "M" "3" 50 280;
  letter_key "N" "1" 50 265;
  letter_key "Q" "10" 50 250;
  letter_key "R" "1" 50 235;
  letter_key "U" "1" 50 220;
  letter_key "V" "4" 50 205;
  letter_key "O" "1" 50 190;
  letter_key "P" "3" 50 175;
  letter_key "S" "1" 50 160;
  letter_key "T" "1" 50 145;
  letter_key "W" "4" 50 130;
  letter_key "X" "8" 50 115;
  letter_key "Y" "4" 50 100;
  letter_key "Z" "10" 50 85;
  (*draws key box*)

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