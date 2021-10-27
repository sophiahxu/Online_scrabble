open Graphics
open Board

exception Exit
(** Raised when the GUI should be closed. *)

(**[draw_row w h num x y] draws a row of [num] rectangles of width [w]
   and height [h]. The lower left corner of the row begins at the
   coordinates ([x], [y]) Requires: [w], [h] > 0. [num], [x], [y] >= 0.*)
let rec draw_row w h num x y =
  if num <= 0 then () else draw_rect x y w h;
  if num > 0 then draw_row w h (num - 1) (x + w) y

(**[color_grid tiles side] adds color to the rectangles as described by
   [tiles] by using the colors and locations in this list. Each
   rectangle that is colored has a length and width of [side]. Requires:
   [tiles] is a valid list of tiles that can be colored. [side] > 0.*)
let rec color_grid tiles side =
  match tiles with
  | [] -> ()
  | h :: t -> (
      match color h with
      | Some x ->
          set_color x;
          fill_rect (tile_x h) (tile_y h) side side;
          color_grid t side
      | None -> color_grid t side)

(**[grid tiles side] draws the rectangles described by [tiles] using
   [side] as the length and width of each rectangle. Requires: [tiles]
   is a valid list of tiles that can be drawn. [side] > 0.*)
let rec grid tiles side =
  match tiles with
  | [] -> ()
  | h :: t ->
      draw_rect (tile_x h) (tile_y h) side side;
      grid t side

(**[player_boxes l h num] draws [num] player boxes depending on the
   length [l] and height [h] of the entire board. Requires: [l], [h] >
   0. 2 <= [num] <= 4.*)
let rec player_boxes l h num =
  if num = 0 then ()
  else
    let x = l * 13 / 50 in
    let y = (h * 1 / 6 * (4 - num)) + (h * 3 / 10) in
    draw_rect x y (l * 1 / 5) (h * 3 / 20);
    moveto (x + (l / 50)) (y + (h * 3 / 25));
    draw_string ("Player " ^ string_of_int num);
    if num > 0 then player_boxes l h (num - 1)

(*[color_key color phrase x y w h] draws a rectangle with lower left
  corner at ([x],[y]) with width [w] and height [h] and colors it with
  [color]. [phrase] is displayed to the right of the rectangle*)
let color_key color phrase x y w h =
  set_color color;
  draw_rect x y w h;
  fill_rect x y w h;
  moveto (x + w) (y + (h / 2));
  set_color black;
  draw_string phrase

(*[letter_key letter number] prints a string with lower left corner at
  ([x],[y]) that associates [letter] with [number]*)
let letter_key letter number x y =
  moveto x y;
  draw_string (Printf.sprintf "%s - %s" letter number)

(**[make board ()] sets up the initial board game, which includes the
   Scrabble board itself, a space for the current player's letters, a
   button to click to draw letters, and two spaces for player scores to
   be displayed. If a key is pressed, the GUI closes. *)
let make_board () =
  let b_length = 800 in
  (*must be a multiple of 100*)
  let b_height = 600 in

  (*must be a multiple of 100*)
  let board = board_setup b_length b_height in

  (*sets up graph*)
  grid (tiles board) (side board);
  color_grid (tiles board) (side board);

  (*draws colored grid*)
  set_color black;
  draw_row
    (b_length * 1 / 15)
    (b_length * 1 / 15)
    7
    (b_length * 57 / 100)
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
  letter_key "I" "1" 50 310;
  letter_key "J" "8" 50 295;
  letter_key "K" "5" 50 280;
  letter_key "L" "1" 50 265;
  letter_key "M" "3" 50 250;
  letter_key "N" "1" 50 235;
  letter_key "Q" "10" 50 220;
  letter_key "R" "1" 50 205;
  letter_key "U" "1" 50 190;
  letter_key "V" "4" 50 175;
  letter_key "O" "1" 50 160;
  letter_key "P" "3" 50 145;
  letter_key "S" "1" 50 130;
  letter_key "T" "1" 50 115;
  letter_key "W" "4" 50 100;
  letter_key "X" "8" 50 85;
  letter_key "Y" "4" 50 70;
  letter_key "Z" "10" 50 55;

  (*draws key box*)
  let num = 4 in
  (*number of players*)
  player_boxes b_length b_height num;

  (*draws player score boxes*)
  draw_circle (b_length * 18 / 50) (b_height * 3 / 20) (b_height / 10);
  moveto (b_length * 34 / 100) (b_height * 3 / 20);
  draw_string "DRAW";
  moveto (b_length * 31 / 100) (b_height * 6 / 50);
  draw_string "98 tiles left"
