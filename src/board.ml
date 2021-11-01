open Graphics

type bonus_type =
  | TripleWord
  | TripleLetter
  | DoubleWord
  | DoubleLetter
  | NoBonus

type t = {
  name : string;
  location : int * int;
  bonus : bonus_type;
  letter : string option;
  turn : bool;
}

type b = {
  side : int;
  tiles : t list;
}

(**[tile_row_setup s num x y row col] is a list of a row of [num] tiles
   that have a lower left corner at [x] [y]. Each tile has a name
   related to the values [row] and [col], which represent the row and
   column of the tile relative to the overall grid. Each tile has a
   length and height of [s]. Requires: [s], [num], [x], [y], [row],
   [col] >= 0. *)
let rec tile_row_setup s num x y row col =
  if num <= 0 then []
  else
    {
      name = Char.escaped (Char.chr (row + 96)) ^ string_of_int col;
      location = (x, y);
      bonus = NoBonus;
      letter = None;
      turn = false;
    }
    :: tile_row_setup s (num - 1) (x + s) y row (col + 1)

(**[tile_setup s num x y row col] is a list of [num] rows of tiles that
   overal has a lower left corner at [x] [y]. Each tile has a name
   related to the values [row] and [col], which represent the row and
   column of the tile relative to the overall grid. Each tile has a
   length and height of [s]. Requires: [s], [num], [x], [y], [row],
   [col] >= 0. *)
let rec tile_setup s num x y row col =
  if num <= 0 then []
  else
    tile_row_setup s 15 x y row col
    @ tile_setup s (num - 1) x (y + s) (row + 1) col

(**[triple_word tile] is a list of tiles with the correct tiles being
   given the correct TripleWord bonus value. Requires: [tile] is a valid
   list of tiles.*)
let triple_word tile =
  let lst = [ "a1"; "a8"; "a15"; "h1"; "h15"; "o1"; "o8"; "o15" ] in
  if List.mem tile.name lst then { tile with bonus = TripleWord }
  else tile

(**[triple_letter tile] is a list of tiles with the correct tiles being
   given the correct TripleLetter bonus value. Requires: [tile] is a
   valid list of tiles.*)
let triple_letter tile =
  let lst =
    [
      "b6";
      "b10";
      "f2";
      "f6";
      "f10";
      "f14";
      "j2";
      "j6";
      "j10";
      "j14";
      "n6";
      "n10";
    ]
  in
  if List.mem tile.name lst then { tile with bonus = TripleLetter }
  else tile

(**[double_word tile] is a list of tiles with the correct tiles being
   given the correct DoubleWord bonus value. Requires: [tile] is a valid
   list of tiles.*)
let double_word tile =
  let lst =
    [
      "b2";
      "b14";
      "c3";
      "c13";
      "d4";
      "d12";
      "e5";
      "e11";
      "h8";
      "k5";
      "k5";
      "k11";
      "l4";
      "l12";
      "m3";
      "m13";
      "n2";
      "n14";
    ]
  in
  if List.mem tile.name lst then { tile with bonus = DoubleWord }
  else tile

(**[double_letter tile] is a list of tiles with the correct tiles being
   given the correct DoubleLetter bonus value. Requires: [tile] is a
   valid list of tiles.*)
let double_letter tile =
  let lst =
    [
      "a4";
      "a11";
      "c7";
      "c9";
      "d1";
      "d8";
      "d15";
      "g3";
      "g7";
      "g9";
      "g13";
      "h4";
      "h12";
      "i3";
      "i7";
      "i9";
      "i13";
      "l1";
      "l8";
      "l15";
      "m7";
      "m9";
      "o4";
      "o12";
    ]
  in
  if List.mem tile.name lst then { tile with bonus = DoubleLetter }
  else tile

(**[tile_bonus tiles] adds the necessary bonus values to the list of
   [tiles]. Requires: [tiles] is a valid list of tiles.*)
let tile_bonus tiles =
  tiles |> List.map triple_word |> List.map triple_letter
  |> List.map double_word |> List.map double_letter

let init () =
  let start_x = 400 in
  let start_y = 120 in
  {
    side = 32;
    tiles = tile_setup 32 15 start_x start_y 1 1 |> tile_bonus;
  }

let side board = board.side

let tiles board = board.tiles

let name tile = tile.name

let tile_x tile =
  match tile.location with
  | x, _ -> x

let tile_y tile =
  match tile.location with
  | _, y -> y

let color tile =
  match tile.bonus with
  | TripleWord -> Some 0xFF0000
  | TripleLetter -> Some 0x0000FF
  | DoubleWord -> Some 0xE88282
  | DoubleLetter -> Some 0x8282E8
  | NoBonus -> None

let letter tile =
  match tile.letter with
  | Some x -> x
  | None -> ""

let turn tile = tile.turn

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

(**[letter_grid tiles] draws the letters as described by the list
   [tiles]. Requires: [tiles] is a valid list of tiles. *)
let rec letter_grid tiles =
  match tiles with
  | [] -> ()
  | h :: t when letter h != "" ->
      set_color 0xFCE283;
      fill_rect (tile_x h + 4) (tile_y h + 4) 24 24;
      moveto (tile_x h + 13) (tile_y h + 10);
      set_color black;
      draw_string (letter h);
      letter_grid t
  | _ :: t -> letter_grid t

(**[init_draw board] draws the initial board with the correct tile
   colors.*)
let draw board =
  color_grid (tiles board) (side board);
  set_color black;
  grid (tiles board) (side board);
  letter_grid (tiles board)

(**[location x y tiles side] is the tile from the list [tiles] that
   houses the coordinates [x], [y], where each of these tiles has the
   side length [side]. Requires: [side] is the side length of the tiles.
   [x], [y] >= 0. [tiles] is a valid list of tiles.*)
let rec find_tile x y tiles side =
  match tiles with
  | [] -> None
  | h :: t
    when x < tile_x h + side
         && x > tile_x h
         && y > tile_y h
         && y < tile_y h + side ->
      Some h
  | _ :: t -> find_tile x y t side

let clicked x y board =
  let tile = find_tile x y (tiles board) (side board) in
  match tile with
  | None -> false
  | Some x -> letter x = ""

let rec replace_tile tiles n new_letter =
  match tiles with
  | [] -> []
  | h :: t when h.name = n ->
      { h with letter = Some new_letter; turn = true } :: t
  | h :: t -> h :: replace_tile t n new_letter

let add_tile x y letter board =
  let tile = find_tile x y (tiles board) (side board) in
  match tile with
  | None -> board
  | Some z ->
      { board with tiles = replace_tile (tiles board) (name z) letter }
