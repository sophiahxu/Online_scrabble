open Graphics
open Yojson.Basic.Util

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
  memory_stack : t list;
  removed : t list;
  tiles : t list;
}

type key = {
  colors : (int * bonus_type) list;
  letters : (string * int) list;
}
(** Represents the scoring key for scrabble. *)

(** [key] represents the scrabble scoring key. *)
let key : key =
  let key_assoc =
    Yojson.Basic.from_file "data/key.json"
    |> to_assoc
    |> List.map (fun (k, v) ->
           ( k,
             v |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))
           ))
  in
  {
    colors =
      List.assoc "colors" key_assoc
      |> List.map (fun (k, v) ->
             ( int_of_string k,
               match v with
               | "TripleWord" -> TripleWord
               | "TripleLetter" -> TripleLetter
               | "DoubleWord" -> DoubleWord
               | "DoubleLetter" -> DoubleLetter
               | _ -> failwith "invalid input" ));
    letters =
      List.assoc "letters" key_assoc
      |> List.map (fun (k, v) -> (k, int_of_string v));
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
  let lst =
    Yojson.Basic.from_file "data/board.json"
    |> to_assoc
    |> List.assoc "triple_word"
    |> to_list |> List.map to_string
  in
  if List.mem tile.name lst then { tile with bonus = TripleWord }
  else tile

(**[triple_letter tile] is a list of tiles with the correct tiles being
   given the correct TripleLetter bonus value. Requires: [tile] is a
   valid list of tiles.*)
let triple_letter tile =
  let lst =
    Yojson.Basic.from_file "data/board.json"
    |> to_assoc
    |> List.assoc "triple_letter"
    |> to_list |> List.map to_string
  in
  if List.mem tile.name lst then { tile with bonus = TripleLetter }
  else tile

(**[double_word tile] is a list of tiles with the correct tiles being
   given the correct DoubleWord bonus value. Requires: [tile] is a valid
   list of tiles.*)
let double_word tile =
  let lst =
    Yojson.Basic.from_file "data/board.json"
    |> to_assoc
    |> List.assoc "double_word"
    |> to_list |> List.map to_string
  in
  if List.mem tile.name lst then { tile with bonus = DoubleWord }
  else tile

(**[double_letter tile] is a list of tiles with the correct tiles being
   given the correct DoubleLetter bonus value. Requires: [tile] is a
   valid list of tiles.*)
let double_letter tile =
  let lst =
    Yojson.Basic.from_file "data/board.json"
    |> to_assoc
    |> List.assoc "double_letter"
    |> to_list |> List.map to_string
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
    memory_stack = [];
    removed = [];
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

let memory_stack b = b.memory_stack

let blank_tiles r side =
  match r with
  | [] -> ()
  | h :: t ->
      set_color white;
      fill_rect (tile_x h) (tile_y h) side side

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

let draw board =
  blank_tiles board.removed (side board);
  color_grid (tiles board) (side board);
  set_color black;
  grid (tiles board) (side board);
  letter_grid (tiles board)

(**[find_tile x y tiles side] is the tile from the list [tiles] that
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

(**[replace_tile tiles n new_letter placed] is the list of [tiles] where
   the tile with the name [n] now contains the letter [new_letter], and
   the field of turn has been updated accordingly. *)
let rec replace_tile tiles n new_letter =
  match tiles with
  | [] -> []
  | h :: t when h.name = n ->
      if new_letter = None then
        { h with letter = new_letter; turn = false } :: t
      else { h with letter = new_letter; turn = true } :: t
  | h :: t -> h :: replace_tile t n new_letter

let add_tile x y l board =
  let tile = find_tile x y (tiles board) (side board) in
  match tile with
  | None -> board
  | Some z ->
      {
        board with
        memory_stack = { z with letter = Some l } :: board.memory_stack;
        removed = [];
        tiles = replace_tile (tiles board) (name z) (Some l);
      }

let undo board =
  match board.memory_stack with
  | [] -> board
  | h :: t ->
      {
        board with
        memory_stack = t;
        removed = h :: board.removed;
        tiles = replace_tile board.tiles h.name None;
      }

let rec undo_all board =
  match board.memory_stack with
  | [] -> board
  | h :: t -> undo_all (undo board)

let clear_mem board = { board with memory_stack = []; removed = [] }

let rec words_in_list lst acc =
  match lst with
  | [] -> []
  | h :: t ->
      if h.letter != None then words_in_list t (h :: acc)
      else if List.length acc != 0 then acc :: words_in_list t []
      else words_in_list t acc

(**[horizontal_words board acc] adds the horizontal words in [board] to
   [acc] *)
let rec horizontal_words b acc =
  match b.tiles with
  | [] -> []
  | h :: t ->
      if List.length t mod 15 = 0 then words_in_list (h :: acc) []
      else horizontal_words b (h :: acc)

(**[vertical_words board row acc ready] adds the vertical words in
   [board] to [acc] *)
let vertical_words b row acc ready = failwith "unimplemented"
(*TODO: Grace*)

(*match b.tiles with | [] -> [] | h ::t -> if List.length acc = 15 then
  words_in_list (h :: acc) [] @ vertical_words b (Char.escaped Char.chr
  (row + 1)) [] false if List.length acc =(String.sub (name h) 0 1 = row
  then *)

(**[filter_placed lst] is [lst] but with only the words that contain a
   tile that has been placed this turn.*)
let filter_placed (lst : t list list) : t list list =
  failwith "unimplemented"
(*TODO: Grace*)

(**[letter_score word] is the score obtained from [word] disregarding
   word bonuses.*)
let rec letter_score (word : t list) : int =
  match word with
  | [] -> 0
  | h :: t ->
      let mult =
        match h.bonus with
        | TripleLetter -> 3
        | DoubleLetter -> 2
        | _ -> 1
      in
      let letter_value =
        List.assoc
          (match h.letter with
          | None -> failwith "Somethin' ain't right"
          | Some l -> l)
          key.letters
      in
      (mult * letter_value) + letter_score t

(**[word_score word] is the score obtained from [word].*)
let word_score (word : t list) : int = failwith "unimplemented"
(*TODO: Sophia, call letter_score as a helper*)

let score b =
  let words_list =
    horizontal_words b [] @ vertical_words b 1 [] false
  in
  words_list |> filter_placed
  |> List.fold_left (fun acc word -> acc + word_score word) 0
