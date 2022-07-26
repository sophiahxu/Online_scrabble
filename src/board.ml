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

let remove_stack b = b.removed

(** [pp_string s] pretty-prints string [s]. *)
let pp_tile s = "\"" ^ s.name ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(**[blank_tile r side] draws white rectangles over the blank tiles in
   [r]. Each of these rectangles has the length and width [side].*)
let rec blank_tiles r side =
  let _ = print_endline (pp_list pp_tile r) in
  match r with
  | [] -> ()
  | h :: t ->
      set_color white;
      fill_rect (tile_x h) (tile_y h) side side;
      blank_tiles t side

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

(**[clear_tiles t] sets the turn variable to false for all of the tiles
   in list [t].*)
let rec clear_tiles = function
  | [] -> []
  | h :: t -> { h with turn = false } :: clear_tiles t

let clear_mem board =
  { board with tiles = clear_tiles board.tiles; memory_stack = [] }

(**[string_of_word word] is the string representation of [word].*)
let rec string_of_word = function
  | [] -> ""
  | h :: t ->
      (match h.letter with
      | Some l -> l
      | None -> failwith "this shouldn't happen")
      ^ string_of_word t

(**[words_in_list lst acc] is the words in [lst] and the words in [acc].
   A word is defined to have a length of at least 1.*)
let rec words_in_list lst acc =
  match lst with
  | [] -> []
  | h :: t ->
      if h.letter != None then words_in_list t (h :: acc)
      else if List.length acc > 1 then acc :: words_in_list t []
      else words_in_list t []

(**[horizontal_words board acc] is a list of the horizontal words in [b] *)
let rec horizontal_words b num =
  if num <= 15 then
    let row =
      b.tiles
      |> List.filter (fun x ->
             String.contains x.name (Char.chr (num + 96)))
    in
    let words = words_in_list row [] in
    words @ horizontal_words b (num + 1)
  else []

(**[vertical_words board row acc] creates the list of words in the
   vertical [row] in [board] and appends it onto the [acc].*)
let rec vertical_words b num =
  if num <= 15 then
    let column =
      b.tiles
      |> List.filter (fun x ->
             String.sub x.name 1 (String.length x.name - 1)
             = string_of_int num)
    in
    let words = words_in_list column [] in
    words @ vertical_words b (num + 1)
  else []

(**[placed_check tiles] checks whether or not there is a tile in [tiles]
   that has been placed during this turn.*)
let rec placed_check tiles =
  match tiles with
  | [] -> false
  | h :: t when turn h -> true
  | h :: t -> placed_check t

(**[filter_placed lst] is [lst] but with only the words that contain a
   tile that has been placed this turn.*)
let rec filter_placed (lst : t list list) : t list list =
  List.filter (fun x -> placed_check x) lst

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
      if h.turn then (mult * letter_value) + letter_score t
      else letter_value + letter_score t

(**[word_score word] is the score obtained from [word].*)
let rec word_score (word : t list) : int =
  let current_score = letter_score word in
  let rec multiplier w =
    match w with
    | [] -> 1
    | h :: t ->
        let mult =
          match h.bonus with
          | TripleWord -> 3
          | DoubleWord -> 2
          | _ -> 1
        in
        if h.turn then mult * multiplier t else multiplier t
  in
  multiplier word * current_score

let score b =
  let words_list = horizontal_words b 1 @ vertical_words b 1 in
  words_list |> filter_placed
  |> List.fold_left (fun acc word -> acc + word_score word) 0
