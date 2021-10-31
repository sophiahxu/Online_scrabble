open OUnit2
open Game
open Board
open Players
open Bag

let id x = x

exception NotFoundError

(**[nth lst n] is the element of [lst] with index [n]. Raises:
   NotFoundError if [n] >= List.length lst.*)
let rec nth lst n =
  match lst with
  | [] -> raise NotFoundError
  | a :: _ when n = 0 -> a
  | _ :: b -> nth b (n - 1)

(**[string_of_int_option] is the string of int option [x].*)
let string_of_int_option x =
  match x with
  | Some e -> string_of_int e
  | None -> "None"

(**[side_test name expected_output b] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with side [b]*)
let side_test name expected_output b =
  name >:: fun _ ->
  assert_equal expected_output (side b) ~printer:string_of_int

(**[name_test n expected_output t] constructs an OUnit test named [n]
   that asserts the quality of [expected_output] with name [t]*)
let name_test n expected_output t =
  n >:: fun _ -> assert_equal expected_output (name t) ~printer:id

(**[tile_x_test name expected_output t] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with tile_x [t]*)
let tile_x_test name expected_output t =
  name >:: fun _ ->
  assert_equal expected_output (tile_x t) ~printer:string_of_int

(**[tile_y_test name expected_output t] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with tile_y [t]*)
let tile_y_test name expected_output t =
  name >:: fun _ ->
  assert_equal expected_output (tile_y t) ~printer:string_of_int

(**[color_test name expected_output t] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with color [t]*)
let color_test name expected_output t =
  name >:: fun _ ->
  assert_equal expected_output (color t) ~printer:string_of_int_option

(**[letter_test name expected_output t] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with letter [t]*)
let letter_test name expected_output t =
  name >:: fun _ ->
  assert_equal expected_output (letter t) ~printer:Char.escaped

let board = Board.init ()

let tile_list = tiles board

let first_tile = nth tile_list 0

let fortyfifth_tile = nth tile_list 45

let last_tile = nth tile_list (List.length tile_list - 1)

let board_tests =
  [
    side_test "New board has side of 32" 32 board;
    name_test "First element of tile list has name 'a1'" "a1" first_tile;
    name_test "Index 45 of tile list has name 'd1'" "d1" fortyfifth_tile;
    name_test "Last element of tile list has name 'o15'" "o15" last_tile;
    tile_x_test "First element of tile list has x value 400" 400
      first_tile;
    tile_x_test "Index 45 of tile list has x value 400" 400
      fortyfifth_tile;
    tile_x_test "Last element of tile list has x value 848" 848
      last_tile;
    tile_y_test "First element of tile list has y value 120" 120
      first_tile;
    tile_y_test "Index 45 of tile list has y value 216" 216
      fortyfifth_tile;
    tile_y_test "Last element of tile list has y value 568" 568
      last_tile;
    color_test "First element of tile list has color 0xFF0000"
      (Some 0xFF0000) first_tile;
    color_test "Index 3 of tile list has color 0x8282E8" (Some 0x8282E8)
      (nth tile_list 3);
    color_test "Index 16 of tile list has color 0xE88282"
      (Some 0xE88282) (nth tile_list 16);
    color_test "Index 20 of tile list has color 0x0000FF"
      (Some 0x0000FF) (nth tile_list 20);
    color_test "Index 2 of tile list has color None" None
      (nth tile_list 2);
    letter_test "First element of tile list has letter ' '" ' '
      first_tile;
  ]

(*[player_points name expected_output player] constructs an OUnit test
  named [name] that asserts the quality of [expected_output] with the
  points of [player]*)
let player_points name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (player_points player)
    ~printer:string_of_int

(*[player_names name expected_output player] constructs an OUnit test
  named [name] that asserts the quality of [expected_output] with the
  name of [player]*)
let player_names name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (player_name player) ~printer:(fun x ->
      x)

(*[num_tiles name expected_output player] constructs an OUnit test named
  [name] that asserts the quality of [expected_output] with the number
  of tiles of [player]*)
let num_tiles name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (num_tiles player) ~printer:string_of_int

let player = Players.init "Player 1"

let empty_player = Players.init ""

let player2 = add_points player 10

let player1t = add_tile player2 "A"

let player2t = add_tile player1t "B"

let player3t = add_tile player2t "C"

let player4t = add_tile player3t "D"

let player5t = add_tile player4t "E"

let player6t = add_tile player5t "F"

let player7t = add_tile player6t "G"

let player8t = add_tile player7t "H"

let player_removet = remove_tile player7t 456

let players_tests =
  [
    player_points "New player has 0 points" 0 player;
    ( "New player has name 'Player 1'" >:: fun _ ->
      assert_equal "Player 1" (player_name player) ~printer:id );
    player_points "New player with 10 points added has 10 points" 10
      player2;
    player_names "player has name Player 1" "Player 1" player;
    player_names "empty string player" "" empty_player;
    num_tiles "player with no tiles" 0 player;
    num_tiles "player with 1 tile" 1 player1t;
    num_tiles "player with 2 tiles" 2 player2t;
    num_tiles "player with 3 tiles" 3 player3t;
    num_tiles "player with 4 tiles" 4 player4t;
    num_tiles "player with 5 tiles" 5 player5t;
    num_tiles "player with 6 tiles" 6 player6t;
    num_tiles "player with 7 tiles" 7 player7t;
    num_tiles "adding a tile onto 7 occupied tiles" 7 player8t;
    num_tiles "removing a tile" 6 player_removet;
  ]

(**[count_test name expected_output b] constructs an OUnit test named
   [name] that asserts the quality of [expected_output] with count [b]*)
let count_test name expected_output b =
  name >:: fun _ ->
  assert_equal expected_output (count b) ~printer:string_of_int

(**[count_letter_test name expected_output b] constructs an OUnit test
   named [name] that asserts the quality of [expected_output] with
   count_letter [b] [letter]*)
let count_letter_test name expected_output b letter =
  name >:: fun _ ->
  assert_equal expected_output (count_letter b letter)
    ~printer:string_of_int

let bag = init ()

let bag2 = remove bag "A"

let bag3 = remove bag2 "Z"

let bag_tests =
  [
    count_test "New bag has 98 letters" 98 bag;
    count_letter_test "New bag has 9 'A's" 9 bag "A";
    count_test "Bag with 1 'A' removed has 97 letters" 97 bag2;
    count_letter_test "Bag with 1 'A' removed has 8'A's" 8 bag2 "A";
    count_test "Bag with 'A' and 'Z' removed has 96 letters" 96 bag3;
    count_letter_test "Bag with 'A' and 'Z' removed has 0 'Z's" 0 bag3
      "Z";
  ]

let tests =
  "test suite for scrabble game"
  >::: List.flatten [ board_tests (*players_tests*) ]

let _ = run_test_tt_main tests