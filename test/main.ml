open OUnit2
open Game
open Board
open Players

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

let board = board_setup 500 300

let board2 = board_setup 0 0

let board3 = board_setup 942 317

let tile_list = tiles board

let first_tile = nth tile_list 0

let fortyfifth_tile = nth tile_list 45

let last_tile = nth tile_list (List.length tile_list - 1)

let board_tests =
  [
    side_test "Normal board: 500x300 has a side of 20" 20 board;
    side_test "Zero board: 0x0 has a side of 0" 0 board2;
    side_test "Rounding board: 942x317 has a side of 36" 37 board3;
    name_test "First element of tile list has name 'a1'" "a1" first_tile;
    name_test "Index 45 of tile list has name 'd1'" "d1" fortyfifth_tile;
    name_test "Last element of tile list has name 'o15'" "o15" last_tile;
    tile_x_test "First element of tile list has x value 250" 250
      first_tile;
    tile_x_test "Index 45 of tile list has x value 250" 250
      fortyfifth_tile;
    tile_x_test "Last element of tile list has x value 530" 530
      last_tile;
    tile_y_test "First element of tile list has y value 60" 60
      first_tile;
    tile_y_test "Index 45 of tile list has y value 100" 120
      fortyfifth_tile;
    tile_y_test "Last element of tile list has y value 530" 340
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

let player_points name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (player_points player)
    ~printer:string_of_int

let player = init_player "Player 1"

let player2 = add_points player 10

let players_tests =
  [
    player_points "New player has 0 points" 0 player;
    ( "New player has name 'Player 1'" >:: fun _ ->
      assert_equal "Player 1" (player_name player) ~printer:id );
    player_points "New player with 10 points added has 10 points" 10
      player2;
  ]

let tests =
  "test suite for scrabble game"
  >::: List.flatten [ board_tests; players_tests ]

let _ = run_test_tt_main tests