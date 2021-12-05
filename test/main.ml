open OUnit2
open Game
open Board
open Player
open Bag

let id x = x

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

exception NotFoundError

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
  assert_equal expected_output (Board.letter t) ~printer:id

(**[clicked_test name expected_output x y board] constructs an OUnit
   test named [name] that asserts the quality of [expected_output] with
   location [x], [y] on board [board]*)
let clicked_test name expected_output x y board =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.clicked x y board)
    ~printer:string_of_bool

(**[memory_stack_test name expected_output board] constructs an OUnit
   test named [name] that asserts the quality of [expected_output] with
   board [b]. *)
let memory_stack_test name expected_output b =
  name >:: fun _ ->
  assert_equal expected_output
    (List.map Board.letter (Board.memory_stack b))
    ~printer:(pp_list pp_string)

let board = Board.init ()

let tile_list = tiles board

let first_tile = List.nth tile_list 0

let fortyfifth_tile = List.nth tile_list 45

let last_tile = List.nth tile_list (List.length tile_list - 1)

let board2 = Board.add_tile 435 155 "A" board

let board3 = Board.add_tile 500 190 "B" board2

let board4 = Board.add_tile 550 220 "C" board3

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
      (List.nth tile_list 3);
    color_test "Index 16 of tile list has color 0xE88282"
      (Some 0xE88282) (List.nth tile_list 16);
    color_test "Index 20 of tile list has color 0x0000FF"
      (Some 0x0000FF) (List.nth tile_list 20);
    color_test "Index 2 of tile list has color None" None
      (List.nth tile_list 2);
    letter_test "First element of tile list has letter empty string" ""
      first_tile;
    clicked_test "0, 0 is not on the board" false 0 0 board;
    clicked_test "435, 155 is on the board" true 435 155 board;
    clicked_test
      "435, 155 is not on the board after a tile has been added \n\
      \    there" false 435 155 board2;
    clicked_test
      "500, 300 is on the board after a tile has been added at 435, 155"
      true 500 300 board2;
    clicked_test
      "405, 123 is on the board after a tile has beena added at 435, \
       155"
      true 405 123 board2;
    memory_stack_test "New board has empty memory stack " [] board;
    memory_stack_test "Board with tile 'A' has memory stack with 'A'"
      [ "A" ] board2;
    memory_stack_test
      "Board with tiles 'A' and 'B' has memory stack with 'A' and 'B'"
      [ "B"; "A" ] board3;
    memory_stack_test
      "Board with tiles 'A', 'B', and 'C' has memory stack with \n\
      \    'A', 'B', and 'C'" [ "C"; "B"; "A" ] board4;
    memory_stack_test
      "Board with tiles 'A', 'B' and 'C' with undo has memory stack \
       with \n\
      \    'A' and 'B'" [ "B"; "A" ] (Board.undo board4);
    memory_stack_test
      "Board with tiles 'A' and 'B' with undo has memory stack with 'A'"
      [ "A" ] (Board.undo board3);
    memory_stack_test
      "Board with tiles 'A' with undo has empty memory stack" []
      (Board.undo board2);
    memory_stack_test "Board with undo all has empty memory stack" []
      (Board.undo_all board3);
  ]

(**[player_points name expected_output player] constructs an OUnit test
   named [name] that asserts the quality of [expected_output] with the
   points of [player]*)
let player_points_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (player_points player)
    ~printer:string_of_int

(**[player_names name expected_output player] constructs an OUnit test
   named [name] that asserts the quality of [expected_output] with the
   name of [player]*)
let player_names_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (player_name player) ~printer:(fun x ->
      x)

(**[num_tiles name expected_output player] constructs an OUnit test
   named [name] that asserts the quality of [expected_output] with the
   number of tiles of [player]*)
let num_tiles_test name expected_output player =
  name >:: fun _ ->
  assert_equal expected_output (num_tiles player) ~printer:string_of_int

(**[pclicked_test name_expected output player x y] constructs an OUnit
   test named [name] that asserts the quality of [expected_output] with
   the clicked location [(x,y)] of [player]*)
let pclicked_test name expected_output player x y =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.clicked player x y)
    ~printer:string_of_bool

let player = Player.init "Player 1"

let empty_player = Player.init ""

let player2 = add_points 10 player

let player1t = add_tile player2 "A"

let player2t = add_tile player1t "B"

let player3t = add_tile player2t "C"

let player4t = add_tile player3t "D"

let player5t = add_tile player4t "E"

let player6t = add_tile player5t "F"

let player7t = add_tile player6t "G"

let player8t = add_tile player7t "H"

let player_removet = remove_tile player7t 457

let player_remove2t = remove_tile player_removet 510

let player_remove3t = remove_tile player_remove2t 563

let player_remove4t = remove_tile player_remove3t 616

let player_remove5t = remove_tile player_remove4t 669

let player_remove6t = remove_tile player_remove5t 722

let player_remove7t = remove_tile player_remove6t 775

let players_tests =
  [
    player_points_test "New player has 0 points" 0 player;
    ( "New player has name 'Player 1'" >:: fun _ ->
      assert_equal "Player 1" (player_name player) ~printer:id );
    player_points_test "New player with 10 points added has 10 points"
      10 player2;
    player_names_test "Player has name Player 1" "Player 1" player;
    player_names_test "Empty string player" "" empty_player;
    num_tiles_test "Player with no tiles" 0 player;
    num_tiles_test "Player with 1 tile" 1 player1t;
    num_tiles_test "Player with 2 tiles" 2 player2t;
    num_tiles_test "Player with 3 tiles" 3 player3t;
    num_tiles_test "Player with 4 tiles" 4 player4t;
    num_tiles_test "Player with 5 tiles" 5 player5t;
    num_tiles_test "Player with 6 tiles" 6 player6t;
    num_tiles_test "Player with 7 tiles" 7 player7t;
    num_tiles_test "Adding a tile onto 7 occupied tiles" 7 player8t;
    num_tiles_test "Removing a tile" 6 player_removet;
    num_tiles_test "Removing 2 tiles" 5 player_remove2t;
    num_tiles_test "Removing 3 tiles" 4 player_remove3t;
    num_tiles_test "Removing 4 tiles" 3 player_remove4t;
    num_tiles_test "Removing 5 tiles" 2 player_remove5t;
    num_tiles_test "Removing 6 tiles" 1 player_remove6t;
    num_tiles_test "Removing 7 tiles" 0 player_remove7t;
    pclicked_test "Player1t has non-empty tile at 456" true player1t 480
      35;
    pclicked_test "Player1t has empty tile at 509" false player1t 555 40;
    pclicked_test "Player1t has empty tile at 562" false player1t 563 42;
    pclicked_test "Player1t has empty tile at 615" false player1t 632 51;
    pclicked_test "Player1t has empty tile at 668" false player1t 669 42;
    pclicked_test "Player1t has empty tile at 721" false player1t 736 33;
    pclicked_test "Player1t has empty tile at 774" false player1t 783 44;
    pclicked_test "Player7t has non-empty at 509" true player7t 510 60;
    pclicked_test "Player7t has non-empty at 562" true player7t 567 60;
    pclicked_test "Player7t has non-empty at 615" true player7t 621 60;
    pclicked_test "Player7t has non-empty at 668" true player7t 671 60;
    pclicked_test "Player7t has non-empty at 721" true player7t 735 60;
    pclicked_test "Player7t has non-empty at 774" true player7t 783 60;
    pclicked_test "Player7t has non-empty tile at 774" true player7t 780
      43;
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

let bag4 = remove bag3 "A"

let bag5 = remove bag4 "A"

let bag6 = remove bag5 "A"

let bag7 = remove bag6 "A"

let bag8 = remove bag7 "A"

let bag9 = remove bag8 "A"

let bag10 = remove bag9 "A"

let bag11 = remove bag10 "A"

let bag_tests =
  [
    count_test "New bag has 98 letters" 98 bag;
    count_letter_test "New bag has 9 'A's" 9 bag "A";
    count_test "Bag with 1 'A' removed has 97 letters" 97 bag2;
    count_letter_test "Bag with 1 'A' removed has 8'A's" 8 bag2 "A";
    count_test "Bag with 'A' and 'Z' removed has 96 letters" 96 bag3;
    count_letter_test "Bag with 'A' and 'Z' removed has 0 'Z's" 0 bag3
      "Z";
    count_letter_test "Bag with 2 A's removed has 7 A's" 7 bag4 "A";
    count_letter_test "Bag with 3 A's removed has 6 A's" 6 bag5 "A";
    count_letter_test "Bag with 4 A's removed has 5 A's" 5 bag6 "A";
    count_letter_test "Bag with 5 A's removed has 4 A's" 4 bag7 "A";
    count_letter_test "Bag with 6 A's removed has 3 A's" 3 bag8 "A";
    count_letter_test "Bag with 7 A's removed has 2 A's" 2 bag9 "A";
    count_letter_test "Bag with 8 A's removed has 1 A" 1 bag10 "A";
    count_letter_test "Bag with 9 A's removed has 0 A's" 0 bag11 "A";
    count_test "Bag with all A's and Z's removed" 88 bag11;
  ]

let tests =
  "test suite for scrabble game"
  >::: List.flatten [ board_tests; players_tests; bag_tests ]

let _ = run_test_tt_main tests