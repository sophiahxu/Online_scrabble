open Board

type info = { board : Board.b }

type t =
  | Loading_initial
  | Initial
  | Loading_active
  | Active of info
  | Loading_complete
  | Complete

let init_info () = { board = board_setup 0 0 }