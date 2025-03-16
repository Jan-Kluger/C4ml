type chip =
  | A
  | B
  | Free

module type PLAYER_SIG = sig
  (* Player takes in playing field and returns collumn to drop chip *)
  val make_move : chip array array -> int
end