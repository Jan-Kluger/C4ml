open C4_lib.C4_types

module Ex_player1 : PLAYER_SIG = struct
  (* Players that always plays collumn 1 *)
  let make_move (_state : board) : int =
    1
end