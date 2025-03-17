open C4_lib.C4_types
open Test_players.Ex_player0
open Test_players.Ex_player1

(* Functor takes in two players to play game of connect 4 *)
module GAME (A : PLAYER_SIG) (B : PLAYER_SIG) : sig
  
  (* Main engine of the game. returns the chip of the winning player *)
  val run_game : ?turn:chip -> board -> chip

end = struct
  (* Method to print board for immersion *)
  let print_board (state : board) : unit =
    print_endline "GAME BOARD:";
  
    (* Get number of rows from the first column *)
    let num_rows =
      match state.(0) with
      | Collumn (_, arr) -> Array.length arr
    in
    (* Print each row from top (num_rows - 1) to bottom (0) *)
    for row = num_rows - 1 downto 0 do
      let line =
        Array.fold_left (fun acc (Collumn (_, col)) ->
          let symbol =
            match col.(row) with
            | Free -> "*"
            | A -> "A"
            | B -> "B"
          in
          acc ^ symbol ^ " "
        ) "" state
      in
      print_endline line
    done

  (* Method to check if last dropped chip was a winner *)
  let check_win (_state : board) (_last_drop : int) : bool =
    false
  
  (* Main engine of game *)
  let rec game_helper (turn : chip) (state : board) : chip =
  (* Get next player to make a move and*)
  (* Get the collumn that the player will drop a chip at *)
  let to_drop, next_player = 
    match turn with
    | A -> A.make_move state, B
    | B -> B.make_move state, A
    | _ -> failwith "tun can only be either a or b"
  in

  let col_d = state.(to_drop) in
  let Collumn(fill, current_col) = col_d in

  (* If player tries to cheat and rop on already full collumn, opponent wins *)
  if fill >= 6 then B else begin
    (* Otherwise we drop chip into collumn *)
    current_col.(fill) <- turn;
    let new_collumn = Collumn(fill + 1, current_col) in
    state.(to_drop) <- new_collumn;
    
    (* Check if the last drop won the game *)
    if check_win state to_drop then 
    (* If last srop was a winner, player that dropped won the game *)
      turn 
    else
    (* Otherwise let next player play *)
      game_helper next_player state
    

  end

  let run_game ?(turn : chip = A) (state : board) : chip =
    (* Print inital game state *)
    print_board state;
    (* Run game *)
    game_helper turn state
    
end

module Test_game = GAME (Ex_player0)(Ex_player1)

(* Initialize with an empty playing field *)
let (state : board) = Array.init 7 (fun _ -> Collumn (0, Array.make 6 Free))

let () =
  let _ = Test_game.run_game state in
  ()