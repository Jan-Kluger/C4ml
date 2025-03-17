open C4_lib.C4_types
open Test_players.Ex_player0
open Test_players.Ex_player1

(* Functor takes in two players to play game of connect 4 *)
module GAME (A : PLAYER_SIG) (B : PLAYER_SIG) : sig
  
  (* Main engine of the game. returns the chip of the winning player *)
  val run_game : ?turn:chip -> board -> chip


end = struct
  (* method to print board for immersion *)
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
  
    let rec game_helper (turn : chip) (state : board) : chip =
    (* Get the collumn that the player will drop a chip at *)
    let to_drop = 
      match turn with
      | A -> A.make_move state
      | B -> B.make_move state
      | _ -> failwith "tun can only be either a or b"
    in
    
    (* Temporarily return -1 *)
    A

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