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
  let check_win (state : board) (last_drop : int) (p : chip) : bool =
    (* Get dimensions from the board *)
    let width = Array.length state in
    let height =
      match state.(0) with
      | Collumn (_, arr) -> Array.length arr
    in
    (* The dropped chip is at column last_drop and row (fill - 1) *)
    let Collumn(fill, _) = state.(last_drop) in
    let row = fill - 1 in
  
    (* Helper: get the chip at position i, j or Free if oob *)
    let get_cell i j =
      if i < 0 || i >= width || j < 0 || j >= height then Free
      else
        let Collumn(_, col) = state.(i) in
        col.(j)
    in
  
    (* Helper: given a list of chips, check for 4 consecutive p *)
    let check_line cells =
      let (_, win) =
        List.fold_left (fun (count, win) cell ->
          let count' = if cell = p then count + 1 else 0 in
          (count', win || count' >= 4)
        ) (0, false) cells
      in
      win
    in
  
    (* Vertical *)
    let vertical =
      let cells = List.init height (fun j -> get_cell last_drop j) in
      check_line cells
    in
  
    (* Horizontal *)
    let horizontal =
      let cells = List.init width (fun i -> get_cell i row) in
      check_line cells
    in
  
    (* Diagonal positive slope *)
    let diag_pos =
      let offset = min last_drop row in
      let start_col = last_drop - offset in
      let start_row = row - offset in
      let rec collect i j acc =
        if i >= width || j >= height then List.rev acc
        else collect (i + 1) (j + 1) (get_cell i j :: acc)
      in
      check_line (collect start_col start_row [])
    in
  
    (* Diagonal negative slope*)
    let diag_neg =
      let offset = min last_drop (height - 1 - row) in
      let start_col = last_drop - offset in
      let start_row = row + offset in
      let rec collect i j acc =
        if i >= width || j < 0 then List.rev acc
        else collect (i + 1) (j - 1) (get_cell i j :: acc)
      in
      check_line (collect start_col start_row [])
    in
  
    vertical || horizontal || diag_pos || diag_neg
  
  (* Main engine of game *)
  let rec game_helper (turn : chip) (state : board) : chip =
  (* Check if game is a draw *)
  let total_chips = Array.fold_left (fun acc (Collumn(chip_count, _)) -> acc + chip_count) 0 state in
  if total_chips >= 49 then Free else 

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
  if fill >= 7 then B else begin
    (* Otherwise we drop chip into collumn *)
    current_col.(fill) <- turn;
    let new_collumn = Collumn(fill + 1, current_col) in
    state.(to_drop) <- new_collumn;
    
    (* Print board after drop *)
    print_endline ("-");
    print_board state;

    (* Check if the last drop won the game *)
    if check_win state to_drop turn then 
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
let (state : board) = Array.init 7 (fun _ -> Collumn (0, Array.make 7 Free))

let () =
  let winner = Test_game.run_game state in
  if winner = Free then
    print_endline "DRAW"
  else
    let w_str = match winner with
      | A -> "A"
      | B -> "B"
      | _ -> failwith "already checked Free condition"
    in
    print_endline ("-");
    print_endline (w_str ^ " Wins!")