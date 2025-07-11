open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let empty_board = init_game []

let print_game (game : Game.t) =
  let board_width = Game_kind.board_length game.game_kind in
  let board_contents =
    List.init board_width ~f:(fun row ->
        let row_contents =
          List.init board_width ~f:(fun column ->
              (match Map.find game.board { row; column } with
              | Some piece -> Piece.to_string piece
              | None -> " ")
              ^ if column < board_width - 1 then " | " else "")
        in
        List.fold row_contents ~init:"" ~f:(fun acc x -> acc ^ x))
  in
  List.iteri board_contents ~f:(fun i row ->
      print_endline row;
      let divider =
        String.concat (List.init board_width ~f:(fun _x -> "---"))
      in
      if i < board_width - 1 then print_endline divider)

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

let%expect_test "print_empty_board" =
  print_game empty_board;
  [%expect
    {|
        |   |
      ---------
        |   |
      ---------
        |   |
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let board_width = Game_kind.board_length game.game_kind in
  let center = board_width / 2 in
  (* let indexes = List.init board_width ~f:(fun x -> x) in
  let position_coords = List.cartesian_product indexes indexes in *)
  let possible_positions =
    List.map (Map.keys game.board) ~f:(fun pos ->
        List.map Position.all_offsets ~f:(fun offset -> offset pos))
  in
  let positions_unpacked =
    List.dedup_and_sort
      (List.concat possible_positions)
      ~compare:Position.compare
  in
  if List.length positions_unpacked = 0 then
    [ { row = center; column = center } ]
  else
    List.filter_map positions_unpacked ~f:(fun pos ->
        let { Position.row; Position.column = col } = pos in
        if (0 <= row && row < board_width) && 0 <= col && col < board_width then
          match Map.find game.board pos with
          | Some X | Some O -> None
          | None -> Some pos
        else None)

let%expect_test "available_win_for_x" =
  List.iter (available_moves win_for_x) ~f:(fun pos ->
      print_endline (Position.to_string pos));
  [%expect {|
      |}];
  return ()

let%expect_test "available_non_win" =
  List.iter (available_moves non_win) ~f:(fun pos ->
      print_endline (Position.to_string pos));
  [%expect
    {|
      ((row 0) (column 1))
      ((row 0) (column 2))
      ((row 1) (column 1))
      ((row 1) (column 2))
      ((row 2) (column 1))
      |}];
  return ()

let%expect_test "available_empty_board" =
  List.iter (available_moves empty_board) ~f:(fun pos ->
      print_endline (Position.to_string pos));
  [%expect
    {|
      ((row 0) (column 0))
      ((row 0) (column 1))
      ((row 0) (column 2))
      ((row 1) (column 0))
      ((row 1) (column 1))
      ((row 1) (column 2))
      ((row 2) (column 0))
      ((row 2) (column 1))
      ((row 2) (column 2))
      |}];
  return ()

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let num_available_moves = List.length (available_moves game) in
  let board_length = Game_kind.board_length game.game_kind in
  let expected_placed_pieces =
    (board_length * board_length) - num_available_moves
  in
  if expected_placed_pieces < Map.length game.board then Evaluation.Illegal_move
  else
    let winner =
      Map.fold game.board ~init:None
        ~f:(fun ~key:initial_pos ~data:initial_piece win_found ->
          (*return piece.t if winner exists*)
          match win_found with
          | Some piece -> Some piece
          | None ->
              List.fold Position.all_offsets ~init:None
                ~f:(fun win_found_in_dir dir ->
                  match win_found_in_dir with
                  | Some piece -> Some piece
                  | None ->
                    (* if bounds are within 5 pieces then return none *)
                      let rec check_next input_pos n =
                        if n = 0 then Some initial_piece
                        else
                          let cur_pos = dir input_pos in
                          let cur_piece = Map.find game.board cur_pos in
                          match cur_piece with
                          | Some piece ->
                              if Piece.equal piece initial_piece then
                                check_next cur_pos (n - 1)
                              else None
                          | None -> None
                      in
                      check_next initial_pos ((Game_kind.win_length game.game_kind) - 1)))
    in
    match winner with
    | Some _piece -> Evaluation.Game_over { winner }
    | None ->
        if num_available_moves = 0 then Evaluation.Game_over { winner }
        else Evaluation.Game_continues

let%expect_test "evaluate_win_for_x" =
  print_endline
    (match evaluate win_for_x with
    | Illegal_move -> "Illegal Move"
    | Game_continues -> "Game Continues"
    | Game_over { winner } -> (
        match winner with
        | Some piece -> "Game Over, winner = " ^ Piece.to_string piece
        | None -> "Game Over, winner = None"));
  [%expect {|
      Game Over, winner = X
      |}];
  return ()

let%expect_test "evaluate_non_win" =
  print_endline
    (match evaluate non_win with
    | Illegal_move -> "Illegal Move"
    | Game_continues -> "Game Continues"
    | Game_over _winner -> "Game Over, winner = ");
  [%expect {|
      Game Continues
      |}];
  return ()

let%expect_test "print_empty_board" =
  print_endline
    (match evaluate empty_board with
    | Illegal_move -> "Illegal Move"
    | Game_continues -> "Game Continues"
    | Game_over _winner -> "Game Over, winner = ");
  [%expect {|
      Game Continues
      |}];
  return ()

let test_move ~(me : Piece.t) ~(game : Game.t) (next_move : Position.t) : Game.t
    =
  {
    Game.game_kind = game.game_kind;
    Game.board = Map.add_exn game.board ~key:next_move ~data:me;
  }

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let possible_moves = available_moves game in
  List.filter possible_moves ~f:(fun new_move_pos ->
      let status = evaluate (test_move ~me ~game new_move_pos) in
      match status with
      | Game_over { winner } -> (
          match winner with Some piece -> Piece.equal piece me | _ -> false)
      | _ -> false)

let%expect_test "find_moves_win_for_x" =
  let expected_moves = winning_moves win_for_x ~me:Piece.O in
  print_s [%sexp (expected_moves : Position.t list)];
  [%expect {|
      ()
      |}];
  return ()

let%expect_test "find_moves_empty_board_for_o" =
  let expected_moves = winning_moves empty_board ~me:Piece.O in
  print_s [%sexp (expected_moves : Position.t list)];
  [%expect {|
      ()
      |}];
  return ()

let%expect_test "find_moves_non_win_for_o" =
  let expected_moves = winning_moves non_win ~me:Piece.O in
  print_s [%sexp (expected_moves : Position.t list)];
  [%expect {|
      ()
      |}];
  return ()

let%expect_test "find_moves_non_win_for_x" =
  let expected_moves = winning_moves non_win ~me:Piece.X in
  print_s [%sexp (expected_moves : Position.t list)];
  [%expect {|
      (((row 1) (column 1))) 
      |}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  winning_moves ~me:(Piece.flip me) game

let%expect_test "find_loss_moves_non_win_for_o" =
  let expected_moves = losing_moves non_win ~me:Piece.O in
  print_s [%sexp (expected_moves : Position.t list)];
  [%expect {|
      (((row 1) (column 1))) 
      |}];
  return ()

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate non_win in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

(* Exercise 6 *)
let available_moves_that_do_not_immediately_lose ~(me : Piece.t) (game : Game.t)
    : Position.t list =
  let all_moves = available_moves game in
  List.filter all_moves ~f:(fun new_move_pos ->
      List.length
        (losing_moves ~me
           {
             Game.game_kind = game.game_kind;
             Game.board = Map.add_exn game.board ~key:new_move_pos ~data:me;
           })
      = 0)

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  (* Exercise 7 *)
  let rec minimax ~(node : Game.t) ~(depth : int) ~(maximizingPlayer : bool) :
      int =
    match evaluate node with
    | Evaluation.Game_over { winner } -> (
        match winner with
        | Some piece ->
            if Piece.equal piece you_play then Int.max_value else Int.min_value
        | _ -> 0 (* tie *))
    | _ ->
        if depth = 0 then 0
        else
          let possible_moves = available_moves node in
          if maximizingPlayer then
            let values =
              List.map possible_moves ~f:(fun pos ->
                  minimax
                    ~node:(test_move ~me:you_play ~game:node pos)
                    ~depth:(depth - 1) ~maximizingPlayer:false)
            in
            Option.value_exn (List.max_elt values ~compare:Int.compare)
          else (* minimizing player *)
            let values =
              List.map possible_moves ~f:(fun pos ->
                  minimax
                    ~node:(test_move ~me:(Piece.flip you_play) ~game:node pos)
                    ~depth:(depth - 1) ~maximizingPlayer:true)
            in
            Option.value_exn (List.min_elt values ~compare:Int.compare)
  in
  let possible_first_moves = available_moves game in
  Option.value_exn
    (List.max_elt possible_first_moves ~compare:(fun x y ->
         let x_val =
           minimax
             ~node:(test_move ~me:you_play ~game x)
             ~depth:2 ~maximizingPlayer:false
         in
         let y_val =
           minimax
             ~node:(test_move ~me:you_play ~game y)
             ~depth:2 ~maximizingPlayer:false
         in
         Int.compare x_val y_val))

let exercise_five =
  Command.async ~summary:"Exercise 5: Is there a non-losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves =
         available_moves_that_do_not_immediately_lose ~me:piece non_win
       in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let make_move_test =
  Command.async ~summary:"Exercise 5: Is there a non-losing move?"
    (let%map_open.Command () = return () in
     fun () ->
       let position = make_move ~you_play:O ~game:non_win in
       print_s [%sexp (position : Position.t)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
      ("five", exercise_five);
      ("make-move", make_move_test);
    ]
