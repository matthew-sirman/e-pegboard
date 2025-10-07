open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Action = struct
  type t =
    | Add_player of Player.t
    | Generate_match
    | Finish_match of { court : int; winner : Court.Team.Side.t }
end

module Match_record = struct
  type t =
    { start_time : Time_ns.t
    ; end_time : Time_ns.t
    ; match_setup : Court.Match.t
    }
  [@@deriving equal]
end

module State = struct
  type t =
    { current_matches : (Time_ns.t * Court.t) Int.Map.t
    ; player_queue : Player.t list
    ; match_record : Match_record.t list
    }
  [@@deriving equal]
end

let draw_best_game ranked_games =
  (* we compute with a normalised softmax function with temperatue

     The function is described here:
     https://en.wikipedia.org/wiki/Softmax_function#Reinforcement_learning *)
  let temperature =
    (* temperature must be in range (0, 1] (exactly 0 is not allowed)
       Lower temperature means less good games are likely to be picked
       Higher temperature means less good games are unlikely to be picked *)
    0.75
  in
  let compare =
    Comparable.lift
      [%compare: float]
      ~f:(fun (_, penalty) -> penalty)
  in
  let%bind.Option (_, min) = List.min_elt ranked_games ~compare
  and (_, max) = List.max_elt ranked_games ~compare in
  let normalised =
    List.map
      ranked_games
      ~f:(fun (game, penalty) -> (game, penalty /. (max -. min)))
  in
  let softmax_weights =
    let pre_weights =
      let factor =
        let sample_size = List.length ranked_games |> Float.of_int in
        -1. *. temperature *. Float.log (sample_size +. 1.)
         /. (1. -. temperature)
      in
      List.map
        normalised
        ~f:(fun (game, xi) ->
            (game, Float.exp (xi *. factor)))
    in
    let base =
      List.sum (module Float) pre_weights ~f:(fun (_, pre_w) -> pre_w)
    in
    List.map pre_weights ~f:(fun (game, pre_w) -> (game, pre_w /. base))
  in
  (* The [softmax_weights] now add to 1. We draw from them by lining them
     up as segments from [0-1] on the real number line, and randomly
     choosing a number from 0 to 1. Whichever segment the random number
     falls in is selected *)
  let draw = Random.float 1. in
  let rec choose_game position = function
    | [] -> None
    | (game, weight) :: games ->
      (* does the [draw] lie in this segment? *)
      if Float.(position <= draw && draw < position +. weight)
      then (printf "drawn with weight %f\n" weight; Some game)
      else choose_game (position +. weight) games
  in
  choose_game 0. softmax_weights
;;

let decide_next_game
    time
    (player_queue : Player.t list)
    (match_record : Match_record.t list)
  =
  let possible_games_from_first_8 =
    let rec possible_games chosen take_n remaining remaining_len =
      if take_n < 0
      then failwith "BUG: [take_n] in [decide_next_game] was negative"
      else if remaining_len < take_n
      then []
      else if take_n = 0
      then [ chosen ]
      else
        (match remaining with
         | [] -> []
         | front :: remaining ->
           let remaining_len = remaining_len - 1 in
           let including_front_player =
             possible_games
               (front :: chosen)
               (take_n - 1)
               remaining
               remaining_len
           in
           let not_including_front_player =
             possible_games chosen take_n remaining remaining_len
           in
           including_front_player @ not_including_front_player)
    in
    match player_queue with
    | front :: rest ->
      let available_player_prefix = List.take rest 7 in
      let games =
        possible_games
          [ front ]
          3
          available_player_prefix
          (List.length available_player_prefix)
      in
      let gen_match p1 p2 p3 p4 =
        { Court.Match.team1 = { side = Side1; player1 = p1; player2 = p2 }
        ; team2 = { side = Side2; player1 = p3; player2 = p4 }
        }
      in
      List.concat_map games ~f:(function
          | [ p1; p2; p3; p4 ] ->
            [ gen_match p1 p2 p3 p4
            ; gen_match p1 p3 p2 p4
            ; gen_match p1 p4 p2 p3
            ]
          | _ -> [])
    | [] -> []
  in
  let player_penalty =
    let penalty_by_player =
      let update start_time end_time player play_time =
        Map.update
          play_time
          player
          ~f:(function
              | None ->
                (start_time, Time_ns.diff end_time start_time, end_time)
              | Some (prev_start_time, total, prev_end_time) ->
                let total =
                  Time_ns.Span.
                    (total + (Time_ns.diff end_time start_time))
                in
                ( Time_ns.min prev_start_time start_time
                , total
                , Time_ns.max prev_end_time end_time))
      in
      let rec compute matches play_time =
        match matches with
        | [] -> play_time
        | { Match_record.start_time
          ; end_time
          ; match_setup =
              { team1 = { player1 = p1; player2 = p2; side = _ }
              ; team2 = { player1 = p3; player2 = p4; side = _ }
              }
          } :: matches ->
          let play_time =
            let update = update start_time end_time in
            play_time
            |> update p1
            |> update p2
            |> update p3
            |> update p4
          in
          compute matches play_time
      in
      Map.map
        (compute match_record Player.Map.empty)
        ~f:(fun (start, total, last_end) ->
            let average_court_time_penalty =
              let penalty =
                (* scaling factor for average play time penalty
                   [penalty = 2.] means you have 1 penalty point for
                   playing for 30m out of every 1h. *)
                2.
              in
              (* TODO: think about total time sitting off
                 - or average time sitting off *)
              Time_ns.Span.(total // (Time_ns.diff time start)) *. penalty
            in
            let recency_penalty =
              (* add penalty points for players who have played recently
                 this penalty decreases exponentially with time, so after
                 10m this accounts for 1 point *)
              let since_last_played =
                Time_ns.Span.max
                  (Time_ns.Span.of_sec 30.)
                  (Time_ns.diff time last_end)
              in
              Time_ns.Span.(of_min 10. // since_last_played)
            in
            average_court_time_penalty +. recency_penalty)
    in
    fun player ->
      Map.find penalty_by_player player
      |> Option.value ~default:0.
  in
  (* Create a ranking score for a particular game configuration. Lower
     scores are better - think of adding to the score as penalising a game
     for a negative trait *)
  let rank_game { Court.Match.team1; team2 } =
    let mixed_game_penalty =
      let penalty =
        (* fixed penalty for having unmixed games (one side mixed, one
           side not) *)
        3.
      in
      let mixed
          { Court.Team.player1 = { sex = player1_sex; _ }
          ; player2 = { sex = player2_sex; _ }
          ; side = _
          }
        =
        match player1_sex, player2_sex with
        | Male, Male | Female, Female -> false
        | Male, Female | Female, Male -> true
      in
      if Bool.(mixed team1 = mixed team2)
      then 0. (* if both teams are mixed, or both are not mixed, there's
                 no mixed penalty *)
      else penalty (* if only one team is mixed, add a penalty *)
    in
    let player_penalty =
      let team_penalty { Court.Team.player1; player2; side = _ } =
        player_penalty player1 +. player_penalty player2
      in
      team_penalty team1 +. team_penalty team2
    in
    let skill_diff_penalty =
      let team_skill
          { Court.Team.player1 = { skill = skill1; _ }
          ; player2 = { skill = skill2; _ }
          ; side = _
          }
        =
        skill1 + skill2
      in
      (* add one penalty point for each point of skill difference between
         the two sides, e.g. a 1 and 2 vs 1 and 4 would have a skill diff
         of 2 points. *)
      Int.abs (team_skill team1 - team_skill team2) |> Int.to_float
    in
    let skill_distribution_penalty =
      let skills =
        let { Court.Team.player1 = p1; player2 = p2; side = _ } = team1 in
        let { Court.Team.player1 = p3; player2 = p4; side = _ } = team2 in
        List.map
          [ p1.skill; p2.skill; p3.skill; p4.skill ]
          ~f:Int.to_float
      in
      let mean = List.sum (module Float) skills ~f:Fn.id /. 4. in
      let square_diffs =
        List.map skills ~f:(fun skill -> (skill -. mean) ** 2.)
      in
      sqrt (List.sum (module Float) square_diffs ~f:Fn.id /. 4.)
    in
    mixed_game_penalty
    +. player_penalty
    +. skill_diff_penalty
    +. skill_distribution_penalty
  in
  let ranked_games =
    List.map
      possible_games_from_first_8
      ~f:(fun game -> game, rank_game game)
  in
  match draw_best_game ranked_games with
  | None -> None, player_queue
  | Some game ->
    let not_in_new_game { Player.uuid; _ } =
      let { Court.Match.team1 = { player1 = p1; player2 = p2; side = _ }
          ; team2 = { player1 = p3; player2 = p4; side = _ }
          } = game
      in
      let in_new_game =
        List.mem
          [ p1.uuid; p2.uuid; p3.uuid; p4.uuid ]
          uuid
          ~equal:Uuid.Unstable.equal
      in
      not in_new_game
    in
    Some game, List.filter player_queue ~f:not_in_new_game
;;

let generate_match
    ({ State.current_matches; player_queue; match_record } as state)
    ~start_time
  =
  let next_available_court =
    match Map.filter current_matches
            ~f:(function
                | _, Court.Match_in_play _ -> false
                | _, Court_available -> true)
          |> Map.keys with
    | [] -> None
    | next_available_court :: _ -> Some next_available_court
  in
  let next_match, leftover_players =
    decide_next_game start_time player_queue match_record
  in
  match Option.both next_available_court next_match with
  | Some (court, next_match) ->
    let current_matches =
      Map.set current_matches ~key:court ~data:(start_time, Match_in_play next_match)
    in
    { state with current_matches
               ; player_queue = leftover_players
    }
  | None -> state
;;

let finish_match
    ({ State.current_matches; player_queue; match_record } as state)
    ~end_time
    court
    (winner : Court.Team.Side.t)
  =
  match Map.find current_matches court with
  | Some (start_time, Match_in_play ({ team1; team2 } as finished_match)) ->
    let current_matches =
      Map.set
        current_matches
        ~key:court
        ~data:(end_time, Court_available)
    in
    let winner, loser =
      match winner with
      | Side1 -> team1, team2
      | Side2 -> team2, team1
    in
    let player_queue =
      player_queue
      @ [ winner.player1; winner.player2; loser.player1; loser.player2 ]
    in
    let match_data =
      { Match_record.start_time; end_time; match_setup = finished_match }
    in
    { State.current_matches
    ; player_queue
    ; match_record = match_data :: match_record
    }
  | Some (_, Court_available) | None -> state
;;

type t =
  { current_matches : Court.t Int.Map.t
  ; player_queue : Player.t Int.Map.t
  ; add_player : Player.t -> unit Effect.t
  ; generate_match : unit Effect.t
  ; finish_match : int -> winner:Court.Team.Side.t -> unit Effect.t
  }

let component ~number_of_courts graph =
  let default_model =
    { State.current_matches =
        Int.Map.of_alist_exn
          (List.init number_of_courts
             ~f:(fun id -> id, (Time_ns.epoch, Court.Court_available)))
    ; player_queue = []
    ; match_record = []
    }
  in
  let state, action =
    Bonsai.state_machine_with_input
      ~equal:[%equal: State.t]
      ~default_model
      ~apply_action:(fun _ input state ->
          let now =
            match input with
            | Active now -> now
            | Inactive -> Time_ns.epoch
          in
          function
          | Action.Add_player player ->
            { state with player_queue = state.player_queue @ [ player ] }
          | Generate_match -> generate_match state ~start_time:now
          | Finish_match { court; winner } ->
            finish_match state court winner ~end_time:now)
      (Bonsai.Clock.now graph)
      graph
  in
  let%arr { current_matches; player_queue; match_record = _ } = state
  and action in
  let player_queue =
    Int.Map.of_alist_exn
      (List.mapi player_queue ~f:(fun index player -> index, player))
  in
  { current_matches =
      Map.map current_matches ~f:(fun (_, current_match) -> current_match)
  ; player_queue
  ; add_player = (fun player -> action (Add_player player))
  ; generate_match = action Generate_match
  ; finish_match =
      (fun court ~winner -> action (Finish_match { court; winner }))
  }
;;
