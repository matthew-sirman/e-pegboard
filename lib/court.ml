open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Styles =
  [%css stylesheet {|
    .court {
      background-color: %{Colours.section_bg#Css_gen.Color};
      width: 28em;
      height: 20em;
      display: flex;
      flex-direction: row;
      justify-content: center;
      align-items: center;
      border: 1px solid %{Colours.border#Css_gen.Color};
      border-radius: 0.5em;
      padding: 0em 1em;
    }

    .court.pending {
      border-style: dotted;
    }

    .team_container {
      display: flex;
      flex-direction: column;
    }

    .team {
      width: 12em;
      height: 14em;
      display: grid;
      grid-template-rows: 1fr 1fr;
      gap: 2px;
      padding: 0.5em 0;
    }

    .team.team1 {
      grid-template-areas:
        "p1 forecourt"
        "p2 forecourt";
      grid-template-columns: 3fr 1fr;
      border-right: 2px solid black;
    }

    .team1 > div {
      background-color: %{Colours.side1_bg#Css_gen.Color};
    }

    .team.team2 {
      display: grid;
      grid-template-areas:
        "forecourt p1"
        "forecourt p2";
      grid-template-columns: 1fr 3fr;
      border-left: 2px solid black;
    }

    .team2 > div {
      background-color: %{Colours.side2_bg#Css_gen.Color};
    }

    .forecourt {
      grid-area: forecourt;
    }

    .player {
      display: block;
      align-content: center;
      padding: 0.5em;
    }

    .player.player1 {
      grid-area: p1;
    }

    .player.player2 {
      grid-area: p2;
    }

    .team1-button {
      color: white;
      background-color: %{Tailwind_v3_colors.create `sky `_500#Css_gen.Color};
      height: 3em;
      margin: 0 1em;
    }

    .team1-button:hover {
      background-color: %{Tailwind_v3_colors.create `sky `_400#Css_gen.Color};
    }

    .team2-button {
      color: white;
      background-color: %{Tailwind_v3_colors.create `red `_500#Css_gen.Color};
      height: 3em;
      margin: 0 1em;
    }

    .team2-button:hover {
      background-color: %{Tailwind_v3_colors.create `red `_400#Css_gen.Color};
    }
  |}]

module Team = struct
  module Side = struct
    type t =
      | Side1
      | Side2
    [@@deriving equal]
  end

  type t =
    { side : Side.t
    ; player1 : Player.t
    ; player2 : Player.t
    }
  [@@deriving equal]
end

module Match = struct
  type t =
    { team1 : Team.t
    ; team2 : Team.t
    }
  [@@deriving equal]
end

type t =
  | Match_in_play of Match.t
  | Court_available
[@@deriving equal]

let team_component (team : Team.t Bonsai.t) graph =
  let%sub { side; player1; player2 } = team in
  let%arr player1_card = Player.card_component player1 graph
  and player2_card = Player.card_component player2 graph
  and side_style =
    match%arr side with
    | Side1 -> Styles.team1
    | Side2 -> Styles.team2
  in
  {%html|
    <div %{Styles.team} %{side_style}>
      <div %{Styles.forecourt}></div>
      <div %{Styles.player} %{Styles.player1}>
        %{player1_card}
      </div>
      <div %{Styles.player} %{Styles.player2}>
        %{player2_card}
      </div>
    </div>
  |}
;;

let component
    (match_setup : t Bonsai.t)
    ~(finish_match : (winner:Team.Side.t -> unit Effect.t) Bonsai.t)
    graph
  =
  match%sub match_setup with
  | Match_in_play { team1; team2 } ->
    let%arr team1 = team_component team1 graph
    and team2 = team_component team2 graph
    and finish_match in
    {%html|
      <div %{Styles.court}>
        <div %{Styles.team_container}>
          %{team1}
          <%{Components.button}
              %{Styles.team1_button}
              on_click=%{fun _ev -> finish_match ~winner:Side1}>
            Blue won
          </>
        </div>
        <div %{Styles.team_container}>
          %{team2}
          <%{Components.button}
              %{Styles.team2_button}
              on_click=%{fun _ev -> finish_match ~winner:Side2}>
            Red won
          </>
        </div>
      </div>
    |}
  | Court_available ->
    Bonsai.return
      {%html|
        <div %{Styles.court}>
          Currently available
        </div>
      |}
;;

let pending_court
    (match_setup : t Bonsai.t)
    graph
  =
  match%sub match_setup with
  | Match_in_play { team1; team2 } ->
    let%arr team1 = team_component team1 graph
    and team2 = team_component team2 graph in
    {%html|
      <div %{Styles.court} %{Styles.pending}>
        <p>Pending game</p>
        <div %{Styles.team_container}>
          %{team1}
        </div>
        <div %{Styles.team_container}>
          %{team2}
        </div>
      </div>
    |}
  | Court_available ->
    Bonsai.return
      {%html|
        <div %{Styles.court} %{Styles.pending}>
          No match pending
        </div>
      |}
