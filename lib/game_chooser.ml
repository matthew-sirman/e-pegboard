open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Styles =
  [%css stylesheet {|
      .container {
        height: 100%;
        display: flex;
        flex-flow: column;
        gap: 1em;
      }

      .header {
        flex: 0 1 auto;
        text-align: center;
        font-size: 3em;
        margin: 0.25em;
      }

      .current-matches-section {
        flex: 0 1 auto;
        display: flex;
        flex-direction: column;
      }

      .current-matches {
        overflow-x: auto;
        display: flex;
        flex-direction: row;
        gap: 1em;
        border: 1px solid %{Colours.border#Css_gen.Color};
        border-radius: 0.5em;
        padding: 0.5em;
      }

      .gen-match {
        margin: 0 auto;
        width: 20em;
        padding: 1em;

        color: white;
        background-color: %{Tailwind_v3_colors.create `emerald `_500#Css_gen.Color};
      }

      .gen-match:hover {
        background-color: %{Tailwind_v3_colors.create `emerald `_400#Css_gen.Color};
      }

      .interface-section {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: row;
        margin: 0 auto;
        gap: 1em;
      }

      .player-queue {
        display: flex;
        flex-direction: column;
        gap: 1em;
        height: 80%;
        padding: 1em;
        border: 1px solid %{Colours.border#Css_gen.Color};
        border-radius: 0.5em;
        background-color: %{Colours.section_bg#Css_gen.Color};
      }
  |}]

let component (pegboard : Pegboard.t Bonsai.t) ~club_name graph =
  let%sub
    { current_matches
    ; player_queue
    ; add_player
    ; generate_match
    ; finish_match
    } = pegboard
  in
  let current_matches =
    let%arr current_matches =
      Bonsai.assoc
        (module Int)
        current_matches
        ~f:(fun position match_setup graph ->
            let finish_match =
              let%arr position and finish_match in
              fun ~winner -> finish_match position ~winner
            in
            Court.component match_setup ~finish_match graph)
        graph
    in
    Vdom.Node.Map_children.div ~attrs:[ Styles.current_matches ] current_matches
  in
  let player_queue =
    let%arr player_queue =
      Bonsai.assoc
        (module Int)
        player_queue
        ~f:(fun _ player graph -> Player.card_component player graph)
        graph
    in
    Vdom.Node.Map_children.ol
      ~attrs:[ {%css|
                padding: 0;
                margin: 0;
                overflow-y: auto;
                height: 100%;
                width: 15em;|} ]
      player_queue
  in
  let add_guest = Add_guest.component ~add_player graph in
  let%arr current_matches
  and pending_court =
    Court.pending_court (Bonsai.return Court.Court_available) graph
  and player_queue
  and generate_match
  and add_guest
  and club_name in
  {%html|
    <div %{Styles.container}>
      <div %{Styles.header}>
        #{club_name} - Badminton Pegboard
      </div>
      <div %{Styles.current_matches_section}>
        %{current_matches}
      </div>
      <%{Components.button} %{Styles.gen_match} on_click=%{fun _ev -> generate_match}>
        Generate match
      </>
      <div %{Styles.interface_section}>
        %{pending_court}
        <div %{Styles.player_queue}>
          <p style="text-align: center; margin: 0;">
            Player queue
          </p>
          %{player_queue}
        </div>
        %{add_guest}
      </div>
    </div>
  |}
;;
