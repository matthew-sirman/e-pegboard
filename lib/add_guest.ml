open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Elements = Bonsai_web_ui_form.With_manual_view.Elements

module Styles =
  [%css stylesheet {|
      .container {
        display: flex;
        flex-direction: column;
        gap: 1em;
        height: min-content;
        padding: 1em;
        border: 1px solid %{Colours.border#Css_gen.Color};
        border-radius: 0.5em;
        background-color: %{Colours.section_bg#Css_gen.Color};
      }
  |}]

let component ~add_player graph =
  let%sub { value = name; view = name_view; _ } =
    Elements.Textbox.string
      ~placeholder:(Bonsai.return "Guest player name")
      ()
      graph
  in
  let%sub { value = skill; view = skill_view; _ } =
    Elements.Number.int
      ~min:1
      ~max:5
      ~default:3
      ~step:1
      ()
      graph
  in
  let%sub{ value = sex; view = sex_view; _ } =
    Elements.Dropdown.enumerable
      (module Player.Sex)
      graph
  in
  let%sub{ value = membership_status; view = membership_status_view; _ } =
    Elements.Dropdown.enumerable
      (module Player.Membership_status)
      graph
  in
  let add_guest =
    let%arr name and skill and sex and membership_status and add_player in
    fun _ev ->
      let player =
        let%map.Or_error name and skill and sex and membership_status in
        Player.create ~name ~skill ~sex ~membership_status
      in
      match player with
      | Ok player -> add_player player
      | Error _error ->
        (* TODO: display constructive error *)
        Effect.Ignore
  in
  let%arr name_view
  and skill_view
  and sex_view
  and membership_status_view
  and add_guest
  in
  {%html|
    <div %{Styles.container}>
      <p style="text-align: center; margin: 0;">Add guest</p>
      %{name_view}
      <div style="display: flex; gap: 1em;">
        %{skill_view}
        %{sex_view}
        %{membership_status_view}
      </div>
      <button on_click=%{add_guest}>
        Add guest
      </button>
    </div>
  |}
;;
