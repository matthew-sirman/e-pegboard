open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Styles =
  [%css stylesheet {|
      html, body {
        margin: 0;
        width: 100%;
        height: 100%;
      }

      .app {
        width: 100%;
        height: 100%;
        font-family: "Inter", sans-serif;
        background-color: %{Colours.app_bg#Css_gen.Color};
      }
  |}]

let database_file_uploader graph =
  let%sub { value = web_ui_file; view = form_view; set = _ } =
    Bonsai_web_ui_form.With_manual_view.Elements.File_select.single_opt
      ()
      graph
  in
  let make_error error =
    let error_html =
      {%html|
        <>
          Error encountered: %{Error.to_string_hum error#String}
        </>
      |}
    in
    None, error_html
  in
  let%sub database, status_indicator =
    match%sub web_ui_file with
    | Error error -> let%arr error in make_error error
    | Ok None ->
      Bonsai.return (None, {%html|Please select a database file|})
    | Ok (Some web_ui_file) ->
      match%arr
        Bonsai_web_ui_file.Read_on_change.create_single
          web_ui_file
          graph
      with
      | _, Starting -> None, {%html|Starting load...|}
      | _, In_progress progress ->
        let progress =
          Bonsai_web_ui_file.Progress.to_percentage progress
        in
        None, {%html|<>Progress: %{progress#Percent} </>|}
      | _, Complete (Error error) -> make_error error
      | _, Complete (Ok content) ->
        let content = Bigstring.to_string content in
        match Player_database.load_from_string ~content with
        | Error error -> make_error error
        | Ok database -> Some database, Vdom.Node.none
  in
  let database_file_input =
    let%arr status_indicator and form_view in
    {%html|
      <div>
        %{status_indicator}
        %{form_view}
      </div>
    |}
    in
  database, database_file_input
;;

let app graph =
  let database, database_file_input = database_file_uploader graph in
  let%arr content =
    match%sub database with
    | None -> database_file_input
    | Some database ->
      let pegboard =
        Pegboard.component ~number_of_courts:4 graph
      in
      Bonsai.Edge.on_change
        database
        ~equal:[%equal: _]
        ~callback:(
          let%arr { add_player; _ } = pegboard in
          fun database ->
            Effect.all_unit
              (List.map
                 (Player_database.rows database)
                 ~f:(fun player -> add_player player)))
        graph;
      let club_name =
        let%arr database in
        Player_database.club_name database
      in
      Game_chooser.component pegboard ~club_name graph
  in
  {%html|
    <div %{Styles.app}>
      %{content}
    </div>
  |}
;;
