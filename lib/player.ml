open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Sex = struct
  type t =
    | Male
    | Female
  [@@deriving sexp, equal, compare, enumerate]
end

module Membership_status = struct
  type t =
    | Member
    | Pay_and_play
  [@@deriving sexp, equal, compare, enumerate]
end

type t =
  { uuid : Uuid.Unstable.t
  ; name : string
  ; skill : int
  ; sex : Sex.t
  ; membership_status : Membership_status.t
  }
[@@deriving sexp, equal, compare]

include Comparable.Make_plain
    (struct
      type nonrec t = t [@@deriving sexp_of, equal, compare]
    end)

let create ~name ~skill ~sex ~membership_status =
  { uuid = Uuid.create_random Random.State.default
  ; name
  ; skill
  ; sex
  ; membership_status
  }

module Styles =
  [%css stylesheet {|
      .player-card.in-queue {
        background-color: white;
        max-width: 15em;
        text-align: center;
        border: 1px solid %{Colours.border#Css_gen.Color};
        border-radius: 0.5em;
        padding: 0.5em;
      }
  |}]

let card_component player _graph =
  let%arr
    { uuid = _
    ; name
    ; skill = _
    ; sex = _
    ; membership_status
    } = player in
  let membership_status =
    match membership_status with
    | Member -> Vdom.Node.none
    | Pay_and_play -> {%html|(G)|}
  in
  {%html|
    <div %{Styles.player_card} %{Styles.in_queue}>
      %{name#String} %{membership_status}
    </div>
  |}
;;
