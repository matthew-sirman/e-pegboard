open! Core
open Bonsai_web

module Team : sig
  module Side : sig
    type t =
      | Side1
      | Side2
  end

  type t =
    { side : Side.t
    ; player1 : Player.t
    ; player2 : Player.t
    }
  [@@deriving equal]
end

module Match : sig
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

(* TODO:
   - voiding of games
   - pending game (probably via another state in [t]) *)
val component
  :  t Bonsai.t
  -> finish_match:(winner:Team.Side.t -> unit Effect.t) Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

val pending_court
  :  t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
