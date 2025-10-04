open! Core
open Bonsai_web

type t =
  { current_matches : Court.t Int.Map.t
  ; player_queue : Player.t Int.Map.t
  ; add_player : Player.t -> unit Effect.t
  ; generate_match : unit Effect.t
  ; finish_match : int -> winner:Court.Team.Side.t -> unit Effect.t
  }

val component
  :  number_of_courts:int
  -> Bonsai.graph
  -> t Bonsai.t
