open! Core
open Bonsai_web

val component
  :  add_player:(Player.t -> unit Effect.t) Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
