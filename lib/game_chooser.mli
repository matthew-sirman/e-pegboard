open! Core
open Bonsai_web

val component
  :  Pegboard.t Bonsai.t
  -> club_name:string Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
