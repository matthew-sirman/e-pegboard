open! Core
open Bonsai_web

module Sex : sig
  type t =
    | Male
    | Female
  [@@deriving sexp_of, equal, compare, enumerate]
end

(* TODO: guest and pay-and-play are different *)
module Membership_status : sig
  type t =
    | Member
    | Pay_and_play
  [@@deriving sexp_of, equal, compare, enumerate]
end

type t = private
  { uuid : Uuid.Unstable.t
  ; name : string
  ; skill : int
  ; sex : Sex.t
  ; membership_status : Membership_status.t
  }
[@@deriving sexp, equal]

include Comparable.S_plain with type t := t

val create
  : name:string
  -> skill:int
  -> sex:Sex.t
  -> membership_status:Membership_status.t
  -> t

val card_component : t Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t
