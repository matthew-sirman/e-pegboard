open! Core

type t

val load_from_string : content:string -> t Or_error.t
val add_row : t -> Player.t -> t
val rows : t -> Player.t list
val club_name : t -> string
