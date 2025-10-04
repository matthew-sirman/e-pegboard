open! Core

type t =
  { club_name : string
  ; rows : Player.t list
  } [@@deriving sexp]

let load_from_string ~content =
  match Sexp.of_string content |> [%of_sexp: t] with
  | exception exn -> Or_error.of_exn exn
  | database -> Ok database
;;

let add_row t row = { t with rows = row :: t.rows }
let rows { club_name = _; rows } = rows
let club_name { club_name; rows = _ } = club_name
