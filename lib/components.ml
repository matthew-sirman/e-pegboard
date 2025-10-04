open! Core
open Bonsai_web

module Styles =
  [%css stylesheet {|
      .button {
        display: inline-block;
        cursor: pointer;
        text-align: center;
        text-decoration: none;
        border: none;
        border-radius: 0.5em;
        box-shadow: 0 9px #999;
      }

      .button:active {
        box-shadow: 0 5px #666;
        transform: translateY(4px);
      }
  |}]

let button ?(attrs = []) children =
  {%html|
    <button %{Styles.button} *{attrs}>
      *{children}
    </button>
  |}
