(* $Id$ *)

type handler =
    [ `Command of string
    | `Function of (string -> string option) ]

val add : string -> handler -> unit
val exists : string -> bool
val find : string -> handler

val count_newlines : string -> int

val expand : string -> string -> string option

val register_command : string -> unit
