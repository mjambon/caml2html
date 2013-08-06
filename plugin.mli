(* $Id$ *)

type handler =
    [ `Command of string (* External command *)
    | `Function of (string -> string option) (* Function *) ]
      (** Custom comment handler. *)

val add : string -> handler -> unit
  (** Add or replace handler. *)

val remove : string -> unit
  (** Remove handler if it exists. *)

val exists : string -> bool
  (** Test whether such handler exists. *)

val find : string -> handler
  (** Find handler or raise [Not_found]. *)


val count_newlines : string -> int
  (** Count the number of newline characters in a string. *)

val expand : string -> string -> string option
  (** [expand handler_name s] find the handler [handler_name]
      and apply it to the input string [s].
      If the handler is an external command, the result is [None]
      if and only if the process exits with a non-zero status.
      If the handler is a function, the behavior corresponds to
      the behavior of the function itself and any exception is propagated.
  *)

val register_command : string -> unit
  (** Parse and register a handler defined as "name:command". *)
