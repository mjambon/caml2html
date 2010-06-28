(* 
   Copyright 2004, 2010 Martin Jambon

   This module produces HTML code for the presentation of OCaml programs
   (.ml .mli .mll .mly).

   This file is distributed under the terms of the GNU Public License
   http://www.gnu.org/licenses/gpl.txt
*)

(* $Id$ *)

type class_definition = (string list * (string * string) list)

val default_style : string

val key_color1 : string option
val key_color2 : string option
val key_color3 : string option
val key_color4 : string option
val key_color5 : string option
val construct_color : string option * string
val comment_color : string option * string
val string_color : string option * string
val alpha_keyword_color : string option * string
val nonalpha_keyword_color : string option * string

val default_keyword_color_list : 
  (string * (string option * string)) list
val default_keyword_colors : 
  (string, string option * string) Hashtbl.t
val all_colors : (string option * string) list
(** colors which are used for the predefined style.
  This is a list of pairs (optional color specification, CSS class). *)

val make_defs_file : 
  ?colors:(string option * string) list -> string -> unit
(** Dump color definitions and matching highlighting commands into a file. *)

type param = {
  line_numbers : bool;
  title : bool;
  body_only : bool;
  tab_size : int;
  latex_comments : bool;
  defs : string;
}
(** the type of the options for making the HTML document *)

val default_param : param

val ocaml :
  ?keyword_colors:(string, string option * string) Hashtbl.t ->
  ?param:param ->
  Buffer.t -> 
  Input.token list -> unit
(** [ocaml buf l] formats the list of tokens [l] into some LaTeX code
  which should be placed within the alltt environment,
  and adds the result the given buffer [buf]. *)

val ocaml_file :
  ?filename:string ->
  ?keyword_colors:(string, string option * string) Hashtbl.t ->
  param:param ->
  Buffer.t ->
  Input.token list -> unit
(** [ocaml_file buf tokens] makes LaTeX code that represents one source file
  of OCaml code. The name of the file is added as title, 
  depending on the parameters and is specified with the [filename] option.
*)

val begin_document : ?param:param -> Buffer.t -> string list -> unit
val end_document : ?param:param -> Buffer.t -> unit

val handle_file :
  ?keyword_colors:(string, string option * string) Hashtbl.t ->
  ?param:param -> Buffer.t -> string -> unit
(** [handle_file buf srcfile] parse the given file [srcfile]
  and puts the HTML into [buf]. *)

val save_file : ?dir:string -> Buffer.t -> string -> unit
(** [save_file buf file] just saves the contents of buffer [buf]
  in the given [file]. *)

val ocaml_document :
  ?dir:string ->
  ?keyword_colors:(string, string option * string) Hashtbl.t ->
  ?param:param -> string list -> string -> unit
(** [ocaml_document files file] parses the given OCaml [files]
  and make one complete HTML document that shows the contents of 
  these files. *)
