(* 
   Copyright 2004 Martin Jambon

   This module produces HTML code for the presentation of OCaml programs
   (.ml .mli .mll .mly).

   This file is distributed under the terms of the GNU Public License
   http://www.gnu.org/licenses/gpl.txt
*)

val version : string
(** Version of caml2html. For compatibility with older versions. 
  Use [Version.version] instead, which returns only the version code,
  without the "caml2html " prefix. *)

type class_definition = (string list * (string * string) list)

val default_default_style : class_definition list

val default_style : string

val key_color1 : string option
val key_color2 : string option
val key_color3 : string option
val key_color4 : string option
val key_color5 : string option
val construct_color : string option * string option * string
val comment_color : string option * string option * string
val string_color : string option * string option * string
val alpha_keyword_color : string option * string option * string
val nonalpha_keyword_color : string option * string option * string

val default_keyword_color_list : 
  (string * (string option * string option * string)) list
val default_keyword_colors : 
  (string, string option * string option * string) Hashtbl.t
val all_colors : (string option * string option * string) list
(** colors which are used for the predefined style.
  This is a list of couples (optional color specification, CSS class). *)

val make_css : 
  ?default: class_definition list ->
  ?colors:(string option * string option * string) list -> string -> unit
(** make a CSS file from the given colors *)

type style = [ `Inline | `Inhead of string | `Url of string ]

type param = {
  line_numbers : bool;
  title : bool;
  body_only : bool;
  tab_size : int;
  footnote : bool;
  style : style;
  html_comments : bool;
  charset : string;
  annot_filter : Annot.filter;
  no_annot : bool;
  ie7 : bool;
}
(** the type of the options for making the HTML document *)

val default_param : param

val ocaml :
  ?nbsp:bool ->
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  ?param:param ->
  Buffer.t -> 
  Input.token list -> unit
(** [ocaml buf l] formats the list of tokens [l] into some HTML code
  which should be placed in a <code> or <pre> region,
  and adds the result the given buffer [buf].
  Option [nbsp] tells if the spaces must be converted into "&nbsp;" or not
  (required in <code> regions but not in <pre>; default is false). *)

val ocamlcode :
  ?annot:Annot.tag list ->
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  ?param:param -> ?tag_open:string -> ?tag_close:string -> string -> string
(** [ocamlcode s1 s2] parses [s1] and formats the result as a HTML string
  enclosed between <code> and </code> unless specified otherwise. *)

val ocamlpre :
  ?annot:Annot.tag list ->
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  ?param:param -> ?tag_open:string -> ?tag_close:string -> string -> string
(** [ocamlcode s1 s2] parses [s1] and formats the result as a HTML string
  enclosed between <pre> and </pre> unless specified otherwise. *)

(* $Id$ *)

val ocaml_file :
  ?filename:string ->
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  param:param ->
  Buffer.t ->
  Input.token list -> unit
(** [ocaml_file buf tokens] makes HTML code that represents one source file
  of OCaml code. The name of the file is added as title, 
  depending on the parameters and is specified with the [filename] option.
*)

val begin_document : ?param:param -> Buffer.t -> string list -> unit
val end_document : ?param:param -> Buffer.t -> unit

val handle_file :
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  ?param:param -> Buffer.t -> string -> unit
(** [handle_file buf srcfile] parse the given file [srcfile]
  and puts the HTML into [buf]. *)

val save_file : ?dir:string -> Buffer.t -> string -> unit
(** [save_file buf file] just saves the contents of buffer [buf]
  in the given [file]. *)

val ocaml_document :
  ?dir:string ->
  ?keyword_colors:(string, string option * string option * string) Hashtbl.t ->
  ?param:param -> string list -> string -> unit
(** [ocaml_document files file] parses the given OCaml [files]
  and make one complete HTML document that shows the contents of 
  these files. *)
