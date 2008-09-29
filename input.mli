(* 
   Copyright 2004 Martin Jambon

   This file is distributed under the terms of the GNU Public License
   http://www.gnu.org/licenses/gpl.txt
*)

(*
   This module provides functions that parse OCaml source code and return
   a list of tokens which are suitable for automatic syntax highlighting.
   Any input is accepted. Only a lexical analysis is performed and thus can
   be used to highlight incorrect programs as well as derivatives
   of OCaml (.ml .mli .mll .mly).
*)

type token =
  [ `Comment of string   (** a (fragment of) comment *)
  | `Special_comment of string * string (** (handler name, full comment) *)
  | `Construct of string (** an uppercase identifier or
			     an identifier starting with ` *)
  | `Keyword of string   (** a keyword *)
  | `Newline             (** a newline character *)
  | `String of string    (** a (fragment of) string or character literal *)
  | `Quotation of string (** a camlp4 quotation *)
  | `Tab                 (** a tabulation character *)
  | `Token of string     (** anything else *)
  | `Start_annot of (Annot.layer_info * string) (** start of a type annotation 
						  read from .annot file *)
  | `Stop_annot of Annot.layer_info ]  (** end of a type annotation
					 read from .annot file *)

val parse :
  ?annot:Annot.tag list -> Lexing.lexbuf -> token list
val string : 
  ?filename:string -> ?annot:Annot.tag list -> string -> token list
val channel : 
  ?filename:string -> ?annot:Annot.tag list -> in_channel -> token list
val file : 
  ?annot:Annot.tag list -> string -> token list
