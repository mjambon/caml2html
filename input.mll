(* $Id$ *)
{
(*
  Copyright 2002-2004 Sebastien Ailleret
  Copyright 2004-2006 Martin Jambon

  This file is distributed under the terms of the GNU Public License
  http://www.gnu.org/licenses/gpl.txt
*)

open Printf
open Lexing

type token = [ `Comment of string
             | `Special_comment of string * string
	     | `Construct of string
	     | `Keyword of string
	     | `Newline
	     | `String of string
	     | `Quotation of string
	     | `Tab
	     | `Token of string
	     | `Start_annot of (Annot.layer_info * string)
	     | `Stop_annot of Annot.layer_info ]

type state = { mutable depth : int;
	       buf : Buffer.t;
	       lexbuf : lexbuf;
	       mutable tokens : token list;
	       mutable annot_tags : Annot.tag list;
	       mutable in_group : bool }

let init_state annot_tags lexbuf = { depth = 0;
				     buf = Buffer.create 1000;
				     lexbuf = lexbuf;
				     tokens = [];
				     annot_tags = annot_tags;
				     in_group = false }

let stringpair_of_token = function
    `Comment s -> "Comment", s
  | `Construct s -> "Construct", s
  | `Keyword s -> "Keyword", s
  | `Newline -> "Newline", ""
  | `String s -> "String", s
  | `Quotation s -> "Quotation", s
  | `Tab -> "Tab", ""
  | `Token s -> "Token", s
  | `Start_annot (_info, s) -> "Start_annot", s
  | `Stop_annot _info -> "Stop_annot", ""

let string_of_token x =
  match stringpair_of_token x with
      a, "" -> a
    | a, b -> sprintf "%s %S" a b

let print_tokens l =
  List.iter (fun s ->
	       printf "%s\n" (string_of_token s))
    l

let keywords = [
  "and";
  "as";
  "asr";
  "assert";
  "begin";
  "class";
  "constraint";
  "do";
  "done";
  "downto";
  "else";
  "end";
  "exception";
  "external";
  "false";
  "for";
  "fun";
  "function";
  "functor";
  "if";
  "in";
  "include";
  "inherit";
  "initializer";
  "land";
  "lazy";
  "let";
  "lor";
  "lsl";
  "lsr";
  "lxor";
  "match";
  "method";
  "mod";
  "module";
  "mutable";
  "new";
  "object";
  "of";
  "open";
  "or";
  "private";
  "rec";
  "sig";
  "struct";
  "then";
  "to";
  "true";
  "try";
  "type";
  "val";
  "virtual";
  "when";
  "while";
  "with" ]

let is_keyword =
  let tbl = Hashtbl.create 100 in
  List.iter (fun key -> Hashtbl.add tbl key ()) keywords;
  Hashtbl.mem tbl

let tokenify s =
  if is_keyword s then `Keyword s
  else `Token s

let init_lexbuf lexbuf filename =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = filename }


let compare_pos a b =
  let c = compare a.pos_lnum b.pos_lnum in
  if c <> 0 then c
  else compare (a.pos_cnum - a.pos_bol) (b.pos_cnum - b.pos_bol)

(* Consume the list of annotations *)
let get_annot state cur_pos =
  let rec loop () =
    match state.annot_tags with
	[] -> []
      | ((_, (tag_pos, _)) as tag) :: tl ->
	  if compare_pos tag_pos cur_pos <= 0 then
	    (state.annot_tags <- tl;
	     tag :: loop ())
	  else [] in
  loop ()

let simple_annot x =
  match x with
      (`Start typ, (_, info)) -> `Start_annot (info, typ)
    | (`Stop, (_, info)) -> `Stop_annot info

let simple_annots = List.map simple_annot

(* Add all unclosed tags that may remain *)
let finish_annot state =
  state.tokens <-
    (List.rev_map simple_annot state.annot_tags)
    @ state.tokens;
  state.annot_tags <- []

let newline state =
  let lexbuf = state.lexbuf in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
			   pos_lnum = pos.pos_lnum + 1;
			   pos_bol = pos.pos_cnum }

let shift x pos =
  { pos with pos_cnum = pos.pos_cnum + x }

let begin_group state =
  state.in_group <- true

let end_group state =
  state.in_group <- false

let add_token ?(offset = 0) state x =
  if x = `Newline then
    newline state;
  let annot1, annot2 =
    if not state.in_group then
      let annot1 =
	get_annot state (shift offset (lexeme_start_p state.lexbuf)) in
      let annot2 =
	get_annot state (shift offset (lexeme_end_p state.lexbuf)) in
      annot1, annot2
    else [], [] in
  state.tokens <- (List.rev_append (simple_annots annot2)
		     (x :: (List.rev_append
			      (simple_annots annot1) state.tokens)))

let return_tokens state =
  let l = List.rev state.tokens in
  let tagged =
    List.map (function
		  `Start_annot _ as x -> (Tag.Start, x)
		| `Stop_annot _ as x -> (Tag.Stop, x)
		| x -> (Tag.Other, x))
      l in
  let annotate b x =
    if not b then x
    else
      match x with
	  `Start_annot (info, typ) ->
	    `Start_annot ({ info with Annot.innermost = true }, typ)
	| `Stop_annot info ->
	    `Stop_annot { info with Annot.innermost = true }
	| _ -> assert false in
  let l = Tag.annotate_innermost annotate (Tag.remove_matches tagged) in
  let result = List.map snd l in
  result
}

let upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let lower = ['a'-'z' '\223'-'\246' '\248'-'\255']
let digit = ['0'-'9']
let identchar = upper | lower | digit | ['_' '\'']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let oct = ['0'-'7']
let bin = ['0'-'1']

let operator_char =
  [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let infix_symbol =
  ['=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' '$' '%'] operator_char*
let prefix_symbol = ['!' '?' '~'] operator_char*

let lident = (lower | '_' identchar) identchar*
let uident = upper identchar*

let blank = [ ' ' '\t' ]
let space = [ ' ' '\t' '\r' '\n' ]

rule token state = parse
| "(*" (lident as handler_name)?
    {
      begin_group state;
      Buffer.clear state.buf;
      state.depth <- 1;
      (match handler_name with
	   Some name when Plugin.exists name ->
	     comment true state lexbuf;
	     let s = Buffer.contents state.buf in
	     let n = Plugin.count_newlines s in
	     (for i = 1 to n do newline state done);
	     add_token state (`Special_comment (name, s))
	 | None
	 | Some _ ->
	     Buffer.add_string state.buf "(*";
	     (match handler_name with
		  Some name -> Buffer.add_string state.buf name
		| None -> ());
	     comment false state lexbuf;
	     Buffer.add_string state.buf "*)";
	     add_token state (`Comment (Buffer.contents state.buf));
      );
      end_group state;
      token state lexbuf
    }
| '"'
    { begin_group state;
      Buffer.clear state.buf;
      Buffer.add_char state.buf '"';
      string state false lexbuf;
      add_token state (`String (Buffer.contents state.buf));
      end_group state;
      token state lexbuf }
| "<<"
| "<:" lident "<"
    { begin_group state;
      add_token state (`Construct (lexeme lexbuf));
      Buffer.clear state.buf;
      quotation state lexbuf;
      add_token ~offset:(-2) state (`Quotation (Buffer.contents state.buf));
      add_token state (`Construct ">>");
      end_group state;
      token state lexbuf }
| '`'
| uident
      { add_token state (`Construct (lexeme lexbuf));
	token state lexbuf }
| lident
      { add_token state (tokenify (lexeme lexbuf));
	token state lexbuf }

| "!=" | "#" | "&" | "&&" | "(" | ")" | "*" | "+" | "," | "-"
| "-." | "->" | "." | ".. :" | "::" | ":=" | ":>" | ";" | ";;" | "<"
| "<-" | "=" | ">" | ">]" | ">}" | "?" | "??" | "[" | "[<" | "[>" | "[|"
| "]" | "_" | "`" | "{" | "{<" | "|" | "|]" | "}" | "~"

    { add_token state (`Keyword (lexeme lexbuf));
      token state lexbuf }

| prefix_symbol | infix_symbol
      { add_token state (`Token (lexeme lexbuf));
	token state lexbuf }

| "'\n'" | "'\r\n'"
      { List.iter (add_token state) [`String "'"; `Newline; `String "'"];
	token state lexbuf }
| "'\\\n'" | "'\\\r\n'"
      { List.iter (add_token state) [`String "'\\"; `Newline; `String "'"];
	token state lexbuf }
| "'" ([^'\'''\\'] | '\\' (_ | digit digit digit | 'x' hex hex)) "'"
      { add_token state (`String (lexeme lexbuf));
	token state lexbuf }

| '\r'? '\n'
    { add_token state `Newline;
      token state lexbuf }
| '\t'
    { add_token state `Tab;
      token state lexbuf }
| eof
    { finish_annot state;
      return_tokens state }
| ' '+
    { add_token state (`Token (lexeme lexbuf));
      token state lexbuf }

| '-'? (digit (digit | '_')*
       | ("0x"| "0X") hex (hex | '_')*
       | ("0o"| "0O") oct (oct | '_')*	
       | ("0b"| "0B") bin (bin | '_')* )

| '-'? digit (digit | '_')* ('.' (digit | '_')* )?
      (['e' 'E'] ['+' '-']? digit (digit | '_')* )?
| _
    { add_token state (`Token (lexeme lexbuf));
      token state lexbuf }

and comment special state = parse
| "(*"
    { state.depth <- state.depth + 1;
      Buffer.add_string state.buf "(*";
      comment special state lexbuf }
| "*)"
    { state.depth <- state.depth - 1;
      if (state.depth > 0) then (
	Buffer.add_string state.buf "*)";
	comment special state lexbuf
      )
    }
| '"'
    { Buffer.add_char state.buf '"';
      string state true lexbuf;
      comment special state lexbuf }
| eof
    { finish_annot state }
| '\r'? '\n'
    { if special then (
	Buffer.add_char state.buf '\n';
	comment special state lexbuf
      )
      else (
	add_token state (`Comment (Buffer.contents state.buf));
	add_token state `Newline;
	Buffer.clear state.buf;
	comment special state lexbuf
      )
    }
| '\t'
    { add_token state (`Comment (Buffer.contents state.buf));
      add_token state `Tab;
      Buffer.clear state.buf;
      comment special state lexbuf }
| _
    { Buffer.add_string state.buf (lexeme lexbuf);
      comment special state lexbuf }


and string state comment = parse
| '"'
    { Buffer.add_char state.buf '"' }
| "\\\\"
| '\\' '"'
    { Buffer.add_string state.buf (lexeme lexbuf);
      string state comment lexbuf }
| eof
    { finish_annot state }
| '\r'? '\n'
    { let s = Buffer.contents state.buf in
      add_token state (if comment then `Comment s else `String s);
      add_token state `Newline;
      Buffer.clear state.buf;
      string state comment lexbuf }
| '\t'
    { let s = Buffer.contents state.buf in
      add_token state (if comment then `Comment s else `String s);
      add_token state `Tab;
      Buffer.clear state.buf;
      string state comment lexbuf }
| _
    { Buffer.add_string state.buf (lexeme lexbuf);
      string state comment lexbuf }

and quotation state = parse
  | ">>"    { () }
  | "\\>>"  { Buffer.add_string state.buf "\\>>";
	      quotation state lexbuf }
  | '\r'? '\n'
      { let s = Buffer.contents state.buf in
	add_token state (`Quotation s);
	add_token state `Newline;
	Buffer.clear state.buf;
	quotation state lexbuf }
  | '\t'
      { let s = Buffer.contents state.buf in
	add_token state (`Quotation s);
	add_token state `Tab;
	Buffer.clear state.buf;
	quotation state lexbuf }
  | _       { Buffer.add_string state.buf (lexeme lexbuf);
	      quotation state lexbuf }

{
  let parse ?(annot = []) lexbuf =
    token (init_state annot lexbuf) lexbuf

  let string ?(filename = "") ?(annot = []) s =
    let lexbuf = Lexing.from_string s in
    init_lexbuf lexbuf filename;
    token (init_state annot lexbuf) lexbuf

  let channel ?(filename = "") ?(annot = []) ic =
    let lexbuf = Lexing.from_channel ic in
    init_lexbuf lexbuf filename;
    token (init_state annot lexbuf) lexbuf

  let file ?annot s =
    let ic = open_in s in
    let l = channel ~filename:s ?annot ic in
    close_in ic;
    l
}
