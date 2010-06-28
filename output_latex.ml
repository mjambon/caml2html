(* $Id$ *)
(* 
   Copyright 2002-2004 Sébastien Ailleret
   Copyright 2004, 2010 Martin Jambon
   
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

open Printf


type class_definition = (string list * (string * string) list)

let key_color1 = Some "0,128,0"
let key_color2 = Some "119,170,170"
let key_color3 = Some "204,153,0"
let key_color4 = Some "153,0,153"
let key_color5 = Some "128,128,128"
let key_color6 = Some "255,0,0"

let construct_color = (Some "0,51,204", "Cconstructor")
let comment_color = (Some "153,0,0", "Ccomment")
let string_color = (Some "170,68,68", "Cstring")
let quotation_color = (None, "Cquotation")
let linenum_color = (None, "Clinenum")

let alpha_keyword_color = (key_color5, "Calphakeyword")
let nonalpha_keyword_color = (None, "Cnonalphakeyword")

let default_keyword_color_list =
  [
    "and", (key_color1, "Cand");
    "as", (key_color1, "Cas");
    "class", (key_color1, "Cclass");
    "constraint", (key_color1, "Cconstraint");
    "exception", (key_color1, "Cexception");
    "external", (key_color1, "Cexternal");
    "fun", (key_color1, "Cfun");
    "function", (key_color1, "Cfunction");
    "functor", (key_color1, "Cfunctor");
    "in", (key_color1, "Cin");
    "inherit", (key_color1, "Cinherit");
    "initializer", (key_color1, "Cinitializer");
    "let", (key_color1, "Clet");
    "method", (key_color1, "Cmethod");
    "module", (key_color1, "Cmodule");
    "mutable", (key_color1, "Cmutable");
    "of", (key_color1, "Cof");
    "private", (key_color1, "Cprivate");
    "rec", (key_color1, "Crec");
    "type", (key_color1, "Ctype");
    "val", (key_color1, "Cval");
    "virtual", (key_color1, "Cvirtual");
    
    "do", (key_color2, "Cdo");
    "done", (key_color2, "Cdone");
    "downto", (key_color2, "Cdownto");
    "else", (key_color2, "Celse");
    "for", (key_color2, "Cfor");
    "if", (key_color2, "Cif");
    "lazy", (key_color2, "Clazy");
    "match", (key_color2, "Cmatch");
    "new", (key_color2, "Cnew");
    "or", (key_color2, "Cor");
    "then", (key_color2, "Cthen");
    "to", (key_color2, "Cto");
    "try", (key_color2, "Ctry");
    "when", (key_color2, "Cwhen");
    "while", (key_color2, "Cwhile");
    "with", (key_color2, "Cwith");
    
    "assert", (key_color3, "Cassert");
    "include", (key_color3, "Cinclude");
    "open", (key_color3, "Copen");
    
    "begin", (key_color4, "Cbegin");
    "end", (key_color4, "Cend");
    "object", (key_color4, "Cobject");
    "sig", (key_color4, "Csig");
    "struct", (key_color4, "Cstruct");
    
    "raise", (key_color6, "Craise");

    "asr", (key_color5, "Casr");
    "land", (key_color5, "Cland");
    "lor", (key_color5, "Clor");
    "lsl", (key_color5, "Clsl");
    "lsr", (key_color5, "Clsr");
    "lxor", (key_color5, "Clxor");
    "mod", (key_color5, "Cmod");
    
    "false", (None, "Cfalse");
    "true", (None, "Ctrue");

    "|", (key_color2, "Cbar");
  ]

let default_keyword_colors =
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun (s, (color, css_class)) -> 
       Hashtbl.add tbl s (color, css_class))
    default_keyword_color_list;
  tbl

let all_colors =
  linenum_color ::
    construct_color ::
    comment_color ::
    string_color ::
    quotation_color ::
    alpha_keyword_color ::
    nonalpha_keyword_color ::
    (List.map snd default_keyword_color_list)

let make_defs
  ?(colors = all_colors) () =
  let buf = Buffer.create 2000 in

  List.iter (
    fun (fg, name) -> 
      match fg with 
	  None ->
            bprintf buf "\
\\newcommand\\%s[1]{#1}
"
              name
	| Some color ->
            bprintf buf "\
\\definecolor{%sColor}{RGB}{%s}
\\newcommand\\%s[1]{\\textcolor{%sColor}{#1}}
"
              name color
              name name
  ) colors;

  Buffer.contents buf

  
let make_defs_file
    ?(colors = all_colors)
    file =
  let oc = open_out file in
  output_string oc (make_defs ~colors ());
  close_out oc

let default_style = make_defs ()

type param = {
  line_numbers : bool; 
  title : bool;
  body_only : bool;
  tab_size : int;
  latex_comments : bool;
  defs : string;
}

let default_param = {
  line_numbers = false; 
  title = false;
  body_only = false;
  tab_size = 8;
  latex_comments = false;
  defs = default_style;
}


let add_string buf s = 
  String.iter
    (function
	 '\\' -> Buffer.add_string buf "\\(\\backslash\\)"
       | '{' -> Buffer.add_string buf "\\{"
       | '}' -> Buffer.add_string buf "\\}"
       | c -> Buffer.add_char buf c)
    s


let line_comment p buf i =
  if p.line_numbers then
    bprintf buf "\\Clinenum{%4i}: " i

 
let colorize ?(comment = false) p buf style s =
  let add =
    if comment && p.latex_comments then Buffer.add_string buf
    else add_string buf in
  let _, clas = style in
  bprintf buf "\\%s{" clas;
  add s;
  Buffer.add_string buf "}"



let rec fold_left f accu l =
  match l with
      [] -> accu
    | a :: rest -> fold_left f (f accu a rest) rest

let ocaml
  ?(keyword_colors = default_keyword_colors)
  ?(param = default_param)
  buf l =
  
  let _last_line =
    fold_left
      (fun line token rest ->
	 match token with
	     `String s ->
	       colorize param buf string_color s;
	       line
	   | `Quotation s ->
	       colorize param buf quotation_color s;
	       line
	   | `Token s ->
	       add_string buf s;
	       line
	   | `Comment s ->
	       colorize ~comment:true param buf comment_color s;
	       line
	   | `Special_comment (handler_name, s0) ->
	       let html = 
		 match Plugin.expand handler_name s0 with
		     None -> 
		       failwith (
			 sprintf "Handler %s failed on line %i with input %s"
			   handler_name line s0
		       )
		   | Some s -> s
	       in
	       bprintf buf "\\end{alltt}%s\\begin{alltt}" html;
	       line + (Plugin.count_newlines s0)
	   | `Construct s ->
	       colorize param buf construct_color s;
	       line
	   | `Keyword k ->
	       (try 
		  let color = Hashtbl.find keyword_colors k in
		  colorize param buf color k;
		with Not_found -> 
		  let color =
		    match k.[0] with
			'a' .. 'z' -> alpha_keyword_color
		      | _ -> nonalpha_keyword_color in
		  colorize param buf color k);
	       line
	   | `Newline ->
	       Buffer.add_char buf '\n';
	       if rest <> [] then
		 line_comment param buf line;
	       line + 1
	   | `Tab ->
	       if param.tab_size < 0 then Buffer.add_char buf '\t'
	       else add_string buf (String.make param.tab_size ' ');
	       line
	   | `Start_annot (info, annot) -> line
	   | `Stop_annot info -> line)
      2 l in
  ()


let esc s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '_' | '{' | '}' | '%' | '~' as c -> bprintf buf "\\%c" c
      | '\\' -> bprintf buf "$\\backslash$"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let ocaml_file
  ?(filename = "") 
  ?keyword_colors
  ~param
  buf l =
  
  if param.title then
    bprintf buf "\\section{\\tt %s}\n" (esc filename);

  Buffer.add_string buf "\n\\begin{alltt}\n";
  line_comment param buf 1;
  ocaml ?keyword_colors ~param buf l;
  Buffer.add_string buf "\\end{alltt}\n"



let begin_document ?(param = default_param) buf files =
  bprintf buf "\
%% Auto-generated by caml2html %s from %s
\\documentclass{article}
\\usepackage{alltt}
\\usepackage{color}
"
    Version.version (String.concat ", " files);
  bprintf buf "%s\n" param.defs;
  Buffer.add_string buf "\\begin{document}\n"


let end_document ?(param = default_param) buf =
  Buffer.add_string buf "\\end{document}\n"


let handle_file ?keyword_colors ?(param = default_param) buf filename =
  let l = Input.file filename in
  ocaml_file ?keyword_colors ~param ~filename buf l

let rec mkdir_p dir =
  if Sys.file_exists dir then ()
  else
    (mkdir_p (Filename.dirname dir);
     Unix.mkdir dir 0o777)

let save_file ?(dir = "") buf file =
  let dir_res_name =
    if dir = "" then file
    else
      (mkdir_p dir;
       Filename.concat dir file) in
  let chan_out = open_out dir_res_name in
  Buffer.output_buffer chan_out buf;
  close_out chan_out

let ocaml_document ?dir ?keyword_colors ?param files outfile =
  let buf = Buffer.create 50_000 in
  begin_document ?param buf files;
  let rec tmp = function
    | [] -> ()
    | [x] -> handle_file ?keyword_colors ?param buf x
    | x :: l ->
	handle_file ?keyword_colors ?param buf x;
        Buffer.add_string buf "\n\\rule{\\textwidth}{1pt}\n";
        tmp l in
  tmp files;
  end_document ?param buf;
  save_file ?dir buf outfile
