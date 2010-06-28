(* $Id$ *)
(* 
   Copyright 2002-2004 Sébastien Ailleret
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

open Printf

let version = "caml2html " ^ Version.version

type class_definition = (string list * (string * string) list)

(* This will be come before the token-specific color definitions *)
let default_default_style : class_definition list = 
  [ ["code"; "pre"], [ "color", "black";
		       "background-color", "white" ];
    ["a.Cannot"], [ "color", "black";
		    "text-decoration", "none" ] ]

let key_color1 = Some "green"
let key_color2 = Some "#77aaaa"
let key_color3 = Some "#cc9900"
let key_color4 = Some "#990099"
let key_color5 = Some "#808080"

let construct_color = (Some "#0033cc", None, "Cconstructor")
let comment_color = (Some "#990000", None, "Ccomment")
let string_color = (Some "#aa4444", None, "Cstring")
let quotation_color = (None, None, "Cquotation")
let annot_color = (None, Some "#b4eeb4", "Cannot:hover")
let background_color = (None, Some "white", "Cbackground")
let linenum_color = (Some "black", Some "silver", "Clinenum")

let alpha_keyword_color = (key_color5, None, "Calphakeyword")
let nonalpha_keyword_color = (None, None, "Cnonalphakeyword")

let default_keyword_color_list =
  [
    "and", (key_color1, None, "Cand");
    "as", (key_color1, None, "Cas");
    "class", (key_color1, None, "Cclass");
    "constraint", (key_color1, None, "Cconstraint");
    "exception", (key_color1, None, "Cexception");
    "external", (key_color1, None, "Cexternal");
    "fun", (key_color1, None, "Cfun");
    "function", (key_color1, None, "Cfunction");
    "functor", (key_color1, None, "Cfunctor");
    "in", (key_color1, None, "Cin");
    "inherit", (key_color1, None, "Cinherit");
    "initializer", (key_color1, None, "Cinitializer");
    "let", (key_color1, None, "Clet");
    "method", (key_color1, None, "Cmethod");
    "module", (key_color1, None, "Cmodule");
    "mutable", (key_color1, None, "Cmutable");
    "of", (key_color1, None, "Cof");
    "private", (key_color1, None, "Cprivate");
    "rec", (key_color1, None, "Crec");
    "type", (key_color1, None, "Ctype");
    "val", (key_color1, None, "Cval");
    "virtual", (key_color1, None, "Cvirtual");
    
    "do", (key_color2, None, "Cdo");
    "done", (key_color2, None, "Cdone");
    "downto", (key_color2, None, "Cdownto");
    "else", (key_color2, None, "Celse");
    "for", (key_color2, None, "Cfor");
    "if", (key_color2, None, "Cif");
    "lazy", (key_color2, None, "Clazy");
    "match", (key_color2, None, "Cmatch");
    "new", (key_color2, None, "Cnew");
    "or", (key_color2, None, "Cor");
    "then", (key_color2, None, "Cthen");
    "to", (key_color2, None, "Cto");
    "try", (key_color2, None, "Ctry");
    "when", (key_color2, None, "Cwhen");
    "while", (key_color2, None, "Cwhile");
    "with", (key_color2, None, "Cwith");
    
    "assert", (key_color3, None, "Cassert");
    "include", (key_color3, None, "Cinclude");
    "open", (key_color3, None, "Copen");
    
    "begin", (key_color4, None, "Cbegin");
    "end", (key_color4, None, "Cend");
    "object", (key_color4, None, "Cobject");
    "sig", (key_color4, None, "Csig");
    "struct", (key_color4, None, "Cstruct");
    
    "raise", (Some "red", None, "Craise");

    "asr", (key_color5, None, "Casr");
    "land", (key_color5, None, "Cland");
    "lor", (key_color5, None, "Clor");
    "lsl", (key_color5, None, "Clsl");
    "lsr", (key_color5, None, "Clsr");
    "lxor", (key_color5, None, "Clxor");
    "mod", (key_color5, None, "Cmod");
    
    "false", (None, None, "Cfalse");
    "true", (None, None, "Ctrue");

    "|", (key_color2, None, "Cbar");
  ]

let default_keyword_colors =
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun (s, (color, bgcolor, css_class)) -> 
       Hashtbl.add tbl s (color, bgcolor, css_class))
    default_keyword_color_list;
  tbl

let all_colors =
  linenum_color ::
  background_color ::
  construct_color ::
    comment_color ::
    annot_color ::
    string_color ::
    quotation_color ::
    alpha_keyword_color ::
    nonalpha_keyword_color ::
    (List.map snd default_keyword_color_list)

let make_style l =
  String.concat ";" (List.map (fun (name, value) -> name ^ ":" ^ value) l)

let inline_style (fg, bg, _class) =
  let colors = [] in
  let colors =
    match fg with 
	None -> colors
      | Some color -> ("color:" ^ color) :: colors in
  let colors =
    match bg with
	None -> colors
      | Some color -> ("background-color:" ^ color) :: colors in
  String.concat ";" colors

let make_classes
  ?(default = default_default_style)
  ?(colors = all_colors) () =
  let buf = Buffer.create 2000 in

  List.iter 
    (fun (classnames, style) ->
       if classnames <> [] then
	 bprintf buf "%s { %s }" 
	   (String.concat "," classnames) (make_style style))
    default;

  let color_groups = 
    Hashtbl2.list_all (Hashtbl2.of_list 50
			 (List.map (fun (a,b,c) -> ((a,b),c)) colors)) in
  List.iter 
    (fun ((fg, bg), l) -> 
       let color =
	 match fg with 
	     None -> ""
	   | Some color -> " color: " ^ color ^ ";" in
       let background_color =
	 match bg with
	     None -> ""
	   | Some color -> " background-color: " ^ color ^ ";" in
       bprintf buf ".%s {%s%s }\n" 
	 (String.concat ",\n." (List.sort String.compare l)) 
	 color background_color)
    color_groups;
  Buffer.contents buf
  
let make_css 
  ?(default = default_default_style)
  ?(colors = all_colors)
  file =
  let oc = open_out file in
  output_string oc (make_classes ~default ~colors ());
  close_out oc

let default_style = make_classes ()

type style = [ `Inline | `Inhead of string | `Url of string ]

type param = 
    { line_numbers : bool; 
      title : bool;
      body_only : bool;
      tab_size : int;
      footnote : bool;
      style : style;
      html_comments : bool;
      charset : string;
      annot_filter : Annot.filter;
      no_annot : bool;
      ie7 : bool (* if true, type annotations will not work 
		    on versions of Internet Explorer prior to IE 7
		    (but rendering is better) *) }

let default_param =
  { line_numbers = false; 
    title = false;
    body_only = false;
    tab_size = 8;
    footnote = true;
    style = `Inhead default_style;
    html_comments = false;
    charset = "iso-8859-1";
    annot_filter = `Innermost;
    no_annot = false;
    ie7 = false }


let add_string buf nbsp s = 
  String.iter
    (function
	 '<' -> Buffer.add_string buf "&lt;"
       | '>' -> Buffer.add_string buf "&gt;"
       | '&' -> Buffer.add_string buf "&amp;"
       | ' ' when nbsp -> Buffer.add_string buf "&nbsp;"
       | c -> Buffer.add_char buf c)
    s


let line_comment p buf i =
  if p.line_numbers then
    match p.style with
	`Inline -> (* should use color parameters *)
	  bprintf buf
	  "<span style=\"%s\">%4d:</span>\
           <span style=\"%s\"> </span>" 
	  (inline_style linenum_color) i (inline_style background_color)
      | `Inhead _ 
      | `Url _ ->
	  bprintf buf 
	    "<span class=\"Clinenum\">%4d:</span>\
             <span class=\"Cbackground\"> </span>" i

 
let colorize ?(comment = false) p buf nbsp style s =
  let add =
    if comment && p.html_comments then Buffer.add_string buf
    else add_string buf nbsp in
  match p.style with
      `Inhead _ | `Url _ ->
	let _, _, clas = style in
	bprintf buf "<span class=\"%s\">" clas;
	add s;
	Buffer.add_string buf "</span>"
    | `Inline ->
	match inline_style style with
	    "" -> add s
	  | sty ->
	      bprintf buf "<span style=\"%s\">" sty;
	      add s;
	      Buffer.add_string buf "</span>"

let compact_annot s =
  let space = ref true in
  let buf = Buffer.create 200 in
  String.iter (function
		   ' ' | '\n' | '\t' | '\r' ->
		     if !space then ()
		     else (space := true;
			   Buffer.add_char buf ' ')
		 | c ->
		     space := false;
		     Buffer.add_char buf c)
    s;
  Buffer.contents buf


let ignore_annot p info =
  p.no_annot || 
  p.annot_filter = `Innermost && not info.Annot.innermost ||
  p.annot_filter = `Outermost && not info.Annot.outermost

let hover_start p =
  if p.ie7 then "span"
  else "a href=\"javascript:;\""

let hover_stop p =
  if p.ie7 then "span"
  else "a"

let start_annot p buf info annot =
  if ignore_annot p info then ()
  else 
    ((* We use "a href" and not "span" 
	in order to make the hover work in IE 6. *)
      match p.style with
	  `Inline ->
	    bprintf buf 
	    "<%s style=\"text-decoration:none;%s\" \
                title=\"\"" 
	      (hover_start p)
	      (inline_style annot_color);
	    add_string buf false (compact_annot annot);
	    Buffer.add_string buf "\">"
	| `Inhead _ | `Url _ ->
	    bprintf buf 
	      "<%s style=\"text-decoration:none\" \
                  class=\"Cannot\" title=\""
	      (hover_start p);
	    add_string buf false (compact_annot annot);
	    Buffer.add_string buf "\">")

let stop_annot p buf info =
  if ignore_annot p info then () 
  else
    bprintf buf "</%s>" (hover_stop p)


let rec fold_left f accu l =
  match l with
      [] -> accu
    | a :: rest -> fold_left f (f accu a rest) rest

let ocaml
  ?(nbsp = false)
  ?(keyword_colors = default_keyword_colors)
  ?(param = default_param)
  buf l =
  
  let _last_line =
    fold_left
      (fun line token rest ->
	 match token with
	     `String s ->
	       colorize param buf nbsp string_color s;
	       line
	   | `Quotation s ->
	       colorize param buf nbsp quotation_color s;
	       line
	   | `Token s ->
	       add_string buf nbsp s;
	       line
	   | `Comment s ->
	       colorize ~comment:true param buf nbsp comment_color s;
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
	       bprintf buf "</pre>%s<pre>" html;
	       line + (Plugin.count_newlines s0)
	   | `Construct s ->
	       colorize param buf nbsp construct_color s;
	       line
	   | `Keyword k ->
	       (try 
		  let color = Hashtbl.find keyword_colors k in
		  colorize param buf nbsp color k;
		with Not_found -> 
		  let color =
		    match k.[0] with
			'a' .. 'z' -> alpha_keyword_color
		      | _ -> nonalpha_keyword_color in
		  colorize param buf nbsp color k);
	       line
	   | `Newline ->
	       Buffer.add_char buf '\n';
	       if rest <> [] then
		 line_comment param buf line;
	       line + 1
	   | `Tab ->
	       if param.tab_size < 0 then Buffer.add_char buf '\t'
	       else add_string buf nbsp (String.make param.tab_size ' ');
	       line
	   | `Start_annot (info, annot) -> (start_annot param buf info annot; 
					    line)
	   | `Stop_annot info -> stop_annot param buf info; line)
      2 l in
  ()

let ocamlcode
  ?annot
  ?keyword_colors
  ?(param = default_param)
  ?(tag_open = "<code>")
  ?(tag_close = "</code>")
  s =
  let buf = Buffer.create (10 * String.length s) in
  Buffer.add_string buf tag_open;
  line_comment param buf 1;
  ocaml ?keyword_colors ~param ~nbsp:true buf (Input.string ?annot s);
  Buffer.add_string buf tag_close;
  Buffer.contents buf

let ocamlpre
  ?annot
  ?keyword_colors
  ?(param = default_param)
  ?(tag_open = "<pre>")
  ?(tag_close = "</pre>")
  s =
  let buf = Buffer.create (10 * String.length s) in
  Buffer.add_string buf tag_open;
  line_comment param buf 1;
  ocaml ?keyword_colors ~param ~nbsp:false buf (Input.string ?annot s);
  Buffer.add_string buf tag_close;
  Buffer.contents buf


let is_valid_anchor =
  let re = Str.regexp "[A-Za-z][-A-Za-z0-9_:.]*$" in
  fun s -> Str.string_match re s 0
    
let ocaml_file
  ?(filename = "") 
  ?keyword_colors
  ~param
  buf l =
  
  let anchor = 
    if is_valid_anchor filename then 
      sprintf "<a name=\"%s\"></a>" filename
    else "" in

  if param.title then
    (bprintf buf "<h1>%s<code>%s</code></h1>\n" anchor filename;
     Buffer.add_string buf "\n<pre>")
  else
    bprintf buf "\n<pre>%s" anchor;

  line_comment param buf 1;
  ocaml ?keyword_colors ~param buf l;
  Buffer.add_string buf "</pre>\n"



let begin_document 
  ?(param = default_param)
  buf files =
  let rec make_title = function
    | [] -> ()
    | [a] -> Buffer.add_string buf a
    | a :: l -> Printf.bprintf buf "%s, " a; make_title l in
  bprintf buf "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \
\"http://www.w3.org/TR/html4/strict.dtd\">
<html>
<head>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=%s\">
  <title>
" param.charset;
  make_title files;
  Printf.bprintf buf
    "</title>\n  <meta name=\"GENERATOR\" content=\"caml2html %s\">\n" 
    Version.version;
  (match param.style with
       `Url url ->
	 Printf.bprintf buf
	 "  <link rel=\"stylesheet\" href=\"%s\" type=\"text/css\">\n" 
	url
     | `Inhead s ->
	 Printf.bprintf buf "<style type=\"text/css\">\n%s</style>\n" s
     | `Inline -> ());
  Buffer.add_string buf "</head>\n<body>\n"


let end_document ?(param = default_param) buf =
  if param.footnote then
    Buffer.add_string buf "
<hr>
<p>
<em>This document was generated using 
<a href=\"http://martin.jambon.free.fr/caml2html.html\">caml2html</a></em>
";
  Buffer.add_string buf "</body>\n</html>\n"


let handle_file ?keyword_colors ?(param = default_param) buf filename =
  let annot =
    match Annot.guess_annot_file filename with
	None -> None
      | Some annot_file ->
	  Annot.from_file 
	    ~impl_file:filename ~annot_file in
  let l = Input.file ?annot filename in
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
        Buffer.add_string buf "\n<hr>\n";
        tmp l in
  tmp files;
  end_document ?param buf;
  save_file ?dir buf outfile
