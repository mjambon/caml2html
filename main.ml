(* $Id$ *)
(*
   Copyright 2002-2004 Sébastien Ailleret
   Copyright 2004-2007, 2010 Martin Jambon

   This file is distributed under the terms of the GNU Public License
   http://www.gnu.org/licenses/gpl.txt
*)

open Printf

open Output

let line_numbers = ref default_param.line_numbers
let title = ref default_param.title
let tab_size = ref default_param.tab_size
let footnote = ref default_param.footnote
let style = ref default_param.style
let raw_comments = ref default_param.html_comments
let charset = ref default_param.charset
let annot_filter = ref default_param.annot_filter
let no_annot = ref default_param.no_annot
let ie7 = ref default_param.ie7
let out_format = ref (`Html : [`Html | `Latex ])
let body_only = ref false


let get_html_param () = {
  Output.line_numbers = !line_numbers;
  title = !title;
  body_only = !body_only;
  tab_size = !tab_size;
  footnote = !footnote;
  style = !style;
  html_comments = !raw_comments;
  charset = !charset;
  annot_filter = !annot_filter;
  no_annot = !no_annot;
  ie7 = !ie7
}

let get_latex_param () = {
  Output_latex.line_numbers = !line_numbers;
  title = !title;
  body_only = !body_only;
  tab_size = !tab_size;
  latex_comments = !raw_comments;
  defs = Output_latex.default_param.Output_latex.defs
}

let get_param () =
  match !out_format with
      `Html -> `Html (get_html_param ())
    | `Latex -> `Latex (get_latex_param ())


(* output file *)
let res_file = ref ""
(* output directory *)
let res_dir = ref ""

let usage =
  "
Caml2html colorizes a set of OCaml source files (.ml, .mli, .mll, .mly, ...).
Type annotations will be shown when the mouse pointer passes over
an expression if the corresponding .annot file is available.
To obtain a .annot file, compile with ocamlc -dtypes or ocamlopt -dtypes.

Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [options] file1 ... fileN
Options:"


let speclist =
  [
   ("-annotfilter",
    Arg.Symbol (["innermost"; "outermost"],
		(function
		     "innermost" -> annot_filter := `Innermost
		   | "outermost" -> annot_filter := `Outermost
		   | _ -> assert false)),
    "
          choose whether innermost or outermost type annotations
          should be used (default: innermost)");

   ("-charset", Arg.String (fun s -> charset := s),
    sprintf "\
     <charset>
          specify charset to use (default: %s)" default_param.charset);


   ("-css", Arg.Unit (fun () -> style := `Url "style.css"),
    "
          use CSS named style.css for styling");

   ("-cssurl", Arg.String (fun s -> style := `Url s),
    "<URL>
          use the given URL as CSS for styling");

   ("-inhead", Arg.Unit (fun () -> style := `Inhead Output.default_style),
    "
          use default styling and place it in the <head> section
          of the document (default when applicable)");

   ("-inline", Arg.Unit (fun () -> style := `Inline),
    "
          use inline styling (HTML only, default fallback
          if -inhead is not applicable)");

   ("-body", Arg.Set body_only,
    "
          output only document's body, for inclusion into an
          existing document (see also -make-css and -make-latex-defs)");

   ("-ln", Arg.Unit (fun () -> line_numbers := true),
    "
          add line number at the beginning of each line");

   ("-hc", Arg.Unit (fun () -> raw_comments := true),
    "
          comments are treated as raw HTML or LaTeX code
          (no newlines inside of tags)");

   ("-t", Arg.Unit (fun () -> title := true),
    "
          add a title to the HTML page");

   ("-nf", Arg.Unit (fun () -> footnote := false),
    "
          do not add footnotes to the HTML page");

   ("-ie7", Arg.Set ie7,
    "
          drop support for type annotations on Internet Explorer 6 and older");

   ("-noannot", Arg.Set no_annot,
    "
          do not insert type annotations as read from .annot files
          (HTML output only)");

   ("-notab", Arg.Unit (fun () -> tab_size := -1),
    "
          do not replace tabs by spaces");

   ("-tab", Arg.Set_int tab_size,
    "<integer>
          replace tab by n spaces (default = 8)");

   ("-d", Arg.String (fun s -> res_dir := s),
    "<directory>
          generate files in directory dir, rather than in current directory");

   ("-o", Arg.String (fun s -> res_file := s),
    "<filename>
          output file");

   ("-v", Arg.Unit (fun () -> Printf.printf "%s\n" version; exit 0),
    "
          print version number to stdout and exit");

   ("-make-css", Arg.String (fun s -> Output.make_css s; exit 0),
    "<filename>
          create CSS file with default color definitions and exit");

   ("-ext", Arg.String Plugin.register_command,
    "<NAME:CMD>
          use the given external command CMD to handle comments that start
          with (*NAME. NAME must be a lowercase identifier.");


   ("-latex", Arg.Unit (fun () -> out_format := `Latex),
    "
          output LaTeX code instead of HTML.");

   ("-make-latex-defs",
    Arg.String (fun s -> Output_latex.make_defs_file s; exit 0),
    "<filename>
          create a file containing the default LaTeX color definitions
          and matching highlighting commands, and exit.
          \\usepackage{alltt,color} is not included.");
 ]



let handle_stdin_to_stdout out_format =
  let buf = Buffer.create 8192 in
  let l = Input.channel stdin in
  (match out_format with
       `Html param ->
         if not param.Output.body_only then
           Output.begin_document ~param buf [];
         Output.ocaml_file ~param buf l;
         if not param.Output.body_only then
           Output.end_document ~param buf
     | `Latex param ->
         if not param.Output_latex.body_only then
           Output_latex.begin_document ~param buf [];
         Output_latex.ocaml_file ~param buf l;
         if not param.Output_latex.body_only then
           Output_latex.end_document ~param buf
  );
  Buffer.output_buffer stdout buf

let manage_files out_format files =
  match out_format with
      `Html param ->
        if !res_file = "" then
          (* handles files separately *)
          let manage_one file =
            let buf = Buffer.create 8192 in
            if not param.Output.body_only then
              Output.begin_document ~param buf [file];
            Output.handle_file ~param buf file;
            if not param.Output.body_only then
              Output.end_document ~param buf;
            Output.save_file ~dir:!res_dir buf (file ^ ".html")
          in
          List.iter manage_one files
        else
          (* groups all files into one *)
          Output.ocaml_document ~param ~dir: !res_dir files !res_file
    | `Latex param ->
        if !res_file = "" then
          let manage_one file =
            let buf = Buffer.create 8192 in
            if not param.Output_latex.body_only then
              Output_latex.begin_document ~param buf [file];
            Output_latex.handle_file ~param buf file;
            if not param.Output_latex.body_only then
              Output_latex.end_document ~param buf;
            Output_latex.save_file ~dir:!res_dir buf (file ^ ".tex")
          in
          List.iter manage_one files
        else
          Output_latex.ocaml_document ~param ~dir: !res_dir files !res_file


let () =
  let files = ref [] in
  Arg.parse
    speclist
    (fun x -> files := x :: !files)
    usage;
  if !res_file = "" && !files = [] then
    (title := false;
     handle_stdin_to_stdout (get_param ()))
  else
    (if !res_file <> "" && ((List.length !files) >= 2) then
       title := true;
     (manage_files (get_param ())) (List.rev !files))
