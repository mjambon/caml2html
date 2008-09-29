open Printf
open Scanf
open Lexing

type t = { start : position;
	   stop : position;
	   typ : string }

type layer_info = { innermost : bool;
		    outermost : bool }

type tag = [ `Start of string | `Stop ] * (position * layer_info)

let create_pos file line linechar char =
  { pos_fname = file;
    pos_lnum = line;
    pos_bol = linechar;
    pos_cnum = char }

(* The format of .annot files provides the fields that are required
   by the standard Lexing.position type.
   That's convenient, however the pos_bol and pos_cnum are relative
   to the .ml file from which the information is extracted.
   This works if the source file is the .ml file, but if it has line directives
   indicating that the source is another file such as a .mll or .mly,
   the pos_fname and pos_lnum fields will correctly point to the
   source file, while the pos_bol and pos_cnum fields will point to the
   position in the .ml file, because line directives don't allow to retrieve
   this information.

   As a consequence, we must use the (line,char) positions and not
   absolute character position.
*)   
let parse_item pos_line type_lines =
  sscanf pos_line "%S %i %i %i %S %i %i %i"
    (fun file1 line1 linechar1 char1 file2 line2 linechar2 char2 ->
       let pos1 = create_pos file1 line1 linechar1 char1 in
       let pos2 = create_pos file2 line2 linechar2 char2 in
       { start = pos1;
	 stop = pos2;
	 typ = String.concat "\n" type_lines })


(* Pervasives.compare is not guaranteed to work like this: *)
let compare_arrays a b =
  let c = compare (Array.length a) (Array.length b) in
  if c <> 0 then c
  else
    let result = ref 0 in
    try
      for i = 0 to Array.length a - 1 do
	let c = compare a.(i) b.(i) in
	if c <> 0 then
	  (result := c;
	   raise Exit)
      done;
      !result
    with Exit -> !result

let compare_tags (a, _) (b, _) = compare_arrays a b

let print_pos pos =
  printf "%S %i %i %i\n" pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum


(* Generate a sequence of nested opening and closing tags. *)
let tagify ~impl_file l =
  let info0 = { innermost = false; outermost = false } in
  let length x = x.stop.pos_cnum - x.start.pos_cnum in
  let tags =
    List.fold_left 
      (fun l x ->
	 if x.start.pos_fname <> impl_file || 
	    x.stop.pos_fname <> impl_file then l
	 else
	   let len = length x in
	   let start = x.start in
	   let stop = x.stop in
	   let start_key = [| start.pos_lnum; start.pos_cnum - start.pos_bol; 
			      1; -len |] in
	   let stop_key = [| stop.pos_lnum; stop.pos_cnum - stop.pos_bol;
			     -1; len |] in
	   if compare_arrays start_key stop_key >= 0 then
	     (* Bad tagging! *)
	     (eprintf
		"Ignoring annotation: stop tag at or before start tag!\n%!";
	      l)
	   else
	     (start_key, (`Start x.typ, (x.start, info0))) :: 
	       (stop_key, (`Stop, (x.stop, info0))) :: l) [] l in
  List.map snd (List.sort compare_tags tags)

(* We keep only a sequence of non-nested annotations. 
   That's too bad, but it would have to be implemented in javascript
   and it's not so easy to implement something reliable.
   Without nesting, CSS with hover is sufficient, even in IE (but
   we must use <a> elements). *)
(*
let rec remove_outer_tags = function
    ((_, `Start _) as a) :: ((_, `Stop) as b) :: l ->
      a :: b :: remove_outer_tags l
  | (_, `Start _) :: ((_, `Start _) :: _ as l) -> remove_outer_tags l
  | (_, `Stop) :: l -> remove_outer_tags l
  | [] -> []
  | [(_, `Start _)] -> assert false

let rec remove_inner_tags = function
    (_, `Start _) as start :: l -> 
      let stop, rest = skip_tag_sequence 1 l in
      start :: stop :: remove_inner_tags rest
  | (_, `Stop) :: _ -> assert false
  | [] -> []
and skip_tag_sequence n = function
    (_, `Start _) :: l -> skip_tag_sequence (n+1) l
  | ((_, `Stop) as stop) :: l -> 
      let n = n - 1 in
      if n = 0 then stop, l
      else skip_tag_sequence n l
  | [] -> assert false
*)


let set_innermost (tag, (pos, x)) =
  (tag, (pos, { x with innermost = true }))

let set_outermost (tag, (pos, x)) =
  (tag, (pos, { x with outermost = true }))


let rec mark_innermost = function
    ((`Start _, _) as a) :: ((`Stop, _) as b) :: l ->
      set_innermost a :: set_innermost b :: mark_innermost l
  | ((`Start _, _) as a) :: ((`Start _, _) :: _ as l) -> a :: mark_innermost l
  | ((`Stop, _) as a) :: l -> a :: mark_innermost l
  | [] -> []
  | [(`Start _, _)] -> invalid_arg "Annot.mark_innermost"

let rec mark_outermost = function
    (`Start _, _) as start :: l -> 
      set_outermost start :: skip_tag_sequence 1 l
  | (`Stop, _) :: _ -> invalid_arg "Annot.mark_outermost"
  | [] -> []

and skip_tag_sequence n = function
    ((`Start _, _) as start) :: l -> start :: skip_tag_sequence (n+1) l
  | ((`Stop, _) as stop) :: l -> 
      let n = n - 1 in
      if n = 0 then set_outermost stop :: mark_outermost l
      else stop :: skip_tag_sequence n l
  | [] -> invalid_arg "Annot.skip_tag_sequence"

let set_layer_info l = mark_outermost (mark_innermost l)

(*
let z = { innermost = false; outermost = false };;
let start x = (`Start x, (x, z));;
let stop x = (`Stop, (x, z));;
let l = 
  [ start 1; stop 1; start 2; start 3; start 4; stop 4; stop 3; stop 2 ];;
mark_outermost (mark_innermost l);;
*)      


type filter = [ `All | `Innermost | `Outermost ]

(* impl_file is the file that we want to annotate and annot_file
   if the file that contains the annotation information.
   Usually impl_file is a .ml, but it may be a .mll or .mly file.
   Annotation files normally end in .annot and are produced
   by ocamlc or ocamlopt when -dtypes is specified.
   Only annotations that refer to impl_file are selected. *)
let parse ~impl_file ~annot_file =
  let ic = open_in annot_file in
  let l = ref [] in
  try
    while true do
      let line1 = input_line ic in
      try 
	let line2 = input_line ic in
	if line2 <> "type(" then
	  failwith "unrecognized format in .annot file";
	let rec loop () =
	    match input_line ic with
		")" -> []
	      | s -> s :: loop () in
	l := parse_item line1 (loop ()) :: !l
      with End_of_file -> failwith "unexpected end of file"
    done;
    assert false
  with
      End_of_file ->
	close_in ic;
	set_layer_info (tagify ~impl_file (List.rev !l))
    | e -> 
	close_in_noerr ic; 
	raise e

let guess_annot_file file =
  try 
    let name = Filename.chop_extension file ^ ".annot" in
    if Sys.file_exists name then Some name
    else None
  with _ -> None

(* impl_file is the file to annotate. See parse function above. *)
let from_file ~impl_file ~annot_file : tag list option =
  if Sys.file_exists annot_file then 
    Some (parse ~impl_file ~annot_file)
  else None
