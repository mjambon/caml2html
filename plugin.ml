(* $Id$ *)

open Printf

type handler =
    [ `Command of string
    | `Function of (string -> string option) ]

let plugins = Hashtbl.create 20

let add = Hashtbl.replace plugins
let exists = Hashtbl.mem plugins
let find = Hashtbl.find plugins


let count_newlines s = 
  let n = ref 0 in
  String.iter (
    function
	'\n' -> incr n
      | _ -> ()
  ) s;
  !n

let expand name s =
  let h =
    try find name
    with Not_found ->
      failwith (sprintf "Plugin %s doesn't exist." name)
  in
  match h with
      `Function f -> f s
    | `Command cmd ->
	let p = Unix.open_process cmd in
	let ic, oc = p in
	output_string oc s;
	close_out oc;
	let buf = Buffer.create 1024 in
	try
	  while true do
	    Buffer.add_string buf (input_line ic);
	    Buffer.add_char buf '\n'
	  done;
	  assert false
	with End_of_file ->
	  match Unix.close_process p with
	      Unix.WEXITED 0 -> Some (Buffer.contents buf)
	    | _ -> None

	    
let html_handler =
  `Function (fun s -> Some s)

let _ = 
  add "html" html_handler


let register_command s =
  try
    let i = String.index s ':' in
    let name = String.sub s 0 i in
    let cmd = String.sub s (i+1) (String.length s - i - 1) in
    if name = "" || cmd = "" then
      raise Not_found
    else
      add name (`Command cmd)
  with Not_found ->
    failwith (sprintf "Cannot register %S: wrong syntax" s)
