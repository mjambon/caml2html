type ('a, 'b) t = ('a, 'b list ref) Hashtbl.t

let create = Hashtbl.create
let clear = Hashtbl.clear

let add tbl key data =
  let r =
    try Hashtbl.find tbl key
    with Not_found ->
      let r = ref [] in
      Hashtbl.add tbl key r;
      r in
  r := data :: !r

let copy tbl =
  let tbl2 = Hashtbl.copy tbl in
  Hashtbl.iter (fun key r -> Hashtbl.replace tbl2 key (ref !r)) tbl;
  tbl2

let find tbl key =
  List.hd !(Hashtbl.find tbl key)

let find_all tbl key =
  !(Hashtbl.find tbl key)

let mem = Hashtbl.mem

let remove tbl key =
  try
    let r = Hashtbl.find tbl key in
    match !r with
	[data] -> Hashtbl.remove tbl key
      | hd :: tl -> r := tl
      | [] -> invalid_arg "remove"
  with Not_found -> ()

let remove_all = Hashtbl.remove

let replace tbl key data =
  try
    let r = Hashtbl.find tbl key in
    r := data :: (List.tl !r)
  with 
      Not_found -> Hashtbl.add tbl key (ref [data])

let replace_all tbl key l =
  try
    let r = Hashtbl.find tbl key in
    r := l
  with 
      Not_found -> Hashtbl.add tbl key (ref l)

let iter f tbl =
  Hashtbl.iter (fun key r -> f key (List.hd !r)) tbl

let iter_all f tbl =
  Hashtbl.iter (fun key r -> f key !r) tbl

let fold f tbl init =
  Hashtbl.fold (fun key r accu -> f key (List.hd !r) accu) tbl init

let fold_all f tbl init =
  Hashtbl.fold 
    (fun key r accu -> f key !r accu)
    tbl init

let list_keys tbl =
  fold (fun key _ accu -> key :: accu) tbl []

let list_values tbl =
  fold (fun _ data accu -> data :: accu) tbl []

let list_all_values tbl =
  fold_all (fun _ l accu -> l :: accu) tbl []

let list tbl =
  fold (fun key data accu -> (key, data) :: accu) tbl []

let list_all tbl =
  fold_all (fun key l accu -> (key, l) :: accu) tbl []

let of_list n l =
  let tbl = create n in
  List.iter (fun (key, data) -> add tbl key data) l;
  tbl

let of_keys n l =
  let tbl = create n in
  List.iter (fun key -> replace tbl key ()) l;
  tbl
