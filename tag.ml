(* $Id$ *)
(* Various operations on lists of elements (Other) mixed with
   Start and Stop tags *)


(* The type of elements *)
type kind = Start | Stop | Other

(* Recursively remove consecutive start/stop pairs *)
let rec remove_matches = function
    (Start, _) as start :: l ->
      (match remove_matches l with
	   (Stop, _) :: rest -> rest
	 | rest -> start :: rest)
  | (Stop, _) as stop :: l -> stop :: remove_matches l
  | (Other, _) as x :: l -> x :: remove_matches l
  | [] -> []

(* Annotate innermost start/stop pairs *)
let rec annotate_innermost f = function
    (Start, a) :: l ->
      let other, next_stop = find_stop f [] l in
      (match next_stop with
	   (Stop, b) :: rest ->
	     (Start, f true a) :: other @ (Stop, f true b) ::
	     annotate_innermost f rest
	 | (Start, _) :: _ -> other @ annotate_innermost f next_stop
	 | (Other, _) :: _ -> assert false
	 | [] -> other)
  | (tag, x) :: l -> (tag, f false x) :: annotate_innermost f l
  | [] -> []
	
and find_stop f accu = function
    (Other, x) :: l -> find_stop f ((Other, f false x) :: accu) l
  | l -> (List.rev accu), l

(*
let start x = (Start, x);;
let stop x = (Stop, x);;
let other x = (Other, x);;
let annotate b x = (x, b);;
let l1, l2 =
  [ stop 1; stop 2; start 3; start 4; start 5; stop 5; start 6 ],
  [ stop 6; start 7; stop 7; stop 4; stop 3; start 8; stop 8; start 9 ];;
let l = remove_matches (l1 @ [other 10] @ l2);;
annotate_innermost annotate l;;
*)
