
(* Test file for <a href="http://martin.jambon.free.fr/caml2html.html">caml2html</a> (the first line is empty) *)

(* -hc option: <a href="#caml2html_test.mli">link to caml2html_test.mli</a> (same page, colorized)
 *             <a href="caml2html_test.ml">link to caml2html_test.ml</a> (source) *)

(* This is a multi-line "*)"
   comment *)

open Printf

type 'aa' weird = E10

type t = [ `A | `b of int | ` C | ` (* *) D | `
	    E ]

(* nested (* comments *) *)
(* "multi-
    line string in comment" *)

(*html
  <h2>Hello</h2>
  <p>
  This is 
  HTML!
  </p>
*)

(*date*)

(*rot13 Caml2html rules! "*)" *)

(*foo*)

module Zéro'04 = 
struct
  let characters = [ 'a'; '\000'; '\x12'; '
'; '\t'; 'z' ]
  let n = 0X12 + truncate 1.2E-1_2
  let the_Truth =
    let ignore4 a b c d = false in
    not (ignore4 1._0_None 1.0E10E10)
end

let hel'Lo = "\"Hello \
                World!\""

let ( |* ) a b =
  match a, b with
      1, 0 | 0, 1 -> 1+1
    | _ -> 0

let _ =
  assert true;
  if 0 mod 1 < 1 && `Abc <> `def then
    print_endline hel'Lo
;;

(* long types *)
let t x = (x, x)
let a x = t (t x)
let b x = a (a x)
let _ = fun x -> b (b x)
;;

# 123 (* line directives are not parsed, sorry... *)
