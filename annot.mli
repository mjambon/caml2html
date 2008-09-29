type layer_info = { innermost : bool;
		    outermost : bool }

type tag = [ `Start of string | `Stop ] * (Lexing.position * layer_info)

type filter = [ `All | `Innermost | `Outermost ]

val parse :
  impl_file:string ->
  annot_file:string -> tag list
val guess_annot_file : string -> string option
val from_file : impl_file:string -> annot_file:string -> tag list option
