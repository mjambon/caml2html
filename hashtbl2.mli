(** This module provides a kind of hash tables where each key is 
present only once in the table, as opposed to the naive usage of 
the standard [Hashtbl] module.
Its main purpose is to provide efficient implementation
of functions such as [list_keys] with enhanced safety
over the direct use of an [('a, 'b list ref) Hashtbl.t] type.
Many functions have two variants:
- the first one is applied only on the current bindings, like
[iter].
- the second one has the [_all] suffix like [iter_all]
and is applied to the list of 
all the values that are bound to the given key 
instead of only to the topmost value.
This list of values
is prebuilt, so there is no cost for building the list when
such a function is applied.

Example - clustering elements:

[Hashtbl2.list_all (Hashtbl2.of_list 10 [ (1, "a"); (2, "b"); (1, "c") ])]

returns [[(2, ["b"]); (1, ["c"; "a"])]].

[Hashtbl2] is an additional layer over the standard [Hashtbl] module.

@author Martin Jambon *)

type ('a, 'b) t
(** The type of hash tables from type ['a] to type ['b].
   This representation is suitable for clustering elements 
   according to the given keys. *)

val create : int -> ('a, 'b) t
(** [Hashtbl2.create n] creates a new, empty hash table, with
   initial size [n].  For best results, [n] should be on the
   order of the expected number of elements that will be in
   the table.  The table grows as needed, so [n] is just an
   initial guess. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl2.add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Hashtbl2.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val find : ('a, 'b) t -> 'a -> 'b
(** [Hashtbl2.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Hashtbl2.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl2.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [Hashtbl2.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val remove_all : ('a, 'b) t -> 'a -> unit
(** [Hashtbl2.remove_all tbl x] removes all bindings of [x] in [tbl].
    It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl2.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl2.remove}[ tbl x]
   followed by {!Hashtbl2.add}[ tbl x y]. *)

val replace_all : ('a, 'b) t -> 'a -> 'b list -> unit
(** [Hashtbl2.replace_all tbl x y] replaces all bindings of [x]
    in [tbl] by bindings of [x] to the elements of [y].
    The first element of [y] defines the current binding,
    the second element is the defined the previous binding, and so on. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [Hashtbl2.iter f tbl] applies [f] to current bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each current binding is presented exactly once to [f].
   Hidden bindings are ignored.
   The order in which the bindings are passed to [f] is unspecified. *)

val iter_all : ('a -> 'b list -> unit) -> ('a, 'b) t -> unit
(** [Hashtbl2.iter_all f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and all the associated values
   as second argument in reverse order of introduction in the table.
   The order in which the bindings are passed to [f] is unspecified. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl2.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of current bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each current binding is presented exactly once to [f].
   Hidden bindings are ignored. *)

val fold_all : ('a -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl2.fold_all f tbl init] computes
   [(f kN lN ... (f k1 l1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [l1 ... lN] are the lists of associated values, in reverse order
   of introduction in the table. *)


val list_keys : ('a, 'b) t -> 'a list
(** [Hashtbl2.list_keys tbl] returns a list of all the keys
   from the current bindings. Therefore no key is duplicated.
   Order is unspecified. *)

val list_values : ('a, 'b) t -> 'b list
(** [Hashtbl2.list_values tbl] returns a list of all the values
   from the current bindings. Hidden bindings are ignored.
   Order is unspecified. *)

val list_all_values : ('a, 'b) t -> 'b list list
(** [Hashtbl2.list_all_values tbl] returns a list of all the values
   from all bindings. Order is unspecified. *)

val list : ('a, 'b) t -> ('a * 'b) list
(** [Hashtbl2.list tbl] returns a list of the current bindings.
   Order is unspecified. *)

val list_all : ('a, 'b) t -> ('a * 'b list) list
(** [Hashtbl2.list_all tbl] returns a list of all the bindings clustered
   according to their key. Order is unspecified. *)

val of_list : int -> ('a * 'b) list -> ('a, 'b) t
(** [Hashtbl2.of_list n l] converts a list of bindings into 
   a hash table of initial size [n]. The ordering of the list is the order
   of introduction of the bindings in the table. *)

val of_keys : int -> 'a list -> ('a, unit) t
(** [Hashtbl2.of_keys n l] converts a list of elements into
   a hash table of initial size [n] containing unique copies of these
   elements bound at most one time to [()]. *)
