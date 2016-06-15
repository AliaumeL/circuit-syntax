(** Set of small functions that are used to simplify 
 * the code of algorithms
 *)


(** Create a list containing numbers from 1 to n
 *
 * length (range n) = n
 * List.nth (range n) i = i+1    si 0 <= i <= n-1 
 *)
val range     : int -> int list

(** Create a list of size n with the same element
 *
 * length (replicate n e) = n
 * List.nth (replicate n e) = e si 0 <= i < n
 *)
val replicate : int -> 'a -> 'a list

val flip      : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(**
 * zipWith f a b = [f a1 b1; .... ; f aN bN] 
 *
 * length (zipWith f a b) = min (length a, length b)
 *
 *)
val zipWith   : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** 
 * Surrounds a text with two other strings
 *
 * If the string is empty, does NOT put the 
 * surroundings ! 
 *)
val surround  : string -> string -> string -> string
