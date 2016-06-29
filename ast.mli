type 'a circuit =
    Par of 'a * 'a
  | Seq of 'a * 'a
  | VarI of string
  | VarO of string
  | Const of string * int * int
  | Id of int
  | IdPoly
  | Links of (string * string) list * 'a

val fmap : ('a -> 'b) -> 'a circuit -> 'b circuit
type circ = Circ of circ circuit
type 'a typed_circ = TCirc of ('a typed_circ circuit * 'a)

val foldc : ('a circuit -> 'a) -> circ -> 'a
val foldc_typed : ('a circuit -> 'b -> 'a) -> 'b typed_circ -> 'a

val ( === ) : circ -> circ -> circ
val ( ||| ) : circ -> circ -> circ
val vari : string -> circ
val varo : string -> circ
val const : string -> int -> int -> circ
val id : int -> circ
val idpoly : circ
val links : (string * string) list -> circ -> circ
val twist : circ
val trace : circ -> circ
val bindi : string -> circ -> circ
val bindo : string -> circ -> circ
val print_ast : circ -> string
