type 'a circuit =
    Par of 'a * 'a
  | Seq of 'a * 'a
  | Fork
  | Join
  | Forget
  | Create
  | VarI of string
  | VarO of string
  | Const of string * int * int
  | Trace of 'a
  | Id of int
  | Twist
  | BindI of string * 'a
  | BindO of string * 'a
  | Links of (string * string) list * 'a
 
val fmap : ('a -> 'b) -> 'a circuit -> 'b circuit

type circ = Circ of circ circuit
type 'a typed_circ = TCirc of ('a typed_circ circuit * 'a)

val unfix : circ -> circ circuit
val fix : circ circuit -> circ

val foldc : ('a circuit -> 'a) -> circ -> 'a

val ( === ) : circ -> circ -> circ
val ( ||| ) : circ -> circ -> circ

val f : circ
val j : circ
val w : circ
val b : circ
val vari : string -> circ
val varo : string -> circ
val const : string -> int -> int -> circ
val id : int -> circ
val twist : circ
val trace : circ -> circ
val bindi : string -> circ -> circ
val bindo : string -> circ -> circ
val links : (string * string) list -> circ -> circ
