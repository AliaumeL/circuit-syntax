
(** The module for handling the AST *)

(** 
 * The non-recursive type 
 * that represents the circuit constructors
 *)
type 'a circuit =
    Par of 'a * 'a                                 (** Parallel composition of two values *)
  | Seq of 'a * 'a                                 (** Sequential composition of two values *)
  | Fork                                           (** Forking a wire *)
  | Join                                           (** Joining two wires *)
  | Forget                                         (** Forgetting a value *)
  | Create                                         (** Creating a value *)
  | VarI of string                                 (** An INPUT variable *)
  | VarO of string                                 (** An OUTPUT variable *)
  | Const of string * int * int                    (** A constant circuit named a with type (b,c) *)
  | Trace of 'a                                    (** The trace of a value *)
  | Id of int                                      (** N parallel wires *)
  | Twist                                          (** Switching two wires *)
  | BindI of string * 'a                           (** Binding an input variable *)
  | BindO of string * 'a                           (** Binding an output variable *)
  | Links of (string * string) list * 'a           (** Linking a set of variables *)

(** 
 * Applies a function at each « hole » of 
 * the 'a circuit 
 *)
val fmap : ('a -> 'b) -> 'a circuit -> 'b circuit

(** 
 * The recursive type 
 * corresponding to the 'a circuit 
 *)
type circ = Fix of circ circuit

val unfix : circ -> circ circuit
val fix : circ circuit -> circ

(**
 * Destroying a recursive circuit type (circ)
 * using a function to destroy a regular (non-recursive)
 * circuit type
 *)
val foldc : ('a circuit -> 'a) -> circ -> 'a


(** 
 *
 * Consturctors for to ease the use of the 
 * recursive circuit type (circ)
 *
 *)


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
