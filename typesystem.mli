type v_id  = int
type c_var = Const of int | Var of v_id

val calcul_type : Ast.circ -> (c_var * c_var) Ast.typed_circ * int array

val tests : (string * (unit -> unit)) list
