
type matrice = int array array
val construire_matrice :
  ((int * int) list * 'a) list -> int -> int array array

val print_matrix : int array array -> unit

type v_id = int

type c_var = Const of int | Var of v_id

val varid : v_id ref

val newvarid : unit -> v_id

type equation = (int * v_id) list * int

val print_equation : (int * int) list * int -> unit

type 'a var_type_map 

type c_type = {
  constraints : equation list;
  itype : c_var;
  otype : c_var;
  vtypes : (v_id * v_id) list var_type_map
}

val calcul_type : Ast.circ -> c_type
