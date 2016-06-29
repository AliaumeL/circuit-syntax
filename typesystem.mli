val construire_matrice :
  ((int * int) list * int) list -> int -> float array array * float array
type v_id = int
type c_var = Const of int | Var of v_id
val varid : v_id ref
val newvarid : unit -> v_id
type equation = (int * v_id) list * int
val print_equation : (int * int) list * int -> unit
val equation_of_list : (int * c_var) list -> (int * v_id) list * int
val make_equations :
  (int * c_var) list list -> ((int * v_id) list * int) list
module VarType :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type 'a var_type_map = 'a VarType.t
val union_vars :
  (v_id * v_id) list VarType.t ->
  (v_id * v_id) list VarType.t -> (v_id * v_id) list VarType.t
type c_type = {
  itype : c_var;
  otype : c_var;
  vtypes : (v_id * v_id) list var_type_map;
}
val base_type : int -> int -> c_type
val var_type :
  VarType.key -> int -> int -> v_id * v_id * (int * c_var) list list * c_type
val union_vtypes : c_type -> c_type -> (v_id * v_id) list VarType.t
val remove_variables :
  (VarType.key * VarType.key) list -> 'a VarType.t -> 'a VarType.t
val eqn_fam : c_type -> VarType.key -> (int * c_var) list list
val imageV : elem:'a -> func:('a * 'b) list -> 'b option
val remove_duplicates : 'a list -> 'a list
val of_option : 'a option -> 'a
val linking_equations :
  c_type -> (VarType.key * VarType.key) list -> (int * c_var) list list
val calcul_type : Ast.circ -> c_type * ((int * v_id) list * int) list
val test1a : Ast.circ
val test1b : Ast.circ
val test1c : Ast.circ
val test2 : Ast.circ
val test3 : Ast.circ
val test4 : Ast.circ
val tests : (string * (unit -> unit)) list
