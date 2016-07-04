
open Ast;;
open Utils;;
open Solver;;

(* FIXME: 
    * Pour faire le système de type polymorphe 
    * 1) annoter chaque variable par un identifiant unique de type
    * 2) construire l'arbre avec l'annotation (pas trivial, portée des variables)
    * 3) Pour chaque constructeur, écrire une équation 
    * 4) Récupérer toutes les équations dans une matrice 
    * 5) Résoudre la matrice si possible 
    * 6) Remplacer les annotations par le type réel 
    *
    * Sachant qu'une annotation correspond à deux « variables » 
    * « nombre entrées » et « nombre sorties »
    *
    *)

(***
 *
 * Build the matrix that represents 
 * the linear system of type inference
 * equations
 *
 * @eqns : a list of equations
 * @vmax : the number of variables
 *
 *)
let construire_matrice eqns vmax = 
    let n   = List.length eqns in 
    let mat = Array.make_matrix n vmax 0. in  
    let b   = Array.make n 0. in 
    List.iteri (fun i (e,c) -> 
        b.(i) <- float_of_int c;
        List.iter (fun (v,j) -> mat.(i).(j) <- float_of_int v) e) eqns;
    (mat,b);;



(***** Le système de type ******)

type v_id   = int;;                        (** unique identifier for type variable *)
type c_var  = Const of int | Var of v_id;; (** possible expressions for a type *)

(***
 *
 * Variable identifier generation 
 *
 *)
let varid = ref (-1);;
let newvarid () = incr varid; !varid;;

(* Type system equation *)
type equation = (int * v_id) list * int;;


(** 
 * DEBUG PURPOSE
 *
 * permet d'afficher une équation
 *
 *)
let print_equation (l,r) = 
    let print_x id = "x(" ^ string_of_int id ^ ")" in 
    let print_prod (u,v) = string_of_int u ^ "*" ^ print_x v in
    let prods = String.concat " + " (List.map print_prod l) in 
    prods ^ " = " ^ string_of_int r ^ "\n" |> print_string;;

(**
 * Transforme une liste de constantes et variables
 * avec des multiplicateurs en une équation 
 * bien formée
 *)
let equation_of_list eqn = 
    let is_var = function 
          Var _ -> true
        | Const _ -> false 
    in
    let vars = eqn
            |> List.filter (fun (x,y) -> is_var y)
            |> List.map (fun (x, Var y) -> (x,y))
    in 
    let addV x = function 
          (_,Var _) -> x
        | (z,Const y) -> x + z * y
    in
    let cst = List.fold_left addV 0 eqn in   
    (vars, - cst);;

(**
 * Construit une liste d'équations en appliquant
 * equation_of_list sur chaque sous liste
 *)
let make_equations = List.map equation_of_list;; 

module VarType = Map.Make(String);; 
type 'a var_type_map = 'a VarType.t;;

let union_vars = VarType.union (fun k x y -> Some (x @ y));; 

type c_type = {
    itype       : c_var;
    otype       : c_var;
    vtypes      : (v_id * v_id) list var_type_map 
};;

let base_type n m = 
    { itype  = Const n;
      otype  = Const m;
      vtypes = VarType.empty
    };;

let var_type nom a b = 
    let ivar = newvarid () in 
    let ovar = newvarid () in  
    let c = [ [ (1,Var ivar); (-1, Const a)];
              [ (1,Var ovar); (-1, Const b)]]
    in
    (ivar,ovar,c,  
      { itype  = Var ivar;
      otype  = Var ovar; 
      vtypes = VarType.singleton nom [(ivar,ovar)]
      });;

let union_vtypes a b = 
    union_vars a.vtypes b.vtypes;;

let remove_variables l m = 
    List.fold_left (fun x (y,z) -> VarType.remove z (VarType.remove y x)) m l;; 



(* 
 * FIXME plante si on bind des variables 
 * qui n'existent pas 
 *
 *)
let linking_equations a l = 
    let vi = l |> List.map snd |> remove_duplicates in (* input variables bounded *) 
    let vo = l |> List.map fst |> remove_duplicates in (* output variables bounded *)

    let c  = vi |> List.map (fun v -> (v,newvarid ())) in (* node connectors for inputs *) 
    let d  = vo |> List.map (fun v -> (v,newvarid ())) in (* node connectors for outputs *) 
    
    let equations_input x = 
        try 
            a.vtypes |> VarType.find x  (* FIXME plante si x n'est pas une variable utilisée *)
                     |> List.map (fun (ivar,ovar) -> [ [ (1, Var ivar) ];
                                                       [ (1, Var ovar); (-1, Var (of_option (imageV x c))) ] ])  
                     |> List.concat
        with
            Not_found -> []
    in

    let equations_output x = 
        try
            a.vtypes |> VarType.find x  (* FIXME plante si x n'est pas une variable utilisée *)
                     |> List.map (fun (ivar,ovar) -> [ [ (1, Var ovar) ];
                                                       [ (1, Var ivar); (-1, Var (of_option (imageV x d))) ] ])  
                     |> List.concat
        with
            Not_found -> []
    in

    let equations_link = l
        |> List.map (fun (a,b) -> [ (1, Var (of_option (imageV a d))); (-1, Var (of_option (imageV b c)))])
    in

    (vi |> List.map equations_input |> List.concat) @ (vo |> List.map equations_output |> List.concat) @ equations_link;; 


(*** 
 * Function that calculates 
 * the type equations and then uses 
 * the Solver module to have an answer
 * and interpret it as a type error or
 * a type inference
 *
 *)
let calcul_type circuit = 
    varid := (-1); (* FIXME : ugly !!!! *)
    let constraints = ref [] in (* list of equations *) 
    let add_constraints l = l 
                 |> make_equations  
                 |> (fun e -> constraints := e @ !constraints)
    in
    let accum_type = function
        | Id x          -> (base_type x x, TCirc (Id x, (Const x, Const x)))  
        | IdPoly        -> 
                           let i = Var (newvarid ()) in 
                           let o = Var (newvarid ()) in 
                           let r = {
                               itype = i;
                               otype = o;
                               vtypes = VarType.empty
                                   }
                           in
                           add_constraints [ [ (1, i) ; (-1, o) ] ];
                           (r, TCirc (IdPoly, (i,o)))
        | Const (x,y,z) -> (base_type y z, TCirc (Const (x,y,z), (Const y, Const z)))
        | VarI  y       -> let (_,o,c,v) = var_type y 0 1 in 
                           add_constraints c;
                           (v, TCirc (VarI y, (Const 0, Var o)))
        | VarO  y       -> let (i,_,c,v) = var_type y 1 0 in 
                           add_constraints c;
                           (v, TCirc (VarO y, (Var i, Const 0)))
        | Par ((a,annotA),(b,annotB))     -> 
                let i = Var (newvarid ()) in 
                let o = Var (newvarid ()) in 
                let r = { 
                    itype = i;
                    otype = o;
                    vtypes = (union_vtypes a b)
                        } 
                in   
                let eqn_i = [ (1,a.itype) ; (1,b.itype) ; (-1, i)] in
                let eqn_o = [ (1,a.otype) ; (1,b.otype) ; (-1, o)] in  
                add_constraints [eqn_i ; eqn_o ];
                (r, TCirc (Par (annotA, annotB), (i,o)))

        | Seq ((a,annotA),(b,annotB))     -> 
                let r = { 
                    itype = a.itype;
                    otype = b.otype;
                    vtypes = (union_vtypes a b)
                        } 
                in   
                let eqn_join = [ (1,a.otype) ; (-1,b.itype) ] in
                add_constraints [eqn_join]; 
                (r, TCirc (Seq (annotA, annotB), (a.itype,b.otype)))

        | Links (l,(a,annotA))   -> 
                let r = {
                    itype = a.itype;
                    otype = a.otype;
                    vtypes = remove_variables l a.vtypes 
                        }
                in
                add_constraints (linking_equations a l);
                (r, TCirc (Links (l, annotA), (a.itype, a.otype)))
    in
    (** CONSTRUCTION, RESOLUTION and INTERPRETATION **)
    let (resulting_type,annotated) = foldc accum_type circuit in
    let nvar = newvarid () in 
    let (m,b) = construire_matrice !constraints nvar in 
    match resolution_type m b with
        | Solution v    -> (annotated,v) 
        | NoSol         -> failwith "Not typeable"
        | Negative _    -> failwith "Negative solution"
        | ManySol liste -> failwith ("Many solution ... fix variable : " ^ (liste |> List.map string_of_int |> String.concat " or ")) 




(****************************** TESTING ****************************)

let test1a = (vari "i2" ||| id 1) === const "G" 2 1 === varo "o2";;
let test1b = (id 1 ||| vari "i1") === const "F" 2 1 === varo "o1";; 
let test1c = (test1a ||| test1b) === (vari "i3" ||| vari "i4");;


let test2 = 
    let bloc1 = (id 1 ||| vari "i1") === const "F" 2 1 === varo "o1" in 

    let bloc2 = (vari "i2" ||| id 1) === const "G" 2 1 === varo "o2" in 

    let sub = (bloc1 ||| bloc2) === (vari "i3" ||| vari "i4") in 

    let linked_sub = links [("o2","i1"); ("o1","i2"); ("o1", "i3"); ("o2", "i4")] sub in
    linked_sub;; 

let test3 = 
    let bloc i o v = (vari "c" ||| vari "x" ||| vari i) === const "B" 3 1 === const v 1 1 === varo o in 
    let b1     = bloc "i1" "o1" "F" in 
    let b2     = bloc "i2" "o2" "G" in 
    let b3     = links [("o2","i1");("o1","i2")] ((b1 ||| b2) === (vari "i2" ||| vari "i1" ||| vari "c") === const "B" 3 1) in 
    let b4     = bindi "x" (bindi "c" b3) in 
    b4;;


let test4 = 
    trace (const "F" 2 2);;

let test5 = 
    bindi "x" ((id 1 ||| vari "x") === const "F" 2 1);;

let test6 = 
    const "F" 2 2 === twist === const "G" 2 2;; 

let tests = [
    ("test1 a",    fun () -> let _ = calcul_type test1a in ());
    ("test1 b",    fun () -> let _ = calcul_type test1b in ());
    ("test1 c",    fun () -> let _ = calcul_type test1c in ());
    ("test2",      fun () -> let _ = calcul_type test2  in ());
    ("test3",      fun () -> let _ = calcul_type test3  in ());
    ("test trace", fun () -> let _ = calcul_type test4  in ());
    ("test bind",  fun () -> let _ = calcul_type test5  in ());
    ("test twist", fun () -> let _ = calcul_type test6  in ());
];;

