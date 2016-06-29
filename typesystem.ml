
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
 * Construire la matrice représentant 
 * le système linéaire d'équations 
 * de types
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

type v_id   = int;;
type c_var  = Const of int | Var of v_id;;

let varid = ref (-1);;

let newvarid () = incr varid; !varid;;

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

let var_type nom a b= 
    let ivar = newvarid () in 
    let ovar = newvarid () in  
    let c = [ [ (1,Var ivar); (-1, Const a)];
                      [ (1,Var ovar); (-1, Const b)]]
    in
    (c,  
      { itype  = Var ivar;
      otype  = Var ovar; 
      vtypes = VarType.singleton nom [(ivar,ovar)]
      });;

let compose_type types = 
    let ivar = newvarid () in 
    let ovar = newvarid () in 
    (Var ivar, Var ovar, { 
      itype  = Var ivar;
      otype  = Var ovar;
      vtypes = types 
    });;

let union_vtypes a b = 
    union_vars a.vtypes b.vtypes;;

let remove_variables l m = 
    List.fold_left (fun x (y,z) -> VarType.remove z (VarType.remove y x)) m l;; 

let eqn_fam a x = 
    if not (VarType.mem x a.vtypes) then 
        []
    else 
        let tmpvari = newvarid () in  
        let tmpvaro = newvarid () in  
        a.vtypes |> VarType.find x 
                 |> List.map (fun (i,o) -> [ [ (1, Var i); (-1, Var tmpvari) ];
                                             [ (1, Var o); (-1, Var tmpvaro) ] ])
                 |> List.concat;;

(** Le calcul de type le plus simple du monde **)
let calcul_type circuit = 
    varid := (-1); (* FIXME : ugly !!!! *)
    let constraints = ref [] in (* list of equations *) 
    let add_constraints l = l 
                 |> make_equations  
                 |> (fun e -> constraints := e @ !constraints)
    in
    let accum_type = function
        | Id x          -> (* let (i,o,r) = compose_type VarType.empty in (* base_type x x *)
                           add_constraints [ [ (1, i) ; (-1, o) ] ];
                           r *) base_type x x
        | IdPoly        -> let (i,o,r) = compose_type VarType.empty in 
                           add_constraints [ [ (1, i) ; (-1, o) ] ];
                           r
        | Const (x,y,z) -> base_type y z
        | VarI  y       -> let (c,v) = var_type y 0 1 in 
                           add_constraints c;
                           v
        | VarO  y       -> let (c,v) = var_type y 1 0 in 
                           add_constraints c;
                           v
        | Par (a,b)     -> 
                let (i,o,r) = compose_type (union_vtypes a b) in 
                let eqn_i = [ (1,a.itype) ; (1,b.itype) ; (-1, i)] in
                let eqn_o = [ (1,a.otype) ; (1,b.otype) ; (-1, o)] in  
                add_constraints [eqn_i ; eqn_o ];
                r
        | Seq (a,b)     -> 
                let (i,o,r) = compose_type (union_vtypes a b) in   
                let eqn_join = [ (1,a.otype) ; (-1,b.itype) ] in
                let eqn_inpt = [ (1,a.itype) ; (-1, i) ] in 
                let eqn_opt  = [ (1,b.otype) ; (-1, o) ] in 
                add_constraints [eqn_join ; eqn_inpt ; eqn_opt ];
                r
        | Links (l,a)   -> 
                let (i,o,r) = compose_type (remove_variables l a.vtypes) in
                let eqn_i = [ (1,a.itype) ; (-1,i) ] in
                let eqn_o = [ (1,a.otype) ; (-1,o) ] in
                let eqn_fams = l |> List.map (fun (x,y) -> eqn_fam a x @ eqn_fam a y) |> List.concat in
                add_constraints (eqn_i :: eqn_o :: eqn_fams);
                r
    in
    let resulting_type = foldc accum_type circuit in
    let nvar = newvarid () in 
    let (m,b) = construire_matrice !constraints nvar in 
    !constraints |> List.iter (fun (s,c) -> List.iter (fun (n,v) -> print_int n; print_string "* x_"; print_int v; print_string " + ") s;
                                                        print_string " = "; print_int c; print_newline ());
    match resolution_type m b with
        | Solution _    -> (resulting_type, !constraints)
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

    let linked_sub = links [("i1","o2"); ("i2","o1"); ("i3", "o1"); ("i4", "o2")] sub in
    linked_sub;; 

let test3 = 
    let bloc i o v = (vari "c" ||| vari "x" ||| vari i) === const "B" 3 1 === const v 1 1 === varo o in 
    let b1     = bloc "i1" "o1" "F" in 
    let b2     = bloc "i2" "o2" "G" in 
    let b3     = links [("i1","o2");("i2","o1")] ((b1 ||| b2) === (vari "i2" ||| vari "i1" ||| vari "c") === const "B" 3 1) in 
    let b4     = bindi "x" (bindi "c" b3) in 
    b4;;


let tests = [
    ("test1 a", fun () -> print_newline (); let _ = calcul_type test1a in  ()); 
    ("test1 b", fun () -> print_newline (); let _ = calcul_type test1b in  ()); 
    ("test1 c", fun () -> print_newline (); let _ = calcul_type test1c in  ()); 
    ("test2", fun () -> let _ = calcul_type test2 in ());
    (* ("test3", fun () -> let _ = calcul_type test3 in ()); *)
];;

