
open Ast;;
open Utils;;

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


(***** Le système de type ******)

type v_id   = int;;
type c_var  = Const of int | Var of v_id;;

let varid = ref 0;;

let newvarid () = incr varid; print_int !varid; print_newline (); !varid;;

type equation = (int * v_id) list * int;;

let print_equation (l,r) = 
    let print_x id = "x(" ^ string_of_int id ^ ")" in 
    let print_prod (u,v) = string_of_int u ^ "*" ^ print_x v in
    let prods = String.concat " + " (List.map print_prod l) in 
    prods ^ " = " ^ string_of_int r ^ "\n" |> print_string;;

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


module VarType = Map.Make(String);; 

let union_vars = VarType.union (fun k x y -> Some (x @ y));; 

type c_type = {
    constraints : equation list;
    itype       : c_var;
    otype       : c_var;
    vtypes      : (v_id * v_id) list VarType.t 
};;

let base_type n m = 
    { constraints = [];
      itype  = Const n;
      otype  = Const m;
      vtypes = VarType.empty
    };;

let var_type nom = 
    let ivar = newvarid () in 
    let ovar = newvarid () in 
    { constraints = [];
      itype  = Var ivar;
      otype  = Var ovar;
      vtypes = VarType.singleton nom [(ivar,ovar)]
    };;

let cstr_type cstr types = 
    let ivar = newvarid () in 
    let ovar = newvarid () in 
    { constraints = cstr (Var ivar) (Var ovar);
      itype  = Var ivar;
      otype  = Var ovar;
      vtypes = types 
    };;

(** Le calcul de type le plus simple du monde **)
let calcul_type circuit = 
    let accum_type = function
        | Id x          -> base_type x x
        | Twist         -> base_type 2 2
        | Join          -> base_type 2 1
        | Fork          -> base_type 1 2
        | Forget        -> base_type 1 0
        | Create        -> base_type 0 1
        | Const (x,y,z) -> base_type y z 
        | VarI  y       -> var_type y 
        | VarO  y       -> var_type y 
        | Par (a,b)     -> 
                let eqn_i i = [ (1,a.itype) ; (1,b.itype) ; (-1, i)] in
                let eqn_o o = [ (1,a.otype) ; (1,b.otype) ; (-1, o)] in  
                let make_equations i o = 
                    [eqn_i i; eqn_o o] |> List.map equation_of_list |> (@) a.constraints
                in
                cstr_type make_equations (union_vars a.vtypes b.vtypes)
        | Seq (a,b)     -> 
                let eqn_join = [ (1,a.otype) ; (-1,b.itype) ] in
                let make_equations i o = 
                    [ eqn_join ] |> List.map equation_of_list |> (@) a.constraints
                in
                cstr_type make_equations (union_vars a.vtypes b.vtypes)
        | Trace a       -> 
                let eqn_i i = [ (1,a.itype) ; (-1,i) ; (-1, Const 1)] in
                let eqn_o o = [ (1,a.otype) ; (-1,o) ; (-1, Const 1)] in
                let make_equations i o = 
                    [eqn_i i; eqn_o o] |> List.map equation_of_list|> (@) a.constraints
                in
                cstr_type make_equations a.vtypes
        | BindI (x,a)   -> 
                let eqn_i i = [ (1,a.itype) ; (-1,i) ; (1, Const 1)] in
                let eqn_o o = [ (1,a.otype) ; (-1,o) ] in
                let eqn_fam = 
                    if not (VarType.mem x a.vtypes) then 
                        []
                    else 
                        let tmpvari = newvarid () in  
                        let tmpvaro = newvarid () in  
                        a.vtypes |> VarType.find x 
                                 |> List.map (fun (i,o) -> [ [ (1, Var i); (-1, Var tmpvari) ];
                                                             [ (1, Var o); (-1, Var tmpvaro) ] ])
                                 |> List.concat
                in
                let make_equations i o = 
                    (eqn_i i :: eqn_o o :: eqn_fam) |> List.map equation_of_list |> (@) a.constraints
                in
                cstr_type make_equations (VarType.remove x a.vtypes) 
        | BindO (x,a)   -> 
                let eqn_i i = [ (1,a.itype) ; (-1,i) ] in
                let eqn_o o = [ (1,a.otype) ; (-1,o) ; (1, Const 1)] in
                let eqn_fam = 
                    if not (VarType.mem x a.vtypes) then 
                        []
                    else 
                        let tmpvari = newvarid () in  
                        let tmpvaro = newvarid () in  
                        a.vtypes |> VarType.find x 
                                 |> List.map (fun (i,o) -> [ [ (1, Var i); (-1, Var tmpvari) ];
                                                             [ (1, Var o); (-1, Var tmpvaro) ] ])
                                 |> List.concat
                in
                let make_equations i o = 
                    (eqn_i i :: eqn_o o :: eqn_fam) |> List.map equation_of_list |> (@) a.constraints
                in
                cstr_type make_equations (VarType.remove x a.vtypes) 
        | Links (l,a)   -> 
                let eqn_i i   = [ (1,a.itype) ; (-1,i) ] in
                let eqn_o o   = [ (1,a.otype) ; (-1,o) ] in
                let eqn_fam x = 
                    if not (VarType.mem x a.vtypes) then 
                        []
                    else 
                        let tmpvari = newvarid () in  
                        let tmpvaro = newvarid () in  
                        a.vtypes |> VarType.find x 
                                 |> List.map (fun (i,o) -> [ [ (1, Var i); (-1, Var tmpvari) ];
                                                             [ (1, Var o); (-1, Var tmpvaro) ] ])
                                 |> List.concat
                in
                let eqn_fams = l |> List.map (fun (x,y) -> eqn_fam x @ eqn_fam y) |> List.concat in
                let make_equations i o = 
                    (eqn_i i :: eqn_o o :: eqn_fams) |> List.map equation_of_list|> (@) a.constraints
                in
                cstr_type make_equations a.vtypes
    in
    foldc accum_type circuit;; 
