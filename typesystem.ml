
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

(***
 *
 * On fait un pivot de gauss
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

let print_line b = 
    Array.iter (fun i -> print_string " "; print_float i; print_string " ") b;
    print_newline ();;

let print_matrix m = 
    Array.iter (fun l -> 
        Array.iter (fun i -> print_string " "; print_float i; print_string " ") l;
        print_newline ()) m;;


let permute_lignes i j v =   
    let tmp = v.(i) in 
    v.(i) <- v.(j);
    v.(j) <- tmp;;

let pivot_colonne j m =  
    let k = ref j in 
    for i = 0 to Array.length m - 1 do 
        if m.(i).(j) <> 0. then 
            k := i
    done;
    if m.(!k).(j) <> 0. then
        Some (!k)
    else
        None;;

let eliminate j m b =   
    let p = m.(j).(j) in 
    for i = j + 1 to Array.length m - 1 do 
        let v = m.(i).(j) in 
        let c = v /. p in 
        for k = 0 to Array.length m.(j) - 1 do 
            m.(i).(k) <- m.(i).(k) -. m.(j).(k) *. c 
        done;
        b.(i) <- b.(i) -. b.(j) *. c
    done;;

let produit = Array.map2 (fun a b -> a *. b);;  
let somme   = Array.fold_left (fun a b -> a +. b) 0.;;

let gauss_test m b = 
    try
        for j = 0 to min (Array.length m.(0) - 1) (Array.length m - 1) do 
            begin
                match pivot_colonne j m with 
                    | Some k -> 
                        permute_lignes j k m;
                        permute_lignes j k b;
                        eliminate j m b
                    | None -> raise Exit;
            end
        done;
        Some true;
    with Exit -> None;;

let is_valid_elim m b = 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    range (ligs - cols) 
        |> List.map (fun i -> Array.for_all ((=) 0.) m.(cols + i - 1) && b.(i + cols - 1) = 0.) 
        |> List.for_all (fun x -> x);;

let remontee_types rm rb = 
    let cols = Array.length rm.(0) in 
    let xs   = Array.make cols 0.  in
    for i = cols - 1 downto 0 do  
        xs.(i) <- (rb.(i) -. somme (produit xs rm.(i))) /. rm.(i).(i) 
    done;
    xs;;


let resolution_type m b =  
    let rm = Array.map Array.copy m in     
    let rb = Array.copy b in 
    match gauss_test rm rb with
        | None -> failwith "Erreur de typage : pas assez de contraintes"
        | Some _ -> 
            if is_valid_elim rm rb then 
                remontee_types rm rb
            else
                failwith "Erreur de typage : contraintes insatisfaisables";;



(***** Le système de type ******)

type v_id   = int;;
type c_var  = Const of int | Var of v_id;;

let varid = ref (-1);;

let newvarid () = incr varid; !varid;;

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
    let constraints = ref [] in (* list of equations *) 
    let add_constraints l = l 
                 |> make_equations  
                 |> (fun e -> constraints := e @ !constraints)
    in
    let accum_type = function
        | Id x          -> base_type x x
        | Twist         -> base_type 2 2
        | Join          -> base_type 2 1
        | Fork          -> base_type 1 2
        | Forget        -> base_type 1 0
        | Create        -> base_type 0 1
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
        | Trace a       -> 
                let (i,o,r) = compose_type a.vtypes in   
                let eqn_i = [ (1,a.itype) ; (-1,i) ; (-1, Const 1)] in
                let eqn_o = [ (1,a.otype) ; (-1,o) ; (-1, Const 1)] in
                add_constraints [eqn_i ; eqn_o ];
                r
        | BindI (x,a)   -> 
                let (i,o,r) = compose_type (VarType.remove x a.vtypes) in   
                let eqn_i = [ (1,a.itype) ; (-1,i) ; (1, Const 1)] in
                let eqn_o = [ (1,a.otype) ; (-1,o) ] in
                add_constraints (eqn_i :: eqn_o :: eqn_fam a x);
                r
        | BindO (x,a)   -> 
                let (i,o,r) = compose_type (VarType.remove x a.vtypes) in   
                let eqn_i = [ (1,a.itype) ; (-1,i) ] in
                let eqn_o = [ (1,a.otype) ; (-1,o); (1, Const 1) ] in
                add_constraints (eqn_i :: eqn_o :: eqn_fam a x);
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
    print_line (resolution_type m b);
    (resulting_type, !constraints);;



