(******
 *
 * Aliaume Lopez 
 *
 * Created : Ven 17 jui 2016 09:47:49 BST
 *
 * Solving linear equations 
 * and giving meaningful 
 * outputs 
 *)

open Utils;;

let produit = Array.map2 (fun a b -> a *. b);;  
let somme   = Array.fold_left (fun a b -> a +. b) 0.;;


(* Le type de solution qu'on peut attendre 
 * après une résolution des types 
 *)
type solution = 
      Solution   of int array
    | Negative   of int * int 
    | ManySol    of int list 
    | NoSol      ;; (* of int * int * int list;; *)


let print_line b = 
    Array.iter (fun i -> print_string " "; print_float i; print_string " ") b;
    print_newline ();;

let print_matrix m = 
    Array.iter (fun l -> 
        Array.iter (fun i -> print_string " "; print_float i; print_string " ") l;
        print_newline ()) m;;


(*
 * TODO
 *
 * Retourner non plus une string, mais les indices des variables 
 * et leurs valeurs, de manière à produire un affichage 
 * joli quand on aura accès aux « morceaux » d'expressions 
 *
 *)
let print_incoherence indice m b =   
    let non_null = Array.to_list m.(indice) 
                |> List.mapi (fun i v -> (i,v)) 
                |> List.filter (fun (i,v) -> v <> 0.) 
    in
    let egalite  = b.(indice) in  
    if non_null = [] then 
        "Sum of zeros equals to " ^ string_of_float egalite 
    else
        let defs = non_null |> List.map (fun (i,v) -> "x_{" ^ string_of_int i ^ "} = " ^ string_of_float v) 
                            |> String.concat "\n"
        in
        let equation = non_null 
                    |> List.map (fun (i,v) -> string_of_float m.(indice).(i) ^ " * x_{" ^ string_of_int i ^ "}")
                    |> String.concat " + " 
                    |> (fun s -> s ^ " = " ^ string_of_float egalite)  
        in
        defs ^ "\n AND \n" ^ equation;;
        



(*
 * Find a pivot for line for column j
 *
 *)
let find_pivot r j m =  
    let k = ref r in 
    let p = ref m.(r).(j) in 
    for i = r+1 to Array.length m - 1 do 
        if m.(i).(j) <> 0. && (!p = 0. || abs_float m.(i).(j) < abs_float !p) then 
            begin
                k := i;
                p := m.(i).(j)
            end
    done;
    if !p <> 0. then
        Some (!k)
    else
        None;;

(** Compute the transposition **)
let transpose m = 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    let nm = Array.make_matrix cols ligs 0. in
    Array.iteri (fun i ligne -> 
        Array.iteri (fun j valeur -> 
            nm.(j).(i) <- valeur)
        ligne) m;
    nm;;

let eliminate r j m b =   
    let p = m.(r).(j) in 
    for i = r + 1 to Array.length m - 1 do 
        let v = m.(i).(j) in 
        let c = v /. p in 
        for k = 0 to Array.length m.(j) - 1 do 
            m.(i).(k) <- m.(i).(k) -. m.(r).(k) *. c 
        done;
        b.(i) <- b.(i) -. b.(r) *. c
    done;;
(** 
 *
 * Tries to do gaussian elimination
 *
 * Si une colonne de zéros est trouvée,
 * alors on dit où elle est
 *
 *)

let gauss_elimination m b = 
    let r = ref 0 in 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    let maxi = min ligs cols in 
    let c    = ref true in 
    for j = 0 to maxi - 1 do
        match find_pivot !r j m with
            | None -> c := false
            | Some k -> 
                    permute_lignes !r k m;
                    permute_lignes !r k b;
                    eliminate !r j m b;
                    incr r;
    done;
    !c;;

(* Using the classical way to find a basis *)
let find_kernel_basis m = 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    let augmented = Array.make_matrix cols (ligs + cols) 0. in  
    let b    = Array.make cols 0. in  
    let is_m_column_null i = 
        range ligs |> List.map (fun x -> x - 1)
                   |> List.map (fun x -> augmented.(i).(x))
                   |> List.for_all ((=) 0.)
    in
    Array.iteri (fun i l -> 
        Array.iteri (fun j v -> 
            augmented.(j).(i) <- v 
        ) l) m;
    range cols |> List.iter (fun i -> augmented.(i-1).(ligs + i - 1) <- 1.);
    let _ = gauss_elimination augmented b in 
    print_newline ();
    range cols |> List.map (fun x -> x - 1)
               |> List.filter (fun i -> is_m_column_null i) 
               |> List.map (fun i -> Array.sub augmented.(i) ligs cols );;
                            

(* 
 *
 * Regarde si le bas de la matrice est bien fait .... Sinon 
 * donne l'indice d'une ligne qui ne correspond pas 
 * (TODO)
 *
 *)
let is_valid_elim m b = 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    range (ligs - cols) 
        |> List.map (fun i -> Array.for_all ((=) 0.) m.(cols + i - 1) && b.(i + cols - 1) = 0.) 
        |> List.for_all (fun x -> x);;


(* 
 * Trouve les numéros de lignes 
 * tels qu'il y ait une incohérence 
 * dans la matrice
 *)
let find_non_valid_elims m b = 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    range (ligs - cols) 
        |> List.filter (fun i -> b.(i + cols - 1) <> 0.);;


(* 
 * Termine la résolution 
 *
 *)
let remontee_types rm rb = 
    let cols = Array.length rm.(0) in 
    let ligs = Array.length rm in 
    let xs   = Array.make cols 0.  in
    for i = min (cols - 1) (ligs - 1) downto 0 do  
        xs.(i) <- (rb.(i) -. somme (produit xs rm.(i))) /. rm.(i).(i) 
    done;
    xs;;



(******
 *
 * La grosse fonction qui fait toute la résolution 
 *
 *)
let resolution_type m b =
    let rm   = Array.map Array.copy m in     
    let rb   = Array.copy b in 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    let r = gauss_elimination rm rb in 
    if is_valid_elim rm rb then 
        if r = true then 
                let solution = remontee_types rm rb in 
                match array_find ((>) 0.) solution with
                    | Some (i,j) -> Negative (i, int_of_float j) 
                    | None       -> begin 
                        print_newline ();print_line solution;
                                        match array_find (fun x -> x <> float_of_int (int_of_float x)) solution with 
                                           | Some _ -> NoSol  
                                           | None   -> Solution (Array.map int_of_float solution) 
                                    end
         else
            let kb = find_kernel_basis m in 
            let s  = ref [] in  
            Array.iteri (fun i x -> if x <> 0. then s := i :: !s) (List.hd kb);
            ManySol !s
    else
        (* FIXME: say incompatible types *)
        let i = List.hd (find_non_valid_elims rm rb) in 
        let j = range ligs |> List.map (fun x -> x - 1)
                           |> List.filter (fun j -> j < cols && m.(j).(j) <> 0.)
                           |> List.fold_left max 0
        in
        print_matrix rm;
        print_line   rb;
        print_newline (); print_int i; print_string " ... "; print_int j;
        NoSol;; 


let tests = [
    ("pivot identity", fun () ->   
        let m = [| [| 1.; 0. |] ; [| 0. ; 1. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (true = gauss_elimination m b);
        assert (m =  [| [| 1.; 0. |] ; [| 0. ; 1. |] |]));
    ("pivot simple", fun () ->   
        let m = [| [| 2.; 6. |] ; [| 4. ; 5. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (true = gauss_elimination m b); 
        assert (m =  [| [| 2.; 6. |] ; [| 0. ; -7. |] |]));
    ("pivot twist", fun () ->   
        let m = [| [| 4. ; 5. |]; [| 2.; 6. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (true = gauss_elimination m b);
        assert (m =  [| [| 2.; 6. |] ; [| 0. ; -7. |] |]));
    ("pivot special", fun () ->   
        let m = [| [| -1. ; 0. ; 0. |]; [| 0.; 0.;0. |]; [| 0.; 1. ; 0. |]; [| 0. ; 0. ; -1. |] |] in
        let b = [| 0. ; 0. ; 0. ; 0. |] in 
        assert (true = gauss_elimination m b);
        assert (m =  [| [| -1. ; 0. ; 0. |];  [| 0.; 1. ; 0. |]; [| 0. ; 0. ; -1. |]; [| 0.; 0.;0. |] |]));
    ("pivot example", fun () ->   
        let m =[|[| -1.;  0. ; 0. ; 1. ; 0. ; 0. |];
                 [|  0.;  0. ; 1. ; 0. ; -1.;  0.|]; 
                 [|  0.;  1. ; 0. ; 0. ; 0. ; -1.|]; 
                 [|  0.;  0. ; 1. ; -1.;  0.;  0.|]; 
                 [|  1.;  -1.;  0.;  0.;  0.;  0.|] |] in  
        let b = [| 0. ; 0. ; 0. ; 0. ; 0. |] in 
        assert (true = gauss_elimination m b); 
        assert (m = 
        [|  [|-1.;  0.;  0.;  1. ; 0. ; 0. |]; 
            [| 0.;  1.;  0.;  0. ; 0. ; -1.|];
            [| 0.;  0.;  1.;  0. ; -1.;  0.|];
            [| 0.;  0.;  0.;  -1.;  1.;  0.|];
            [| 0.;  0.;  0.;  0. ; 1. ; -1.|] |]));
    ("pivot wikipedia", fun () ->   
        let m = 
        [|  [| 1.;  2.;  2.; -3. ; 2. ;  3.|]; 
            [| 2.;  4.;  1.;  0. ; -5.; -6.|];
            [| 4.;  8.;  5.; -6. ; -1.;  0.|];
            [|-1.; -2.; -1.;  1. ;  1.;  1.|]|] in 
        let b = [| 0. ; 0. ; 0. ; 0. ; 0. ; 0. |] in 
        let _ = gauss_elimination m b in 
        assert (m = 
        [|  [| 1.;  2.;  2.; -3. ;  2.;  3.|]; 
            [| 0.;  0.;  1.; -2. ;  3.;  4.|];
            [| 0.;  0.;  0.;  0. ;  0.;  0.|];
            [| 0.;  0.;  0.;  0. ;  0.;  0.|]|]));
    ("pivot second membre", fun () ->   
        let m = [| [| 2.; 6. |] ; [| 4. ; 5. |] |] in
        let b = [| 1. ; 1. |] in 
        assert (true =gauss_elimination m b);
        assert (b =  [| 1. ; -1. |]));
    ("pivot second membre twist", fun () ->   
        let m = [| [| 4. ; 5. |]; [| 2.; 6. |] |] in
        let b = [| 1. ; 1. |] in 
        assert (true = gauss_elimination m b);
        assert (b =  [| 1. ; -1. |]));
    ("is_valid simple true (1)", fun () -> 
        let m = [| [| 4. ; 5. |]; [| 0.; 0. |] |] in
        let b = [| 1. ; 1. |] in 
        assert (is_valid_elim m b = true)); 
    ("is_valid simple true (2)", fun () -> 
        let m = [| [| 4. ; 5. |]; [| 0.; 0. |] |] in
        let b = [| 1. ; 0. |] in 
        assert (is_valid_elim m b = true)); 
    ("is_valid simple true (3)", fun () -> 
        let m = [| [| 4. ; 5. |]; [| 2.; 1. |]; [| 0.; 0. |] |] in
        let b = [| 1. ; 7. ; 0. |] in 
        assert (is_valid_elim m b = true)); 
    ("is_valid simple false (1)", fun () -> 
        let m = [| [| 4. ; 5. |]; [| 2.; 1. |]; [| 0.; 0. |] |] in
        let b = [| 1. ; 7. ; 1. |] in 
        assert (is_valid_elim m b = false)); 
    ("is_valid simple false (2)", fun () -> 
        let m = [| [| 4. ; 5. |]; [| 2.; 1. |]; [| 1.; 0. |] |] in
        let b = [| 1. ; 7. ; 0. |] in 
        assert (is_valid_elim m b = false)); 
];;
