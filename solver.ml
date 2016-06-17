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

(*
 * Find a pivot for line for column j
 *
 *)
let pivot_colonne j m =  
    let k = ref j in 
    let p = ref m.(j).(j) in 
    for i = j to Array.length m - 1 do 
        if m.(i).(j) <> 0. && (!p = 0. || abs_float m.(i).(j) < abs_float !p) then 
            begin
                k := i;
                p := m.(i).(j)
            end
    done;
    if m.(!k).(j) <> 0. then
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
(** 
 *
 * Tries to do gaussian elimination
 *
 * Si une colonne de zéros est trouvée,
 * alors on dit où elle est
 *
 *)
let gauss_elimination m b = 
    let j = ref 0 in 
    let c = ref true in 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    let maxi = min ligs cols in 
    while !j < maxi && !c do  
        match pivot_colonne !j m with 
            | Some k -> 
                permute_lignes !j k m;
                permute_lignes !j k b;
                eliminate !j m b;
                incr j;
            | None -> 
                c := false
    done;
    if !c = false then 
        Some !j
    else
        None;;
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

let resolution_type m b =
    let rm = Array.map Array.copy m in     
    let rb = Array.copy b in 
    let ligs = Array.length m in 
    let cols = Array.length m.(0) in 
    if ligs < cols then 
        failwith "Not enough constraints : add type annotations"
    else
        match gauss_elimination rm rb with
            | Some j -> failwith ("Not enough constraints ... : variable x_{" ^ string_of_int j ^ "} is arbitrary") 
            | None   -> 
                if is_valid_elim rm rb then 
                    let solution = remontee_types rm rb in 
                    if Array.exists ((>) 0.) solution then 
                        failwith ("The constraints forces a negative number of inputs/outputs ...")
                    else
                        solution
                else
                    failwith "Constraints are too strong : cannot resolve";;

let tests = [
    ("pivot identity", fun () ->   
        let m = [| [| 1.; 0. |] ; [| 0. ; 1. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (None = gauss_elimination m b);
        assert (m =  [| [| 1.; 0. |] ; [| 0. ; 1. |] |]));
    ("pivot simple", fun () ->   
        let m = [| [| 2.; 6. |] ; [| 4. ; 5. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (None = gauss_elimination m b); 
        assert (m =  [| [| 2.; 6. |] ; [| 0. ; -7. |] |]));
    ("pivot twist", fun () ->   
        let m = [| [| 4. ; 5. |]; [| 2.; 6. |] |] in
        let b = [| 0. ; 0. |] in 
        assert (None = gauss_elimination m b);
        assert (m =  [| [| 2.; 6. |] ; [| 0. ; -7. |] |]));
    ("pivot special", fun () ->   
        let m = [| [| -1. ; 0. ; 0. |]; [| 0.; 0.;0. |]; [| 0.; 1. ; 0. |]; [| 0. ; 0. ; -1. |] |] in
        let b = [| 0. ; 0. ; 0. ; 0. |] in 
        assert (None = gauss_elimination m b);
        assert (m =  [| [| -1. ; 0. ; 0. |];  [| 0.; 1. ; 0. |]; [| 0. ; 0. ; -1. |]; [| 0.; 0.;0. |] |]));
    ("pivot example", fun () ->   
        let m =[|[| -1.;  0. ; 0. ; 1. ; 0. ; 0. |];
                 [|  0.;  0. ; 1. ; 0. ; -1.;  0.|]; 
                 [|  0.;  1. ; 0. ; 0. ; 0. ; -1.|]; 
                 [|  0.;  0. ; 1. ; -1.;  0.;  0.|]; 
                 [|  1.;  -1.;  0.;  0.;  0.;  0.|] |] in  
        let b = [| 0. ; 0. ; 0. ; 0. ; 0. |] in 
        assert (None = gauss_elimination m b); 
        assert (m = 
        [|  [|-1.;  0.;  0.;  1. ; 0. ; 0. |]; 
            [| 0.;  1.;  0.;  0. ; 0. ; -1.|];
            [| 0.;  0.;  1.;  0. ; -1.;  0.|];
            [| 0.;  0.;  0.;  -1.;  1.;  0.|];
            [| 0.;  0.;  0.;  0. ; 1. ; -1.|] |]));
    ("pivot second membre", fun () ->   
        let m = [| [| 2.; 6. |] ; [| 4. ; 5. |] |] in
        let b = [| 1. ; 1. |] in 
        assert (None =gauss_elimination m b);
        assert (b =  [| 1. ; -1. |]));
    ("pivot second membre twist", fun () ->   
        let m = [| [| 4. ; 5. |]; [| 2.; 6. |] |] in
        let b = [| 1. ; 1. |] in 
        assert (None = gauss_elimination m b);
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
