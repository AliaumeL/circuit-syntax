(****
 * Module de fonctions auxiliaires 
 * de traitement souvent utilisées 
 *
 * Aliaume Lopez 
 *
 * Crée le : Mar 14 jui 2016 09:55:34 BST
 *
 *)

(* split a list in two sublists preserving 
 * the order of the elements
 *)
let split n l = 
    let rec aux i acc rest = 
        if i = 0 then
            (List.rev acc, rest)
        else
            begin 
                match rest with
                    | []     -> aux 0 acc []
                    | t :: q -> aux (i-1) (t :: acc) q
            end
    in
    aux n [] l;;


(* range n = [1 ... n] *)
let range n = 
    let rec aux l i = 
        if i = 0 then 
            l 
        else
            aux (i :: l) (i - 1)
    in
    if n <= 0 then 
        []
    else
        aux [] n;;

(*
 * replicate n e = [e ... e] 
 * length (replicate n e) = n
 *)
let replicate n e = 
        range n 
     |> List.map (fun _ -> e);;

let flip f = fun x y -> f y x;;

(*
 * zipWith f a b = [f a1 b1; .... ; f aN bN] 
 *
 * length (zipWith f a b) = min (length a, length b)
 *
 *)
let rec zipWith f l1 l2 = match (l1,l2) with
    | ([],_) -> []
    | (_,[]) -> []
    | (a::c, b::d) -> (f a b) :: (zipWith f c d);;

(* 
 * Place un morceau de texte des deux côtés 
 * d'un texte
 *
 * Utile pour composer avec des fonctions
 *
 * ATTENTION: ne met entre p et q 
 * que si la chaine est non vide !
 * (pratique)
 *)
let surround p q s =
    if s = "" then "" else p ^ s ^ q;;

(**
 * permute les lignes d'un vecteur
 *)
let permute_lignes i j v =   
    let tmp = v.(i) in 
    v.(i) <- v.(j);
    v.(j) <- tmp;;

let array_find f a = 
    let i = ref 0 in 
    let c = ref None in 
    let m = Array.length a - 1 in 
    while !i < m && !c = None do
        (if f a.(!i) then
            c := Some (!i, a.(!i)));
        incr i;
    done;
    !c;;

let (<|>) a b = match a with
    | Some x -> a
    | None -> b;;

(* Definition alternative 
let array_find f a = 
    let g (x,i) y = (x <|> (if f y then Some i else None), i+1) in
    a |> Array.fold_left g (None,0) |> fst;;
*)

(******
 *
 * TESTS UNITAIRES 
 *
 ******)
let tests = [ 
              ("range length"  , fun () -> assert (List.length (range 10)   = 10));
              ("range start"   , fun () -> assert (List.hd     (range 10)   = 1));
              ("range end"     , fun () -> assert (List.nth    (range 10) 9 = 10));
              ("surround empty", fun () -> assert ("" = surround "a" "b" ""));
              ("surround"      , fun () -> assert ("abc" = surround "a" "c" "b"));
              ("array find (1)", fun () -> assert (None  = array_find (fun x -> x) [| false ; false ; false |]));
              ("array find (2)", fun () -> assert (Some (1,true) = array_find (fun x -> x) [| false ; true ; false |]));
    ];; 
