(****
 * Module de fonctions auxiliaires 
 * de traitement souvent utilisées 
 *
 * Aliaume Lopez 
 *
 * Crée le : Mar 14 jui 2016 09:55:34 BST
 *
 *)


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
              ("surround"      , fun () -> assert ("abc" = surround "a" "b" "c"))
    ];; 
