(****
 * utils.ml
 *
 * Aliaume Lopez 
 *
 * Set of ocaml utilities that are used 
 * through the whole program. 
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
 *
 * Put surroundings to a string
 * Usefull in long function composition
 * chain
 *
 * WARNING: intended behaviour,
 * if the string is empty, does NOT
 * put any surroundings
 *
 *)
let surround p q s =
    if s = "" then "" else p ^ s ^ q;;

(**
 * Swap lines of a vector 
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

(* option trying combinator *)
let (<|>) a b = match a with
    | Some x -> a
    | None -> b;;

(* Definition alternative 
let array_find f a = 
    let g (x,i) y = (x <|> (if f y then Some i else None), i+1) in
    a |> Array.fold_left g (None,0) |> fst;;
*)

(**
 * Inserting a node in a sorted list 
 * List sorted with the lower numbers at the end 
 *
 * Not tail recursive.
 *
 * @f : the merging function in case of name clash
 * @a : the key
 * @b : the value
 * @l : the assoc list
 *)
let rec insertV ~merge:f ~key:a ~value:b ~func:l = 
    match l with
        | []     -> [(a,b)]
        | (c,d) :: q -> 
                if c < a then 
                    (a,b) :: (c,d) :: q
                else if c = a then 
                    (a, f b d) :: q 
                else
                    (c,d) :: insertV f a b q;;

(** 
 * Get the image of a by 
 * the partial function l
 *
 * returns an option
 *
 * Note: could be done in a more efficient fashion
 * using the ordering
 *)
let imageV ~elem:a ~func:l = 
    l |> List.filter (fun (x,y) -> x = a)
      |> (function []     -> None
                 | x :: q -> Some (snd x));;

(** 
 * Get the fiber of b by
 * the partiel function l
 *
 * returns a list
 *)
let fiberV ~elem:b ~func:l = 
    l |> List.filter (fun (x,y) -> y = b)
      |> List.map fst;;

(**
 * Removes duplicates and sorts the list 
 * at the same time
 *)
let remove_duplicates l = 
    let remdup (x,q) y = match x with
        | None   -> (Some y, y :: q)
        | Some t -> if t = y then (Some t,q) else (Some y, y::q)
    in
    l |> List.sort compare 
      |> List.fold_left remdup (None, [])
      |> snd;;

(*
 * Unsafely get the inside of an option
 *)
let of_option = function 
    | None -> failwith "oups, option none"
    | Some x -> x;;

(*
 * Convert a list of options into 
 * a list without the Nones 
 *)
let list_of_options l = 
    l |> List.filter (fun x -> not (x = None)) 
      |> List.map of_option;;

(*
 * Removes the elements of b 
 * from the list a 
 *
 * Warning: does not preserve order !
 *)
let remove_list a b = 
    let l1 = List.sort compare a in 
    let l2 = List.sort compare b in 
    let rec aux acc rest1 rest2 = match (rest1,rest2) with
        | ([],_) -> List.rev acc 
        | (x,[]) -> List.rev acc @ x 
        | (x :: q, y :: p) when x = y -> aux acc q rest2
        | (x :: q, y :: p) when x > y -> aux acc rest1 p 
        | (x :: q, y :: p) when x < y -> aux (x :: acc) q rest2 
    in
    aux [] l1 l2;;

(**
 * Put the list in the right order 
 *)
let correct_list l = List.sort (fun a b -> compare (fst b) (fst a)) l;; 

(******
 *
 * TESTS UNITAIRES 
 *
 ******)
let tests = [ 
              ("range length"  , fun () -> assert (List.length (range 10)   = 10));
              ("range start"   , fun () -> assert (List.hd     (range 10)   = 1));
              ("range end"     , fun () -> assert (List.nth    (range 10) 9 = 10));
              ("surround empty", fun () -> assert (""    = surround "a" "b" ""));
              ("surround"      , fun () -> assert ("abc" = surround "a" "c" "b"));
              ("array find (1)", fun () -> assert (None  = array_find (fun x -> x) [| false ; false ; false |]));
              ("array find (2)", fun () -> assert (Some (1,true) = array_find (fun x -> x) [| false ; true ; false |]));
    ];; 
