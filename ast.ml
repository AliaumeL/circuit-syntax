(********
 *
 * MODULE pour l'arbre de syntaxe abstraite
 *
 * Aliaume Lopez
 *
 * Crée le : Mar 14 jui 2016 09:48:30 BST
 *
 *) 

(*
 * Un type de circuit, avec en plus une information contenue 
 * dans chaque noeud 
 *)
type 'a circuit = Par    of  'a * 'a 
                | Seq    of  'a * 'a  
                | VarI   of string
                | VarO   of string
                | Const  of string * int * int
                | Id     of int
                | IdPoly    
                | Links  of (string * string) list * 'a;;

(* 
 * Prend un 'a circuit et applique une fonction 
 * à chaque trou 
 *)
let fmap f x = match x with
    | Par (a,b)      -> Par (f a, f b)
    | Seq (a,b)      -> Seq (f a, f b)
    | Links (x,c)    -> Links (x, f c)
    | VarI a         -> VarI a
    | VarO a         -> VarO a
    | Const (a,b,c)  -> Const (a,b,c)
    | Id n           -> Id n 
    | IdPoly         -> IdPoly;;

(* Le type de circuit récursif associé *)
type circ          = Circ  of circ circuit;; 
type 'a typed_circ = TCirc of ('a typed_circ circuit * 'a) 

(* L'extracteur *)
let unfix = function 
    | Circ x -> x;;

(* Le constricteur *)
let fix x = Circ x;;

(* 
 *
 * Fix F ---- unfix ----> F (Fix F) 
 *  |                        |
 *  |                        |
 * fold g                  fmap (fold g)
 *  |                        |
 *  |                        |
 *  \/                      \/
 *  A    <---- g -------- F A
 *
 *)
let rec foldc g c = g (fmap (foldc g) (unfix c));; 

let uid_var       = ref 0;; 
let newvarname () = incr uid_var; "v_" ^ string_of_int !uid_var ;;

(* Les petites fonctions qui aident bien dans la vie *)
let (===) a b    = Circ (Seq (a,b));;
let (|||) a b    = Circ (Par (a,b));;
let vari x       = Circ (VarI x);;
let varo x       = Circ (VarO x);;
let const x y z  = Circ (Const (x,y,z));;
let id x         = Circ (Id x);;
let idpoly       = Circ IdPoly;;
let links l c    = Circ (Links (l,c));;

let twist        =
    let x = newvarname () in 
    let y = newvarname () in 
    let z = newvarname () in 
    let t = newvarname () in 
    links [(x,y);(z,t)] ((varo x) ||| (varo z) ||| (vari t) ||| (vari y));; 

let trace a      = 
    let x = newvarname () in 
    let y = newvarname () in 
    links [(x,y)] (((vari y) ||| idpoly) === ((varo x) ||| idpoly));;

let bindi y c    = 
    let x = newvarname () in 
    links [(x,y)] ((varo x) ||| c);;

let bindo x c    = 
    let y = newvarname () in 
    links [(x,y)] ((vari y) ||| c);;
