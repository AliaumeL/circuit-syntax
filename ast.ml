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
                | Fork     
                | Join     
                | Forget  
                | Create   
                | VarI   of string
                | VarO   of string
                | Const  of string * int * int 
                | Trace  of 'a  
                | Id     of  int
                | Twist  
                | BindI  of string * 'a
                | BindO  of string * 'a
                | Links  of (string * string) list * 'a;;

(* 
 * Prend un 'a circuit et applique une fonction 
 * à chaque trou 
 *)
let fmap f x = match x with
    | Par (a,b)      -> Par (f a, f b)
    | Seq (a,b)      -> Seq (f a, f b)
    | Trace a        -> Trace (f a)
    | BindI (x,c)    -> BindI (x, f c)
    | BindO (x,c)    -> BindO (x, f c)
    | Links (x,c)    -> Links (x, f c)
    | Fork           -> Fork
    | Join           -> Join
    | Forget         -> Forget
    | Create         -> Create 
    | VarI a         -> VarI a
    | VarO a         -> VarO a
    | Const (a,b,c)  -> Const (a,b,c)
    | Id n           -> Id n 
    | Twist          -> Twist;;

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

(* Les petites fonctions qui aident bien dans la vie *)
let (===) a b    = Circ (Seq (a,b));;
let (|||) a b    = Circ (Par (a,b));;
let f            = Circ Fork;;
let j            = Circ Join;;
let w            = Circ Forget;;
let b            = Circ Create;;
let vari x       = Circ (VarI x);;
let varo x       = Circ (VarO x);;
let const x y z  = Circ (Const (x,y,z));;
let id x         = Circ (Id x);;
let twist        = Circ Twist;;
let trace a      = Circ (Trace a);;
let bindi x c    = Circ (BindI (x,c));;
let bindo x c    = Circ (BindO (x,c));;
let links l c    = Circ (Links (l,c));;
