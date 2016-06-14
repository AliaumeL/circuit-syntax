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
type circ = Fix of circ circuit;; 

(* L'extracteur *)
let unfix = function 
    | Fix x -> x;;

(* Le constricteur *)
let fix x = Fix x;;

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
let (===) a b    = Fix (Seq (a,b));;
let (|||) a b    = Fix (Par (a,b));;
let f            = Fix Fork;;
let j            = Fix Join;;
let w            = Fix Forget;;
let b            = Fix Create;;
let vari x       = Fix (VarI x);;
let varo x       = Fix (VarO x);;
let const x y z  = Fix (Const (x,y,z));;
let id x         = Fix (Id x);;
let twist        = Fix Twist;;
let trace a      = Fix (Trace a);;
let bindi x c    = Fix (BindI (x,c));;
let bindo x c    = Fix (BindO (x,c));;
let links l c    = Fix (Links (l,c));;
