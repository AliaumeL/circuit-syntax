
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
type c_type = int * int;;

(** Le calcul de type le plus simple du monde **)
let calcul_type circuit = 
    let accum_type = function
        | Id x        -> (x,x)
        | Twist       -> (2,2)
        | Join        -> (2,1)
        | Fork        -> (1,2)
        | Forget      -> (1,0)
        | Create      -> (0,1)
        | Const x     -> (1,1)
        | VarI  y     -> (0,1)
        | VarO  y     -> (1,0)
        | Par (a,b)   -> (fst a + fst b, snd a + snd b)
        | Seq (a,b)   -> if snd a = fst b then (fst a, snd b) else failwith "Type mismatch composition"
        | Trace a     -> if fst a < 1 || snd a < 1 then failwith "Type mismatch" else (fst a - 1, snd a - 1)
        | BindI (_,c) -> (fst c + 1, snd c)
        | BindO (_,c) -> (fst c, snd c + 1)
        | Links (_,c) -> c
    in
    foldc accum_type circuit;; 
