(******
 *
 * Module pour la sortie en DOT 
 *
 * Aliaume Lopez 
 *
 * Crée le : Mar 14 jui 2016 09:59:21 BST
 *
 *)

open Utils;;

(* 
 * TODO
 *
 * UTILISER UN STRINGBUILDER plutôt que de la concaténation stupide ... 
 *
 *)

module SM = Map.Make (String);;

type dot      = string;;
type node_mod = string SM.t;;
type uid      = int;;


(* 
 * Crée les morceaux nécessaires à mettre au début 
 * et à la fin d'une description de circuit 
 *
 *)
let addPrelude   = 
    let debut = String.concat "\n" ["digraph G {"; "graph [rankdir=LR];"; "edge [arrowhead=none,arrowtail=none];"] in 
    let fin   = "}" in 
    surround debut fin;;



(* Un compteur pour construire des uid de manière unique *)
let count = ref 0;;
let uid () = incr count; !count;; 


(*
 * Permet de construire les arguments à partir 
 * de modificateurs
 *
 *)
let renderMods m = m
    |> SM.bindings
    |> List.map (fun (x,y) -> x ^ "=" ^ y)
    |> String.concat ",";;

let mod_shape x = SM.add "shape" x;;
let mod_label x = SM.add "label" (surround "\"" "\"" x);;
let mod_style x = SM.add "style" (surround "\"" "\"" x);;

(* 
 * Construit un label associé à un noeud, un port
 * et savoir si c'est une entrée / sortie 
 *
 *)
let mkLabel id port io = 
    match port with
        | None   -> "N" ^ string_of_int id 
        | Some i -> "N" ^ string_of_int id ^ ":" ^ io ^ string_of_int i;;

let mkNode mods = 
    let id = uid () in 
    (id, "N" ^ string_of_int id ^ " [" ^ renderMods mods ^ "]\n");;

let emptyMod = SM.empty;;
let baseMod  = SM.singleton "shape" "record";;

(* Génère le label pour un noeud avec des entrées sorties *)
let inputsOutputs label n m =
    let inLabels   = List.map string_of_int (range n) in
    let outLabels  = List.map string_of_int (range m) in 
    let generate   = surround "\"{" "}\"" in 
    let lbls l nom = l 
                  |> List.map (fun id -> surround "<" ">" (nom ^ id) ^ id )
                  |> String.concat "|"
    in
    let labelFinal = surround "{" "}|" (lbls inLabels "in") ^ label ^ surround "|{" "}" (lbls outLabels "out")
                  |> generate 
    in 
    SM.add "label" labelFinal;;

(* Link : fait un lien entre deux sommets *)
let link  = fun i1 x i2 y -> 
    mkLabel i1 x "out" ^ " -> " ^ mkLabel i2 y "in" ^ "\n";;

(* shadowLink : fait un lien avec des pointillés ... *)
let shadowLink i1 x i2 y = 
    mkLabel i1 x "out" ^ " -> " ^ mkLabel i2 y "in" ^ " [style=dotted]\n";;  


(**
 * syntaxe :  {rank=same; q1 q2 ... qn}
 *)
let same_rankdir l = 
    let s = surround "{rank=same;" "}\n" in
    l |> List.map (fun id -> mkLabel id None "") |> String.concat " " |> s;; 

let addDot  = (^);; 
let addDots = List.fold_left addDot "";;

