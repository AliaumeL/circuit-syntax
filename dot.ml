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
    let debut = String.concat "\n" ["digraph G {"; "graph [rankdir=LR];"; "edge [arrowhead=none,arrowtail=none];\n"] in 
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

let mod_shape x     = SM.add "shape" x;;
let mod_color x     = SM.add "color" x;;
let mod_label x     = SM.add "label" ("\"" ^ x ^ "\"");;
let mod_style x     = SM.add "style" (surround "\"" "\"" x);;
let mod_width x     = SM.add "width" (string_of_float x);;
let mod_height x    = SM.add "height" (string_of_float x);;
let mod_fixedsize x = SM.add "fixedsize" ("\"" ^ (string_of_bool x) ^ "\"");;

(* 
 * Making a node id 
 *
 * Note : ports starts from 0 but they are displayed 
 * starting from 1, thus the (i+1) in the code 
 *
 *)
let mkLabel id port io = 
    match port with
        | None   -> "N" ^ string_of_int id 
        | Some i -> "N" ^ string_of_int id ^ ":" ^ io ^ string_of_int i;;

let mkNode nid mods = 
    "N" ^ string_of_int nid ^ " [" ^ renderMods mods ^ "]\n";;

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

(* 
 * Make a link between two nodes 
 *
 * i1 : first node 
 * i2 : second node 
 * x  : optionnal port (maybe int)
 * y  : optionnal port (maybe int)
 *
 *)
let mkLink  = fun i1 x i2 y -> 
    mkLabel i1 x "out" ^ " -> " ^ mkLabel i2 y "in" ^ "\n";;

(**
 *
 * syntax : {rank=min; ... } / {rank=max;  ... } 
 *
 *)
let rank_group rg l = 
    let s = surround ("{rank=" ^ rg ^ ";") "}\n" in
    l |> List.map (fun id -> mkLabel id None "") |> String.concat " " |> s;; 

let addDot  = (^);; 
let addDots = List.fold_left addDot "";;




(***** 
 *
 * TESTS UNITAIRES
 *
 ******)
let tests = [ ];; 
