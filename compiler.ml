(******************************* 
 *
 * Lopez Aliaume 
 * Ven 10 jui 2016 14:23:18 BST
 *
 * Compilateur depuis le langage 
 * d'expressions vers dot.
 *
 * Représentation de circuit avec 
 * un superset d'un langage de la
 * théorie des catégories
 *
 *)

open Printf;;
open Typesystem;;

(* TODO
 *
 * 3. Ajouter le typechecking à l'intérieur du calcul [CURRENT]
 *
 * 4. Rendre le code plus joli de manière globale : modules, 
 *    nommage moins implicite, noms de variables, signatures ... la totale 
 *
 * 4bis. Mettre des commentaires sur les fonctions qui demandent un peu 
 *  de réflexion, ainsi que sur les bots de code qui sont des choix au 
 *  niveau du design
 *
 * 5. PASSER À UN MODÈLE DE F-ALGEBRE 
 *  la structure contient les morphismes nécessaires à l'évaluation, et on donne les morphismes qui sont cool 
 *  pour construire les types, etc ... ???? 
 *
 *
 * QUESTION: doit on rester sur une structure de type 
 * « Fix f » ? C'est pas super joli, bien que cela marche ... 
 * On peut imaginer se passer de l'arbre intermédiare à postériori quand on aura assez 
 * d'informations ?
 *
 *)


open Utils;;
open Ast;;

(**** COMPILER VERS DOT *****)

(*** 
 *
 * Faire un dessin d'un circuit, c'est pas si difficile.
 *
 * 1. Quand on compile un bloc, on se souvient des points d'entrées 
 * 2. Quand on compile un bloc, on se souvient des points de sortie
 * 3. On met le tout dans le bon sens et c'est plié 
 *
 ***)

module Variables = Map.Make(String);;
module VarLocs   = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = Dot.uid
  end )


module StringSet = Set.Make(String);;
let remove_duplicates l = 
    l |> StringSet.of_list |> StringSet.elements;;

let mergeVars = Variables.union (fun k x y -> Some (VarLocs.union x y));;

let vars_of_list l =
       l
    |> List.map (fun (x,y) -> Variables.singleton x y)
    |> List.fold_left (Variables.union (fun k x y -> Some x)) Variables.empty;;

(** Le domaine de la sémantique *)
type domain = {
    expr    : Dot.dot;
    inputs  : (Dot.uid * int option) list;
    outputs : (Dot.uid * int option) list;
    vars    : VarLocs.t Variables.t
};;


(** Construit un élément basique du domaine sémantique *)
let dot_basic_uid name n m = 
    let mods   = Dot.inputsOutputs name n m Dot.baseMod in 
    let (id,e) = Dot.mkNode mods in 
    let ins    = range n |> List.map (fun i -> (id, Some i))  in 
    let outs   = range m |> List.map (fun i -> (id, Some i))  in 
    (id, { expr = e; inputs = ins; outputs = outs; vars = Variables.empty });; 

(** Construit un élément basique du domaine sémantique et oublie son identifiant *)
let dot_basic name n m = 
    dot_basic_uid name n m |> snd;;

(** 
 * Construire un élément sans ports de manière générique avec des propriétés 
 * quelconques 
 *)
let dot_generic_uid n m mods = 
    let (id,e) = Dot.mkNode mods in 
    let ins    = range n |> List.map (fun i -> (id, None))  in 
    let outs   = range m |> List.map (fun i -> (id, None))  in 
    (id, { expr = e; inputs = ins; outputs = outs; vars = Variables.empty });; 

(**
 * Construit un élément basique de type « point » (connection)
 * et garde l'identifiant
 *)
let dot_point_uid name n m = 
    let mods   = Dot.baseMod
              |> Dot.mod_label name 
              |> Dot.mod_width 0.02
              |> Dot.mod_height 0.02
              |> Dot.mod_fixedsize false
              |> Dot.mod_shape "point"
    in
    dot_generic_uid n m mods;; 

(**
 * Construit un élément basique de type « point » (connection)
 * et oublie l'identifiant
 *)
let dot_point name n m =
    dot_point_uid name n m |> snd;;

(** 
 * Construit un élément basique de type variable du domaine sémantique  et oublie
 * son identifiant 
 * *)
let dot_variable name n m = 
    let (id,s) = dot_point_uid name n m in 
    let k = Variables.singleton name (VarLocs.singleton id) in  
    { expr = s.expr; inputs = s.inputs; outputs = s.outputs; vars = k };; 

let compile_id n = 
    let pts = range n |> List.map (fun _ -> dot_point "id" 1 1) in 
    { expr = pts |> List.map (fun x -> x.expr) |> Dot.addDots;
      inputs  = pts |> List.map (fun x -> List.hd x.inputs);
      outputs = pts |> List.map (fun x -> List.hd x.outputs);
      vars    = Variables.empty
    };;

(**
 * Récupère dans un élément du domaine 
 * sémantique toutes les occurences d'une 
 * variable 
 *
 * Retourne une liste de Dot.uid 
 *)
let occurences s x = 
    (if Variables.mem x s.vars then 
        Variables.find x s.vars 
    else 
        VarLocs.empty) |> VarLocs.elements;;

let compile_bind_i x s =  
    let occurences_x = occurences s x in 
    let mods   = Dot.baseMod
              |> Dot.mod_label ("BindI " ^ x)  
              |> Dot.mod_style "invisible"
              |> Dot.mod_width 0.02
              |> Dot.mod_height 0.02
              |> Dot.mod_fixedsize false
              |> Dot.mod_shape "point"
    in
    let (bid,binder) = dot_generic_uid 1 0 mods in  
    let liens  = occurences_x
               |> List.map (fun id -> Dot.shadowLink bid None id None) 
    in
    (bid, { expr = Dot.addDots (binder.expr :: s.expr :: liens);
      inputs = (bid, None) :: s.inputs;
      outputs = s.outputs;
      vars   = Variables.remove x s.vars });;

let compile_bind_o x s = 
    let occurences_x = occurences s x in  
    let mods   = Dot.baseMod
              |> Dot.mod_label ("BindO " ^ x)  
              |> Dot.mod_shape "point"
              |> Dot.mod_width 0.02
              |> Dot.mod_height 0.02
              |> Dot.mod_fixedsize false
              |> Dot.mod_style "invisible"
    in
    let (bid,binder) = dot_generic_uid 0 1 mods in  
    let liens  = occurences_x
               |> List.map (fun id -> Dot.shadowLink id None bid None) 
    in
    (bid, { expr = Dot.addDots (s.expr :: binder.expr :: liens);
      inputs = s.inputs;
      outputs = (bid, None) :: s.outputs;
      vars   = Variables.remove x s.vars });;


let compile_link l s =  
    let invars  = l |> List.map fst |> remove_duplicates in 
    let outvars = l |> List.map snd |> remove_duplicates in 

    let (ibinders,ns1) = invars
                      |> List.fold_left (fun (ibids, ns) v -> 
                             let (bid,binder) = compile_bind_i v ns in  
                             ((v,bid) :: ibids, binder)) ([], s)
    in

    let (obinders,ns2) = outvars 
                      |> List.fold_left (fun (obids, ns) v -> 
                             let (bid,binder) = compile_bind_o v ns in  
                             ((v,bid) :: obids, binder))  ([], ns1)
    in

    let ibidMap = ibinders |> vars_of_list in 
    let obidMap = obinders |> vars_of_list in 

    let last_links = l
                  |> List.map (fun (i,o) -> 
                          let ibid = Variables.find i ibidMap in 
                          let obid = Variables.find o obidMap in 
                          Dot.shadowLink obid None ibid None)
    in
    {
        expr = Dot.addDots (ns2.expr :: last_links);
        inputs = s.inputs;
        outputs = s.outputs; 
        vars = List.fold_left (flip Variables.remove) s.vars (invars @ outvars)
    };;


let compile_vers_dot circuit =
    let faire_lien (a,b) (c,d) = Dot.link a b c d in 
    let accum_compile = function
        | Id x          -> compile_id x 
        | Twist         -> dot_basic "Twist"  2 2 
        | Fork          -> dot_point "Fork"   1 2 
        | Join          -> dot_point "Join"   2 1 
        | Create        -> dot_basic "Create" 0 1
        | Forget        -> dot_basic "Forget" 1 0 
        | Const (x,y,z) -> dot_basic x        y z 
        | VarI x        -> dot_variable x     0 1 
        | VarO x        -> dot_variable x     1 0 
        | BindI (x,s)   -> compile_bind_i x s |> snd 
        | BindO (x,s)   -> compile_bind_o x s |> snd 
        | Links (l,s)   -> compile_link l s
        | Trace s -> 
                begin
                    match (s.inputs,s.outputs) with
                        | (_ , []) -> failwith "erreur de typage"
                        | ([], _ ) -> failwith "erreur de typage"
                        | ((id1,p1)::r1, (id2,p2)::r2) -> 
                                { expr    = Dot.addDot s.expr (Dot.link id2 p2 id1 p1); (* la trace fait l'ordre inverse !! *)
                                  inputs  = r1;
                                  outputs = r2;
                                  vars    = s.vars } 
                end
        | Par (s1,s2) -> 
                (* align inputs in parallel composition *)
                let align_in  =  s1.inputs @ s2.inputs |> List.map fst |> Dot.same_rankdir in 
                { expr    = Dot.addDots [s1.expr; s2.expr; align_in];
                  inputs  = s1.inputs  @ s2.inputs;
                  outputs = s1.outputs @ s2.outputs;
                  vars    = mergeVars s1.vars s2.vars } 
           
        | Seq (s1,s2) -> 
                (* ne fait pas de vérification de types ici : peut produire 
                 * du code incorrect si les deux ne peuvent pas de composer
                 *)
                let liens_construire = zipWith faire_lien s1.outputs s2.inputs in   
                {
                    expr = Dot.addDots (s1.expr :: s2.expr :: liens_construire);
                    inputs = s1.inputs;
                    outputs = s2.outputs;
                    vars    = mergeVars s1.vars s2.vars
                }
    in
    let inside = foldc accum_compile circuit in 
    (* Pour chaque input, on ajoute un noeud qui va vers cet input *)
    let liens_input = inside.inputs 
                   |> List.map (fun (id,p) -> 
                           let (sid,ss) = dot_point_uid ("IN" ^ string_of_int id) 0 0 in 
                           Dot.addDots [ ss.expr ; Dot.link sid None id p ])
    in
    (* Idem pour les outputs *)
    let liens_output = inside.outputs 
                    |> List.map (fun (id,p) -> 
                            let (sid,ss) = dot_point_uid ("OUT" ^ string_of_int id) 0 0 in 
                            Dot.addDots [ ss.expr ; Dot.link id p sid None])
    in
    { 
        expr = Dot.addDots (inside.expr :: liens_input @ liens_output);
        inputs = [];
        outputs = []; 
        vars = inside.vars
    };;








(****************************************
 *                                      *
 * NOW THE CODE THE USER WILL SEE/USE   *
 *                                      *
 ****************************************)


let test  = id 1 ||| id 1;; 

let test2 = 
    let bloc1 = (id 1 ||| vari "i1") === const "F" 2 1 === varo "o1" in 

    let bloc2 = (vari "i2" ||| id 1) === const "G" 2 1 === varo "o2" in 

    let sub = (bloc1 ||| bloc2) === (vari "i3" ||| vari "i4") in 

    let linked_sub = links [("i1","o2"); ("i2","o1"); ("i3", "o1"); ("i4", "o2")] sub in
    linked_sub === const "FINAL FUN" 2 1;; 


let test3 = 
    let bloc i o v = (vari "c" ||| vari "x" ||| vari i) === const "B" 3 1 === const v 1 1 === varo o in 
    let b1     = bloc "i1" "o1" "F" in 
    let b2     = bloc "i2" "o2" "G" in 
    let b3     = links [("i1","o2");("i2","o1")] ((b1 ||| b2) === (vari "i2" ||| vari "i1" ||| vari "c") === const "B" 3 1) in 
    let b4     = bindi "x" (bindi "c" b3) in 
    b4;;

let compile file expr =
    let oc = open_out file in 
    (expr |> compile_vers_dot |> (fun x -> x.expr) |> Dot.addPrelude |> fprintf oc "%s");
    close_out oc;;
(*
let () = 
    compile "output.dot" test3;
    let (tp,constraints) = calcul_type test in 
    ();;
*)
