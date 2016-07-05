(*
 * compiler.ml
 *
 * Aliaume Lopez
 *
 * The compile step to go from a circ (AST) to a liDAG (graph)
 * using a typed circuit as an intermediate phase
 *
 *)
open Ast;;
open Dags;;
open Typesystem;;

let extract_type tp types = match tp with
    | Const x -> x
    | Var   x -> types.(x);;

let extract_types (a,b) types = (extract_type a types, extract_type b types);;

(***
 *
 * Print the annotated ast into a readable form showing 
 * the type of each expression and sub-expression 
 *
 *)
let print_typed_ast types c = 
    let print_aux circ tp = match circ with 
        | Par (x,y) -> 
                let (n,m) = extract_types tp types in   
                "(" ^ x ^ ") | (" ^ y ^ ") {" ^ string_of_int n ^ "->" ^ string_of_int m  ^ "}"
        | Seq (x,y) -> 
                let (n,m) = extract_types tp types in   
                "(" ^ x ^ ") o (" ^ y ^ ") {" ^ string_of_int n ^ "->" ^ string_of_int m ^ "}" 
        | VarI y    -> ":" ^ y ^ " {0->1}"
        | Const (x,y,z) -> x ^ " {" ^ string_of_int y ^ "->" ^string_of_int z ^ "}"
        | VarO y    -> y ^ ":" ^ " {1->0}"
        | Id n      -> string_of_int n ^ " {" ^ string_of_int n ^ "->" ^ string_of_int n ^ "}"
        | IdPoly    -> 
                let (n,m) = extract_types tp types in   
                "Id {" ^ string_of_int n ^ "->" ^ string_of_int m 
        | Links (l,x) -> 
                let (n,m) = extract_types tp types in   
                l |> List.map (fun (a,b) -> a ^ ":" ^ b)  
                  |> String.concat " "
                  |> (fun y -> "links " ^ y ^ "." ^ x ^ " {" ^ string_of_int n ^ "->" ^ string_of_int m ^ "}") 
    in 
    foldc_typed print_aux c;;


(*** 
 * Construct a DAG out of 
 * an annotated AST
 *)
let dag_of_typed_ast types c =
    let one_step dag_circ tp = match dag_circ with
        | Id x   ->
                Dags.identity x 
        | IdPoly -> 
                let (n,m) = extract_types tp types in   
                if n <> m then failwith "identity with two types : impossible !"
                else Dags.identity n
        | VarI x -> 
                let (_,_) = extract_types tp types in 
                Dags.ivar ~name:x (* TODO add type information *)
        | VarO x -> 
                let (_,_) = extract_types tp types in 
                Dags.ovar ~name:x (* TODO add type information *)
        | Const (a,b,c) ->
                Dags.constant ~name:a ~inputs:b ~outputs:c 
        | Par (a,b) -> 
                Dags.parallel a b
        | Seq (a,b) ->
                Dags.sequence a b
        | Links (l,a) -> 
                Dags.link ~vars:l ~dag:a
    in
    foldc_typed one_step c;; 

(**
 * Compute the types of an ast,
 * then create the annotated ast, 
 * then create the corresponding dag
 *)
let typecheck_and_compile c =  
    let (annot,types) = calcul_type c in  
    print_newline ();
    print_string (print_typed_ast types annot);
    print_newline ();
    dag_of_typed_ast types annot;;
    




(******* TESTS ********)
let test1a = (vari "i2" ||| id 1) === const "G" 2 1 === varo "o2";;
let test1b = (id 1 ||| vari "i1") === const "F" 2 1 === varo "o1";; 
let test1c = (test1a ||| test1b)  === (vari "i3" ||| vari "i4");;


let test2 = 
    let bloc1 = (id 1 ||| vari "i1") === const "F" 2 1 === varo "o1" in 

    let bloc2 = (vari "i2" ||| id 1) === const "G" 2 1 === varo "o2" in 

    let sub = (bloc1 ||| bloc2) === (vari "i3" ||| vari "i4") in 

    let linked_sub = links [("o2","i1"); ("o1","i2"); ("o1", "i3"); ("o2", "i4")] sub in
    linked_sub;; 

let test3 = 
    let bloc i o v = (vari "c" ||| vari "x" ||| vari i) === const "B" 3 1 === const v 1 1 === varo o in 
    let b1     = bloc "i1" "o1" "F" in 
    let b2     = bloc "i2" "o2" "G" in 
    let b3     = links [("o2","i1");("o1","i2")] ((b1 ||| b2) === (vari "i2" ||| vari "i1" ||| vari "c") === const "B" 3 1) in 
    let b4     = bindi "x" (bindi "c" b3) in 
    b4;;


let test4 = 
    trace (const "F" 2 2);;

let test5 = 
    bindi "x" ((id 1 ||| vari "x") === const "F" 2 1);;

let test6 = 
    const "F" 2 2 === twist === const "G" 2 2;; 

let tests = List.mapi (fun i x -> ("test " ^ string_of_int i, fun () -> typecheck_and_compile x; ())) 
    [ test1a; test1b; test1c; test2; test3; test4; test5; test6 ];;
