(**
 *
 * circuits.ml
 *
 * Aliaume Lopez
 *
 *)

open Dags;;
open Ptg ;;

(******* DOT OUTPUT ... *******)

(*
 * Gives the first index at which the element 
 * x is found inside the list l
 *
 * Throws error if x is not inside l 
 *)
let rec list_index x = function
    | [] -> failwith "(circuits.ml) [list_index] : error, no such thing"
    | t :: q when t = x -> 0
    | t :: q -> 1 + list_index x q;;

open Dot;;

(* conversion from ptg to a dot graph *)
let dot_of_ptg ptg = 

    (* all the input ports, traced nodes and delays have the same init rank *)
    let init_rank = rank_group "min" (ptg.iports @ ptg.traced ) in  

    (* all the output ports have the same maximum rank *) 
    let fin_rank  = rank_group "max" ptg.oports in 

    (* How to draw a main node *)
    let main_node nid =  
        let n = List.length (pre_nodes  ~node:nid ptg) in 
        let m = List.length (post_nodes ~node:nid ptg) in 
        match id_find nid ptg.labels with
            | None       
            | Some (Gate Join) 
            | Some (Gate Fork)
                    ->
                    mkNode nid (emptyMod |> mod_shape "point")
            | Some Disconnect -> 
                    mkNode nid (baseMod |> mod_label (string_of_label Disconnect))
            | Some (Value v) -> 
                    mkNode nid (baseMod |> mod_label (string_of_label (Value v)))
            | Some l ->
                    mkNode nid (baseMod |> inputsOutputs (string_of_label l) n m)
    in

    (* How to get the port number for a node's edge *)
    let node_port_from_edge nid l eid = 
        match id_find nid ptg.labels with
            | None  (* if it has no label or is a special node -> no port ! *)
            | Some (Gate Join)
            | Some (Gate Fork) 
            | Some Disconnect
            | Some (Value _) -> None
            | _              -> 
                    if List.mem nid ptg.delays then
                        None 
                    else
                        Some (1 + list_index eid l)  (* otherwise it is the index of the edge *)
    in

    (* How to drow an edge between two nodes *)
    let draw_edge eid (a,b) = 
        let l1 = edges_from    ~node:a ptg in 
        let l2 = edges_towards ~node:b ptg in 
        let i1 = node_port_from_edge a l1 eid in 
        let i2 = node_port_from_edge b l2 eid in 
        mkLink a i1 b i2
    in

    let edges = 
        ptg.arrows |> id_bindings
                   |> List.map (fun (x,y) -> draw_edge x y)
                   |> String.concat "\n"
    in


    let main_nodes =
        ptg.nodes |> List.map main_node
                  |> String.concat  "\n"
    in

    let inputs =  
        ptg.iports 
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "diamond"))
            |> String.concat "\n"
    in

    let outputs =  
        ptg.oports 
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "diamond"))
            |> String.concat "\n"
    in

    let traced  = 
        ptg.traced
            (*|> List.map (fun x -> mkNode x (emptyMod |> mod_color "red" |> mod_shape "diamond"))*)
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "point" |> mod_width 0.1 |> mod_color "red"))
            |> String.concat "\n"
    in

    let delays  = 
        ptg.delays
            (*|> List.map (fun x -> mkNode x (emptyMod |> mod_color "grey" |> mod_shape "diamond"))*)
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "point" |> mod_width 0.1 |> mod_color "grey"))
            |> String.concat "\n"
    in
    
    (* The actual construction *)
    [ init_rank; fin_rank; main_nodes; inputs; outputs; delays; traced;edges ]
            |> String.concat "\n"
            |> addPrelude;;


(**** DAG CONVERSION *****)

(* turn an implict n-ary fork into an explicit one *)
let real_fork ~node:n ptg = 
    let images = post_nodes ~node:n ptg in 
    ptg |> post_disconnect ~node:n 
        |> fork_into ~node:n ~nodes:images;; 

(* turn an implict n-ary join into an explicit one *)
let real_join ~node:n ptg = 
    let images = pre_nodes ~node:n ptg in 
    ptg |> pre_disconnect ~node:n 
        |> join_into ~node:n ~nodes:images;; 

(* Converting the labels 
 * for constants into PTG labels 
 * *)  
let convert_label = function
    | VarI x  -> Gate (Box x)
    | VarO x  -> Gate (Box x)
    | Const g ->
            begin 
                match g with
                   | "BOT"  -> Value Bottom
                   | "HIGH" -> Value High
                   | "LOW"  -> Value Low
                   | "TOP"  -> Value Top 
                   | "MUX"  -> Gate  Mux 
                   | "NMOS" -> Gate  Nmos
                   | "PMOS" -> Gate  Pmos
                   | "WAIT" -> Gate  Wait
                   | "DISC" -> Disconnect
                   | "FORK" -> Gate Fork
                   | "JOIN" -> Gate Join
                   |   x    -> Gate (Box x)
            end;;

let ptg_of_dag dag = 
    (* FIRST OF ALL TRANSLATE ALL THE NAMES SO THAT
     * THEY DO NOT CONFLICT WITH OTHER PTG NAMES
     *)
    let dag    = mapids (fun x -> x + !counter) dag in 
    counter := 10 + maxid dag;

    (* 
     * then extract the informations
     *)

    let nodes  = dag.nodes  |> List.map (fun (x,y,z) -> x) in 
    let iport  = dag.iports |> List.map (fun (x,y) -> x)   in 
    let oport  = dag.oports |> List.map (fun (x,y) -> x)   in 
    let ibind  = dag.ibinders in
    let obind  = dag.obinders in 

    (* 
     * the « inside nodes » of the DAG !!! Theses are NOT
     * the inside nodes of the whole ptg
     *)
    let inside = Utils.remove_list nodes (ibind @ obind) in

    (* Creating the input nodes and outputs nodes *)
    let ins    = newids (List.length iport) in  
    let outs   = newids (List.length oport) in

    (*** HANDLING THE EDGES ***)
    
    let add_edge_to_list a b = function
        | None   -> Some [(a,b)]
        | Some k -> Some ((a,b) :: k)
    in

    let append_edge (arrows,edges,co_edges) ((i,pi),(o,po)) = 
        let e = neweid () in 
          (arrows   |> id_add e (i,o),
           edges    |> id_update i (add_edge_to_list pi e),
           co_edges |> id_update o (add_edge_to_list po e) 
          )
    in

    let simplify_edges_list l = 
        l |> List.sort compare 
          |> List.map snd
    in

    let arr,edge_tmp,co_edge_tmp = List.fold_left append_edge (id_empty,id_empty,id_empty) dag.edges in  
    
    (* Temporary ptg construction *)
    let tmp_ptg = {
        iports   = ins ;
        nodes    = inside @ obind ;
        traced   = ibind ;
        delays   = [] ;
        oports   = outs ;
        edges    = id_map simplify_edges_list edge_tmp ;
        co_edges = id_map simplify_edges_list co_edge_tmp ;
        arrows   = arr ;
        labels   = dag.labels |> List.fold_left (fun x (y,z) -> id_add y z x) id_empty |> id_map convert_label ;

    } in 

    (* 
     * new we just translate the implicite forks into explicit ones and 
     * connect the ins to inputs, outs to outputs 
     * *)

    tmp_ptg |> batch ~f:real_fork ~nodes:ibind
            |> batch ~f:real_join ~nodes:obind
            |> connect ~from:ins   ~towards:iport
            |> connect ~from:oport ~towards:outs;;
          


(**** MAIN ENTRY POINT ****)

let get_dag_of_file file = 
    let ic    = open_in file in 
    let buf   = Buffer.create 80 in  
    Stream.of_channel ic |> Stream.iter (Buffer.add_char buf);
    let input = Buffer.contents buf in
    let lexed =  input |> Lexer.do_lexing in
    print_string "\n\nLEXED : ";
    print_string lexed;
    print_string "\n\n\n";
    let parsed = lexed |> Parser.parse_ast in 
    print_string "\n\nPARSED : ";
    print_string (Ast.print_ast parsed);
    print_string "\n\n\n";
    let compiled = parsed |> Compiler.typecheck_and_compile in 
    compiled;;

let get_ptg_of_file file = 
    file |> get_dag_of_file |> ptg_of_dag ;;


let ptg_to_file fname ptg =
  let fhandle = open_out fname in
  ptg |> dot_of_ptg 
      |> output_string fhandle;
  close_out fhandle;;



(***** APPLICATION OF REWRITING RULES *******)

let fc = ref 0;;

(** 
 * Report a graph.
 *
 * Outputs the graph to the standard output
 * and writes the dot conversion into the 
 * according testXXX file
 *)
let report txt ptg = 
    incr fc;
    let base = Printf.sprintf "test%03d" !fc in 
    (*print_string (txt ^ ": " ^ base ^ "\n");*)
    (*ptg |> string_of_ptg |> print_string ;*)
    ptg_to_file (base ^ ".dot") ptg;
    Sys.command ("dot -Tpdf " ^ base ^ ".dot" ^ " -o " ^ base ^ ".pdf");;


(* Some utility functions for the local rules application *)
let apply_local_rule rule ptg =  
    List.fold_left (fun t n -> rule ~node:n t) ptg ptg.nodes;;

let apply_local_rules rules ptg = 
    List.fold_left (fun t r -> apply_local_rule r t) ptg rules;;

let rewrite_local rules ptg = 
    let inter = ref ptg in 
    let older = ref (apply_local_rules rules ptg) in 

    while not (!inter == !older) do (* test physical equality in constant time *)
        older := !inter;
        inter := apply_local_rules rules !inter;
    done;
    !inter;;
        
  
(*
 * The set of rules that will be applied 
 * by the local reduction algorithm 
 *)
let rules = [ Rewriting.remove_identity    ; 
              Rewriting.propagate_constant ; 
              Rewriting.propagate_fork     ;
              Rewriting.bottom_join        ;
              Rewriting.disconnect_fork    ;
              Rewriting.reduce_gate        ]

(*
 * Do a single step of reduction 
 *)
let looping_reduction_step x = 
    let x = Rewriting.garbage_collect_dual x in
    report "GARBAGE COLLECT" x;

    let x = rewrite_local rules x in
    report "LOCAL REWRITE" x;

    let x = Rewriting.garbage_collect_dual x in
    report "GARBAGE COLLECT" x;

    try 
        let (v, x1) = Rewriting.first_output x in 
        print_string "OUTPUT DETECTED : ";
        v |> List.map string_of_value |> String.concat ", " |> print_string;
        print_newline ();
        x1
    with
        Rewriting.NoFirstValue -> 
            let x = Rewriting.unfold_trace x in 
            report "TRACE UNFOLDING" x;

            let x = Rewriting.garbage_collect_dual x in
            report "GARBAGE COLLECT" x;

            x;;

    
(***
 *
 * The program's entry point
 *
 *)
let () = 
    print_string "CIRCUITS - \n";
    let file = if Array.length Sys.argv > 1 then 
                    Sys.argv.(1) 
               else
                   "lines.txt"
    in
    let x = ref (get_ptg_of_file file) in 
    report "INIT" !x;
    x := Rewriting.normal_timed_form !x;
    report "INIT" !x;

    let n = 10 in 

    for i = 1 to n do 
        x := looping_reduction_step !x 
    done;
    x := Rewriting.garbage_collect_dual !x;
    report "GARBAGE COLLECT" !x;
    ();;


