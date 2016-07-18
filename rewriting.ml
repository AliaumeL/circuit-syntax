
open Utils;;
open Ptg;;

(****
 *
 * FIXME utiliser des SET de nodes 
 * quand l'appartenance est souvent 
 * de mise.
 *
 * Par exemple quand on fait le garbage
 * collection
 *)

(** 
 * pass a constant node through
 * a traced node 
 *)
let propagate_constant ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some (Value v) = id_find n t.labels             in 
        let [traced_node]  = post_nodes ~node:n           t in 
        if not (List.mem traced_node t.traced) then 
            t
        else
            t |> trace_rem ~node:traced_node
              |> main_rem  ~node:n
              |> label_set ~node:traced_node ~label:(Value v)
              |> main_add  ~node:traced_node
    with
        Match_failure _ -> t;;


(** 
 * remove a main identity node 
 *)
let remove_identity ~node:n t = 
    try (* pattern matching failure means no modification *)
        let [pre] = pre_nodes  ~node:n t in 
        let [pos] = post_nodes ~node:n t in
        let None  = id_find n t.labels   in 
        if List.mem n t.nodes then 
            t |> edge_remove_node ~from:pre ~using:n ~towards:pos 
              |> main_rem ~node:n
        else
            t
    with
        Match_failure _ -> t;;

(**
 * propagate a fork node 
 *)
let propagate_fork ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some (Gate Fork) = id_find n t.labels     in 
        let [z]              = pre_nodes  ~node:n t   in 
        let Some (Value v)   = id_find z  t.labels    in 
        let [x;y]            = post_nodes ~node:n t   in 
        
        (* now do the update *)
        let new_node         = newid ()               in  

        t |> edge_insert_node ~from:n ~towards:x ~using:new_node 
          |> edge_rem ~from:n ~towards:new_node
          |> main_rem ~node:z
          |> label_set ~node:n ~label:(Value v)
          |> label_set ~node:new_node ~label:(Value v)
    with
        Match_failure _ -> t;;


(**
 * Propagating bottoms 
 * through joins and disconnect trough
 * forks
 *)
let bottom_join ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some (Value Bottom) = id_find n t.labels    in 
        let [j]                 = post_nodes ~node:n  t in 
        let Some (Gate  Join)   = id_find j t.labels    in 
        t |> main_rem  ~node:n
          |> label_rem ~node:j
          |> edge_rem  ~from:j ~towards:n
    with
        Match_failure _ -> t;;


let disconnect_fork ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some Disconnect     = id_find n t.labels    in 
        let [f]                 = pre_nodes ~node:n   t in 
        let Some (Gate  Fork)   = id_find f t.labels    in 
        t |> main_rem  ~node:n
          |> label_rem ~node:f
          |> edge_rem  ~from:f ~towards:n
    with
        Match_failure _ -> t;;


let gate_of_node ~node:n t = 
    match id_find n t.labels with
        | Some (Gate g) -> g
        | _             -> failwith "(gate_of_node) try to get a gate from a non-gate node";;

let is_gate ~node:n t = 
    match id_find n t.labels with
        | Some (Gate g) -> true
        | _             -> false;;

type gate_func_outpt = 
    | Result of value 
    | Wire   of int 
    | NoOP;;

let reduce_mux inputs = 
    try 
        let [a;b;c] = inputs in 
        match Lazy.force a with
            | Some (Value Bottom) -> Result Bottom 
            | Some (Value Top)    -> Result Top 
            | Some (Value High)   -> Wire 1 
            | Some (Value Low)    -> Wire 2
            | None                -> NoOP
    with
        Match_failure _ -> NoOP;;

(**
 *
 * TODO:
     * join
     * nmos
     * pmos
 *)


let fun_of_gate = function
    | Mux -> reduce_mux
    | _   -> (fun _ -> NoOP);;

(** 
 * Gate reduction 
 *
 * Things that are important
 *
 * 1) Short circuit / lazyness
 *  -> use Lazy values as input  
 * 2) Simple code : no boilerplate for gate reductin
 *  -> gate reduction is a first class function
 *
 * A] pattern match 
 * B] Use f : (label option lazy) list -> label | nothing | int (* gate output *) 
 * C] replace the pattern
 *    -> NEVER delete the nodes, just put disconnect in front 
 *       of every node that is not used, the garbage collection 
 *       and the dangling nodes rewriting will do their job 
 *       better than we evel will 
 *
 * LIMITATIONS:
     *
     * Gates have only one output
     * 
 *)
let reduce_gate ~node:n t = 
    if is_gate ~node:n t then
        try 
            let pre = pre_nodes  ~node:n t in 
            let [o] = post_nodes ~node:n t in 
            let ipt = List.map (fun x -> lazy (id_find x t.labels)) pre in 
            
            match fun_of_gate (gate_of_node ~node:n t) ipt with
                | Wire   i -> 
                        (* connect wire i with the output
                         * through the gate (edge_remove_node)
                         * (bypassing the gate)
                         *)
                        t |> edge_remove_node ~from:(List.nth pre i) ~towards:o ~using:n
                        (* completely delete the gate
                         * with safe deletion 
                         * *)
                          |> safe_remove ~node:n 
                | Result l ->
                        (* 
                         * insert node between the gate and the output,
                         * putting label l 
                         *)
                        let m = newid () in 
                        t |> main_add  ~node:n 
                          |> label_set ~node:n ~label:(Value l)
                          |> edge_insert_node ~from:n ~towards:o ~using:m
                        (* remove the edge between the inserted node 
                         * and the gate
                         *)
                          |> edge_rem ~from:n ~towards:m
                        (* 
                         * remove the gate using safe remove 
                         *)
                          |> safe_remove ~node:n
                | NoOP     -> t
        with
            Match_failure _ -> t
    else
        t;;

(* 
 * TODO
 * convince that it does the right thing 
 *
 * ---> explain every step of 
 * the process
 *
 *)
let rewrite_delays g1 =
    let (_, g2) = replicate g1 in 

    let (pre1,post1,g1) = trace_split g1 in 
    let (pre2,post2,g2) = trace_split g2 in 

    (* Creating new special nodes *)
    let new_trace   = newids (List.length post1)      in 
    let new_inputs  = newids (List.length g1.iports)  in 
    let new_delays  = newids (List.length g1.oports)  in 
    let new_outputs = newids (List.length g1.oports)  in 

    let bottoms_pre  = newids (List.length post1)     in 
    let bottoms_ipts = newids (List.length g1.iports) in 


    let is_delayed a _ g = 
        match post_nodes ~node:a g with
            | [x] -> 
                    begin
                        match id_find x g.labels with
                            | Some (Gate Wait) -> true
                            | _                -> false
                    end
            | _         -> false
    in
                    

    ptg_merge g1 g2
        |> batch ~f:(label_set ~label:Disconnect) ~nodes:post2

        (** adding trace nodes in reverse order to keep the same order
         * in the end ...
         *)
        |> batch ~f:trace_add  ~nodes:(List.rev new_trace)
        |> batch ~f:main_add   ~nodes:(bottoms_pre @ bottoms_ipts @ new_delays)

        |> dispatch_with  ~f:is_delayed
                          ~from1:new_trace 
                          ~from2:bottoms_pre 
                          ~fst:pre1       
                          ~snd:pre2

        |> dispatch_with  ~f:is_delayed        
                          ~from1:new_inputs         
                          ~from2:bottoms_ipts
                          ~fst:g1.iports  
                          ~snd:g2.iports 

        |> connect        ~from:new_trace ~towards:post1


        |> batch ~f:(label_set ~label:(Value Bottom)) 
                 ~nodes:(bottoms_pre @ bottoms_ipts) 

        |> batch ~f:label_rem ~nodes:(g1.delays @ g2.delays)

        |> connect        ~from:g1.oports      
                          ~towards:new_delays

        |> mk_join        ~fst:new_delays 
                          ~snd:g2.oports
                          ~towards:new_outputs

        (** adding nodes in reverse order to keep the same order
         * in the end ...
         *)
        |> batch ~f:iport_add    ~nodes:(List.rev new_inputs)
        |> batch ~f:oport_add    ~nodes:(List.rev new_outputs);;

(**
 * 
 * trace unfolding !
 * for now without delays 
 *
 *)
let unfold_trace g1 = 
    let g2 = rewrite_delays g1 in 

    let new_inputs   = newids (List.length g1.iports) in 

    let (pre1,post1,g1) = trace_split g1 in 
    let (pre2,post2,g2) = trace_split g2 in 

    
    ptg_merge g1 g2 
        |> batch ~f:(label_set ~label:Disconnect)  ~nodes:post2
        |> batch ~f:(label_set ~label:Disconnect)  ~nodes:g1.oports
        |> batch ~f:(label_set ~label:(Gate Fork)) ~nodes:post1

        |> mk_fork     ~from:post1       ~fst:pre2 ~snd:pre1 
        |> mk_fork     ~from:new_inputs  ~fst:g1.iports ~snd:g2.iports
        
        |> batch ~f:trace_add    ~nodes:(List.rev pre1)
        |> batch ~f:iport_add    ~nodes:(List.rev new_inputs)
        |> batch ~f:oport_add    ~nodes:(List.rev g2.oports);;

(**
 * Mark nodes
 *)
let rec mark_nodes ~seen ~nexts ptg = 
    match nexts with
        | [] -> seen
        | t :: q ->
                if List.mem t seen then 
                    mark_nodes ~seen:seen ~nexts:q ptg
                else
                    let pre_nodes = pre_nodes ~node:t ptg in 
                    mark_nodes ~seen:(t :: seen)
                               ~nexts:(pre_nodes @ q)
                               ptg;;

(**
 *
 * The mark and sweep 
 * phase
 *
 * TODO: be better than that ... because gates other than fork and join 
 * can be targeted !!!!! This only works when the gates only have one 
 * output
 *
 * CORRECT BEHAVIOR
 *
 *  for each reachable node, 
 *      for each edge going to a non-reachable node
 *      replace it with Disconnect
 *
 *      for each edge coming from a non-reachable node
 *      replace it with Bottom 
 *
 *   This function should replace « update_forks_and_joins »
 *
 * NOTE the second part is not necessary because of the 
 * way mark and sweeps works ... (but right now correct 
 * code is better than optimized one)
 *
 *)
let mark_and_sweep t = 
    let reachable           = mark_nodes ~seen:[] ~nexts:t.oports t in 
    print_string "\n REACHABLE : ";
    reachable |> List.map string_of_int |> String.concat " ; " |> print_string;
    let filter_func x       = not (List.mem x reachable) in 
    let is_reachable x      = List.mem x reachable in 
    let nodes_to_delete     = List.filter filter_func t.nodes in 
    
    let remove_node_safely ~node:n t = 
        let pre  = t 
                |> pre_nodes  ~node:n 
                |> List.filter is_reachable 
        in

        let post  = t 
                |> post_nodes  ~node:n 
                |> List.filter is_reachable 
        in
        
        let bottoms = newids (List.length post) in 
        let discons = newids (List.length pre ) in 

        
        t |> apply ~f:(fun (x,y) -> edge_insert_node ~from:n ~using:y ~towards:x) ~elems:(List.combine post bottoms)
          |> apply ~f:(fun (x,y) -> edge_insert_node ~from:x ~using:y ~towards:n) ~elems:(List.combine pre  discons)
          |> batch ~f:(label_set ~label:Disconnect)     ~nodes:discons
          |> batch ~f:(label_set ~label:(Value Bottom)) ~nodes:bottoms
          |> main_rem ~node:n 
          |> node_edges_rem ~node:n
    in

    print_string "\n TO DELETE : ";
    nodes_to_delete |> List.map string_of_int |> String.concat " ; " |> print_string;
    print_string "\n";
    t |> batch ~f:remove_node_safely  ~nodes:nodes_to_delete 


(* TODO
 *
 * 1) test on examples 
 * 2) Gate rewriting : finish implementing  
 * 3) Dot output     : very clever 
 *
 *)


let empty_ptg = 
    { iports = []; oports = []; traced = []; delays = []; nodes = []; labels = id_empty; edges = id_empty ; co_edges = id_empty };;


let example_ptg_2 = 
    let [a;b;c;d] = newids 4 in 
    empty_ptg |> batch ~f:main_add  ~nodes:[a;b]
              |> iport_add ~node:c
              |> oport_add ~node:d
              |> label_set ~node:a ~label:(Gate Fork)
              |> label_set ~node:b ~label:Disconnect
              |> connect   ~from:[a;a;c]  ~towards:[b;d;a];;


let () = 
    example_ptg_2 |> string_of_ptg   |> print_string;
    example_ptg_2 |> mark_and_sweep |> string_of_ptg |> print_string;;



(******* DOT OUTPUT ... *******)

open Dot;;
let dot_of_ptg ptg = 
    let init_rank = rank_group "min" (ptg.iports @ ptg.traced @ ptg.delays) in  
    let fin_rank  = rank_group "max" ptg.oports in 

    let main_node nid =  
        let n = List.length (pre_nodes  ~node:nid ptg) in 
        let m = List.length (post_nodes ~node:nid ptg) in 
        match id_find nid ptg.labels with
            | None      -> mkNode nid (emptyMod |> mod_shape "point") 
            | Some (Gate Join) -> 
                    mkNode nid (emptyMod |> mod_shape "point")
            | Some (Gate Fork) -> 
                    mkNode nid (emptyMod |> mod_shape "point")
            | Some l ->
                    mkNode nid (baseMod |> inputsOutputs (string_of_label l) n m)
    in

    (* 
     * DO SOMETHING CLEVER HERE 
     *
     *)
    let edges_from n t =  () in 


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
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "point" |> mod_width 0.1 |> mod_color "red"))
            |> String.concat "\n"
    in

    let delays  = 
        ptg.delays
            |> List.map (fun x -> mkNode x (emptyMod |> mod_shape "point" |> mod_width 0.1 |> mod_color "grey"))
            |> String.concat "\n"
    in

    [ init_rank; fin_rank; main_nodes; inputs; outputs; delays; traced ]
            |> String.concat "\n"
            |> addPrelude;;

