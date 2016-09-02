
open Utils;;
open Ptg;;

(****
 *
 * FIXME
 * 
 * - Use sets instead of lists for better performance in garbage collection
 * - Use non-persistent datastructures like hash because they have better 
 *  performace (and we are not using the persistence anyway)
 *
 *)

(** 
 * pass a constant node through
 * a traced node 
 *)
let propagate_constant ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some (Value v) = id_find n t.labels             in 
        let [traced_node]  = post_nodes ~node:n           t in 
        let [next_node  ]  = post_nodes ~node:traced_node t in 
        if not (List.mem traced_node t.traced) || List.mem next_node t.delays then 
            t
        else
            t |> trace_rem      ~node:traced_node
              |> main_rem       ~node:n
              |> all_disconnect ~node:n
              |> label_set      ~node:traced_node ~label:(Value v)
              |> main_add       ~node:traced_node
    with
        Match_failure _ -> t;;


(** 
 * remove a main identity node 
 *)
let remove_identity ~node:n t = 
    try (* pattern matching failure means no modification *)
        let [pre] = edges_towards  ~node:n t in 
        let [pos] = edges_from     ~node:n t in
        let None  = id_find n t.labels       in 
        if List.mem n t.nodes then 
            t |> edge_remove_node ~first:pre ~using:n ~second:pos
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
        let Some (Gate Fork)  = id_find n t.labels     in 
        let [z]               = pre_nodes  ~node:n t   in 

        let Some (Value v)    = id_find z  t.labels    in 
        let [e1;e2]           = edges_from ~node:n t   in 
        
        (* now do the update *)
        let new_node         = newid ()                in  
        let new_edge         = neweid ()               in

        (* fist introduce the new node *)
        t |> edge_insert_node ~edge:e2 ~using:new_edge ~node:new_node
          (* disconnect from the fork node *)
          |> edge_rem ~edge:e2
          (* remove the value node *)
          |> main_rem ~node:z
          |> all_disconnect ~node:z
          (* set the new labels accordingly *)
          |> main_add  ~node:new_node
          |> label_set ~node:n ~label:(Value v)
          |> label_set ~node:new_node ~label:(Value v)
    with
        Match_failure _ -> t;;


(**
 * Propagating bottoms 
 * through joins and disconnect trough
 * forks
 *
 * NOTE useless because of join reducing gate ... 
 *
 *
 * FIXME There should be a global garbage collecting stuff 
 * so that local reductions are not needed to replace 
 * whole subcircuits with _|_ ...
 *)
let bottom_join ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some (Value Bottom) = id_find n t.labels    in 
        let [j]                 = post_nodes ~node:n  t in 
        let Some (Gate  Join)   = id_find j t.labels    in 
        t |> main_rem  ~node:n
          |> label_rem ~node:j
          |> post_disconnect ~node:n (* remove the edge *)
    with
        Match_failure _ -> t;;


let disconnect_fork ~node:n t = 
    try (* pattern matching failure means no modification *)
        let Some Disconnect     = id_find n t.labels    in 
        let [f]                 = pre_nodes ~node:n   t in 
        let Some (Gate  Fork)   = id_find f t.labels    in 
        t |> main_rem  ~node:n
          |> label_rem ~node:f
          |> pre_disconnect ~node:n
    with
        Match_failure _ -> t;;

(**
 * A small function that gets the gate 
 * out of a node if possible, and otherwise 
 * fails 
 *)
let gate_of_node ~node:n t = 
    match id_find n t.labels with
        | Some (Gate g) -> g
        | _             -> failwith "(gate_of_node) try to get a gate from a non-gate node";;

(** 
 * A small function that tells if a node 
 * is a gate 
 *)
let is_gate ~node:n t = 
    match id_find n t.labels with
        | Some (Gate g) -> true
        | _             -> false;;

(**
 * GATE REDUCTION PROCESS
 *
 * The gate reduction is made into several phases 
 *
 * 0) check if the node is a gate
 * 1) get the inputs as regular nodes 
 * 2) get the reduction function corresponding
 *    to the gate
 * 3) calculate the output having given the input nodes
 * 4) rewrite the graph according to the output
 *
 *
 * For this to be easy and non-redundant the gate function 
 * is going to output a `gate_func_outpt` sum type that 
 * tells how to rewrite the circuit
 *)
type gate_func_outpt = 
    | Result of value  (* the result is the value given *)
    | Wire   of int    (* the result is given by the Nth wire *)
    | NoOP;;           (* there should be no rewriting *)


(* The gate function for multiplexer *)
let reduce_mux inputs = 
    try 
        let [a;b;c] = inputs in  (* first get the inputs *)
        (* Then determine the function by matching only the first value *)
        match a with
            | Some (Value Bottom) -> Result Bottom 
            | Some (Value Top)    -> Result Top 
            | Some (Value High)   -> Wire 1 
            | Some (Value Low)    -> Wire 2
            | Some _              -> NoOP
            | None                -> NoOP
    with
        Match_failure _ -> NoOP;;

(* The gate function for the nmos transistor *)
let reduce_nmos inputs = 
    try 
        let [a;b] = inputs in 
        match (a,b) with
            | Some (Value Bottom), _                 -> Result Top
            | Some (Value Low)   , _                 -> Result Bottom
            | Some (Value High), Some (Value High)   -> Result Bottom
            | Some (Value High), Some (Value x)      -> Result x
            | Some (Value Top) , Some (Value Bottom) -> Result Bottom
            | Some (Value Top) , Some (Value _)      -> Result Top 
            | _ -> NoOP
    with
        Match_failure  _ -> NoOP;;


(* The gate function for the pmos transistor *)
let reduce_pmos inputs = 
    try 
        let [a;b] = inputs in 
        match (a,b) with
            | Some (Value Bottom), _                 -> Result Top
            | Some (Value Low)   , _                 -> Result Bottom
            | Some (Value High), Some (Value High)   -> Result Bottom
            | Some (Value High), Some (Value x)      -> Result x
            | Some (Value Top) , Some (Value Bottom) -> Result Bottom
            | Some (Value Top) , Some (Value _)      -> Result Top 
            | _ -> NoOP
    with
        Match_failure  _ -> NoOP;;

(* A small function that gives the lowest common 
 * ancestor for two values of the lattice 
 *)
let combine_values v1 v2 = match (v1,v2) with
    | Low,  Low -> Low
    | High,High -> High
    | Bottom,x  -> x
    | x,Bottom  -> x
    | High, Low -> Top
    | Low, High -> Top
    | Top,x     -> Top
    | x,Top     -> Top;;

(* The previous function extended to whole lists of 
 * values
 *)
let rec combine_values_list w1 w2 = match (w1,w2) with
    | [],_      -> w2
    | _,[]      -> w1
    | a::b,c::d -> (combine_values a c) :: combine_values_list b d;;

(* The gate reduction function for join *)
let reduce_join inputs = 
    try 
        let [a;b] = inputs in 
        match (a,b) with
            (* Fist of all, the short-circuits *)
            | Some (Value Bottom),_                -> Wire 1
            | _, Some (Value Bottom)               -> Wire 0
            (* Then the list values *)
            | Some (Value x), Some (Value y)       -> Result (combine_values x y)
            (* otherwise do nothing *)
            |     _                                -> NoOP
    with
        Match_failure _ -> NoOP;;

(* The function that tells which reduction fonction 
 * a gate should use
 *)
let fun_of_gate = function
    | Mux  -> reduce_mux
    | Nmos -> reduce_nmos
    | Pmos -> reduce_pmos
    | Join -> reduce_join
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
            let pre = edges_towards  ~node:n t in 
            let [o] = edges_from     ~node:n t in 
            let ipt = List.map (fun x -> id_find x t.labels) (pre_nodes ~node:n t) in 


            (*** TODO update this part of the code to use 
             *   node ids 
             *)
            
            match fun_of_gate (gate_of_node ~node:n t) ipt with
                | Wire   i -> 
                        (* connect wire i with the output
                         * through the gate (edge_remove_node)
                         * (bypassing the gate)
                         *)
                        t |> edge_remove_node ~first:(List.nth pre i) ~second:o ~using:n
                        (* completely delete the gate
                         * with safe deletion 
                         * *)
                          |> safe_remove ~node:n
                | Result l ->
                        (* 
                         * insert node between the gate and the output,
                         * putting label l 
                         *)
                        let m = newid  () in 
                        let e = neweid () in 
                        t |> main_add  ~node:m
                          |> label_set ~node:m ~label:(Value l)
                          |> edge_insert_node ~edge:o ~node:m ~using:e
                        (* remove the edge between the inserted node 
                         * and the gate
                         *)
                          |> edge_rem ~edge:o
                        (* 
                         * remove the gate using safe remove 
                         *)
                          |> safe_remove ~node:n
                | NoOP     -> t
                        
        with
            Match_failure _ -> t
    else
        t;;


(***
 * Put a delay into the normal form 
 * (a single node). Apply this function 
 * to each node to put the whole graph into 
 * normal form 
 *
 * Creates new nodes that are NOT delays,
 * so if you want to normalize a whole graph,
 * just iterate over the ORIGINAL nodes of 
 * the graph.
 *)
let normalize_delay ~node:n ptg = 
    try 
        let (Some (Gate Wait)) = id_find n ptg.labels in 
        let [e]                = edges_towards ~node:n ptg in 

        let trace_node         = newid ()  in 
        let trace_edge         = neweid () in 
        
        ptg |> edge_insert_node ~edge:e ~node:trace_node ~using:trace_edge 
            |> trace_add ~node:trace_node
            |> main_rem  ~node:n
            |> delay_add ~node:n
            |> label_set ~node:n ~label:(Gate Wait)
    with
        Match_failure _ -> ptg;;


(**
 * Put a graph into the normal timed form 
 *)
let normal_timed_form ptg = 
    let is_delay n = 
        match id_find n ptg.labels with
            | Some (Gate Wait) -> true
            |  _               -> false
    in
    let delayed_nodes =
        ptg.nodes |> List.filter is_delay
    in
    ptg |> batch ~f:normalize_delay ~nodes:delayed_nodes;;

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
    let new_trace    = newids (List.length post1)      in (* trace dispatch    *)
    let new_inputs   = newids (List.length g1.iports)  in (* input dispatch    *)
    let new_delays   = newids (List.length g1.oports)  in (* delays at the end *)
    let new_outputs  = newids (List.length g1.oports)  in (* outputs merge     *)

    let bottoms_pre  = newids (List.length post1)     in (* bottoms for the trace  *)
    let bottoms_ipts = newids (List.length g1.iports) in (* bottoms for the inputs *)


    (* Determines if a trace node is a delayed one (the next thing is a delay) *)
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
    
    (* Starting by merging the two copies *)
    let new_ptg = ptg_merge g1 g2

        |> batch ~f:main_add   ~nodes:(bottoms_pre @ bottoms_ipts @ new_delays @ new_trace)
        (** adding nodes in reverse order to keep the same order
         * in the end ...
         *)
        |> batch ~f:iport_add    ~nodes:(List.rev new_inputs)
        |> batch ~f:oport_add    ~nodes:(List.rev new_outputs)

        (* Dispatching the trace *)
        |> dispatch_with  ~f:is_delayed
                          ~from1:new_trace 
                          ~from2:bottoms_pre 
                          ~fst:pre1       
                          ~snd:pre2

        (* Dispatching the inputs *)
        |> dispatch_with  ~f:is_delayed        
                          ~from1:new_inputs         
                          ~from2:bottoms_ipts
                          ~fst:g1.iports  
                          ~snd:g2.iports 

        (* Disconnecting the trace output for the second graph *)
        |> batch ~f:(label_set ~label:Disconnect)  ~nodes:post1 
        |> batch ~f:(label_set ~label:Disconnect)  ~nodes:post2 
        |> batch ~f:(label_set ~label:(Gate Wait)) ~nodes:new_delays
        |> batch ~f:(label_set ~label:(Value Bottom)) 
                 ~nodes:(bottoms_pre @ bottoms_ipts) 
        |> batch ~f:label_rem ~nodes:(g1.delays @ g2.delays)

        |> connect        ~from:g1.oports      
                          ~towards:new_delays

        |> mk_join        ~fst:new_delays 
                          ~snd:g2.oports
                          ~towards:new_outputs
    in
    (new_trace, new_delays, new_ptg);;


(**
 * 
 * trace unfolding !
 * for now without delays 
 *
 *)
let unfold_trace g1 = 
    if g1.traced <> [] then 
        (* Construct a new graph with the 
         * right dispatching of inputs - trace nodes,
         * the disconnects in the right places,
         * and gives : 
             * the pre nodes to connect to 
             * the new delays that were created
             * the graph itself
         *)
        let (pre2,new_delays,g2) = rewrite_delays (snd (replicate g1)) in

        let (pre1,post1,g1) = trace_split g1 in 

        let new_inputs   = newids (List.length g1.iports) in 

        ptg_merge g1 g2
             |> batch ~f:(label_set ~label:Disconnect)    ~nodes:g1.oports

             |> mk_fork  ~from:post1       ~fst:pre2      ~snd:pre1 
             |> mk_fork  ~from:new_inputs  ~fst:g1.iports ~snd:g2.iports

             (* remove from main nodes before adding elsewhere ! *) 
             |> batch ~f:main_rem     ~nodes:(pre1 @ g2.oports)
             
             |> batch ~f:trace_add    ~nodes:(List.rev pre1)
             |> batch ~f:iport_add    ~nodes:(List.rev new_inputs)
             |> batch ~f:oport_add    ~nodes:(List.rev g2.oports)

             |> batch ~f:normalize_delay ~nodes:new_delays
             |> batch ~f:delay_add       ~nodes:g1.delays
    else
        g1

    ;;

(**
 * Mark nodes to calculate 
 * the accessible nodes in the DUAL graph
 *)
let rec mark_nodes_dual ~seen ~nexts ptg = 
    match nexts with
        | [] -> seen
        | t :: q ->
                if List.mem t seen then 
                    mark_nodes_dual ~seen:seen ~nexts:q ptg
                else
                    let pre_nodes = pre_nodes ~node:t ptg in 
                    mark_nodes_dual ~seen:(t :: seen)
                                    ~nexts:(pre_nodes @ q)
                                    ptg;;

(**
 * Mark nodes to calculate 
 * the accessible nodes in the graph
 *)
let rec mark_nodes ~seen ~nexts ptg = 
    match nexts with
        | [] -> seen
        | t :: q ->
                if List.mem t seen then 
                    mark_nodes ~seen:seen ~nexts:q ptg
                else
                    let post_nodes = post_nodes ~node:t ptg in 
                    mark_nodes ~seen:(t :: seen)
                               ~nexts:(post_nodes @ q)
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
let garbage_collect_dual t = 
    let reachable         = mark_nodes_dual ~seen:t.iports ~nexts:t.oports t in 
    let filter_func x     = not (List.mem x reachable) in 
    let is_reachable f e  = 
        List.mem (f (edge_get_nodes ~edge:e t)) reachable 
    in 
    let nodes_to_delete     = List.filter filter_func (t.traced @ t.delays @ t.nodes) in 

    print_string "DELETING NODES: ";
    nodes_to_delete |> List.map (string_of_int)
                    |> String.concat ", "
                    |> print_string;
    print_newline ();
    
    let remove_node_safely ~node:n t = 
        print_string ("\tREMOVE NODE : " ^ string_of_int n ^ "\n");

        (* The edges comming from a reachable node *)
        let pre  = t 
                |> edges_towards  ~node:n 
                |> List.filter (is_reachable fst)
        in

        (* The edges going to a reachable node *)
        let post  = t 
                |> edges_from  ~node:n 
                |> List.filter (is_reachable snd)
        in

        let bottoms = newids (List.length post) in 
        let discons = newids (List.length pre ) in 
        
        t |> apply ~f:(fun (e,y) -> edge_insert_node ~edge:e ~node:y ~using:(neweid ())) ~elems:(List.combine post bottoms)
          |> apply ~f:(fun (e,y) -> edge_insert_node ~edge:e ~node:y ~using:(neweid ())) ~elems:(List.combine pre  discons)
          |> batch ~f:(label_set ~label:Disconnect)     ~nodes:discons
          |> batch ~f:(label_set ~label:(Value Bottom)) ~nodes:bottoms
          |> batch ~f:main_add   ~nodes:(discons @ bottoms)
          |> main_rem ~node:n 
          |> trace_rem ~node:n (* possible *)
          |> delay_rem ~node:n (* possible *)
          |> node_edges_rem ~node:n
    in

    t |> batch ~f:remove_node_safely  ~nodes:nodes_to_delete 


(* TODO
 *
 * 1) test on examples 
 * 2) Gate rewriting : finish implementing  
 * 3) Dot output     : very clever 
 *
 *)


let empty_ptg = 
    { iports = []; oports = []; traced = []; delays = []; nodes = []; labels = id_empty; edges = id_empty ; co_edges = id_empty; arrows = id_empty };;


let example_ptg_2 = 
    let [a;b;c;d;e;f;g] = newids 7 in 
    empty_ptg |> batch ~f:main_add  ~nodes:[a;b;c;d;e;f;g]
              |> fork_into ~node:a  ~nodes:[b;c;d;e;f;g];;


let example_ptg_3 = 
    let [o] = newids 1  in
    let bot = newids 10 in 
    empty_ptg |> oport_add ~node:o
              |> batch ~f:main_add ~nodes:bot
              |> batch ~f:(label_set ~label:(Value Bottom)) ~nodes:bot
              |> join_into ~node:o ~nodes:bot;;


let example_ptg_4 = 
    let [i;o;t] = newids 3 in 
    empty_ptg |> iport_add ~node:i
              |> oport_add ~node:o
              |> main_add  ~node:t
              |> edge_add  ~from:i ~towards:t
              |> edge_add  ~from:t ~towards:o;;
