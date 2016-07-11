(**
 *
 * circuits.ml
 *
 * Dan Ghica
 *
 * Entry point of the program
 * generates dot output,
 * handles graph reduction,
 * embedded DSL, and all.
 *
 * « aliaume hook » is the 
 * hook from all the rest of 
 * the librairies into this 
 * file to interface the new 
 * language and definition to
 * the new model
 *
 *)

open Dot;;

module ComparableInts =
struct
  type t = int
  let compare = compare
end;;

module IntegerDictionary = Map.Make (ComparableInts);;

type 'a mapping = 'a IntegerDictionary.t;;

type nid = int;;

let id_empty     = IntegerDictionary.empty
let id_add       = IntegerDictionary.add
let id_map       = IntegerDictionary.map
let id_mapi      = IntegerDictionary.mapi
let id_filter    = IntegerDictionary.filter
let id_fold      = IntegerDictionary.fold
let id_merge     = IntegerDictionary.merge
let id_bindings  = IntegerDictionary.bindings
let id_remove    = IntegerDictionary.remove
let id_mem       = IntegerDictionary.mem
let id_singleton = IntegerDictionary.singleton
let id_find x y = 
    try 
        Some (IntegerDictionary.find x y)
    with
        Not_found -> None;;


type connection = { 
    nbr : int;         (* the number of ports *)
    con : nid mapping; (* a map from port number to node *)
}

(** FOR NOW WE WILL NOT USE THE CONNECTION TYPE **)

type gate = 
    | Fork
    | Join
    | Nmos
    | Pmos
    | Box of string
    | Wait 
    | Mux
    | Disconnect;;

type value = 
    | High
    | Low 
    | Top 
    | Bottom;;


type label = 
    | Value   of value
    | Gate    of gate;;


(****
 *
 * The pTG
 *
 * iports : a connection giving the input nodes  in order
 * oports : a connection giving the output nodes in order 
 *
 * traced : a connection giving the traced nodes  with an order
 * delays : a connection giving the delayed nodes with an order
 *
 * labels : a mapping from nodes to their labels
 *          ALL nodes are represented
 *
 * edges  : a mapping from a node to a connection 
 *          representing it's « sons » 
 *          with convention that a negative ID 
 *          represents an output
 *
 * co-edeges : a mapping from nodes to a connection
 *          representing it's « parents » 
 *          with convention that a negative ID
 *          represents an input
 *
 *)
type ptg = {
    maxid  : int; (* the maximum id inside the graph *) 

    (* naturally have a notion of order *)
    iports : nid list;
    oports : nid list;

    (* 
     * impose a notion of order to simplify
     * ulterior modification
     *)
    traced : nid list; (* 1 input 1 output *)
    delays : nid list; (* 1 input 1 output *) 

    (* nodes that are not iport or oport or traced or delays *) 
    nodes  : (int * nid * int) list; 

    (* only need to store the labels to have the nodes
     * this time the labelling function is total and 
     * we have an CONNECTOR label for connecting nodes
     *)
    labels  : label mapping;
    
    (* edges in right order *)
    edges   : (nid * int option * nid * int option) list; 

    (* edges in reverse order *)
    (* co_edges : connection mapping; *)
}
    

(*** PRETTY PRINTING ***)
let string_of_gate = function
    | Fork  -> "F"
    | Join  -> "J"
    | Nmos  -> "N"
    | Pmos  -> "P"
    | Box s -> "B " ^ s 
    | Wait  -> "W"
    | Mux   -> "M"
    | Disconnect -> "D";;

let string_of_value = function
    | High       -> "H"
    | Low        -> "L"
    | Top        -> "T"
    | Bottom     -> "Z";;

let string_of_label = function
    | Value v -> string_of_value v
    | Gate  g -> string_of_gate  g;;


(**
 * @t : potential tabulation in front of each line 
 *)
let string_of_connection t c = 
    let nbr = string_of_int c.nbr in 
    let arr = Array.make c.nbr "EMPTY" in
    c.con 
       |> id_bindings 
       |> List.iter (fun (i,y) -> arr.(i) <- string_of_int y);

    arr |> Array.mapi (fun i x -> t ^ string_of_int i ^ " :: " ^ x) 
        |> Array.to_list
        |> String.concat "\n";;

let string_of_ptg ptg = 
    (* TODO add the pretty printing of labels *)
    let structure = [
        "BEGIN";
        "\tINPUTS : ";
        ptg.iports |> List.map (fun x -> string_of_int x) |> String.concat ", ";
        "\tOUTPUTS : ";
        ptg.oports |> List.map (fun x -> string_of_int x) |> String.concat ", ";
        "\tTRACED : ";
        ptg.traced |> List.map (fun x -> string_of_int x) |> String.concat ", ";
        "\tDELAYS : ";
        ptg.delays |> List.map (fun x -> string_of_int x) |> String.concat ", ";
        "\tEDGES";
        ptg.edges |> List.map (fun (x,i,y,j) -> 
                             "\t\t  " ^ string_of_int x ^ " -> "
                                          ^ string_of_int y)
                  |> String.concat "\n";
    ] in 
    structure |> String.concat "\n";;

let pp_ptg ptg = ptg |> string_of_ptg |> print_string;;


(**** DOT CONVERSION ****)

let example_ptg = 
    {
        maxid  = 6;

        iports = [1;2];
        oports = [3];

        traced = [];
        delays = [];
        
        nodes  = [(0,4,0);
                  (0,5,9);
                  (2,6,1)];

        labels = id_empty |> id_add 4 (Gate Fork)
                          |> id_add 5 (Gate Join)
                          |> id_add 6 (Gate (Box "Test"));

        edges  = [ (1,None,4,None);
                   (2,None,5,None);
                   (4,None,6,Some 1);
                   (5,None,6,Some 2);
                   (6,Some 1, 3, None) ]
    };;


let dot_of_ptg ptg = 
    let init_rank = rank_group "min" ptg.iports in  
    let fin_rank  = rank_group "max" ptg.oports in 

    let edges = ptg.edges
             |> List.map (fun (x,y,z,t) -> mkLink x y z t)
             |> String.concat "\n"
    in

    let main_node nid n m =  
        match id_find nid ptg.labels with
            | None      -> mkNode nid (emptyMod |> mod_shape "point") 
            | Some (Gate Join) -> 
                    mkNode nid (emptyMod |> mod_shape "point")
            | Some (Gate Fork) -> 
                    mkNode nid (emptyMod |> mod_shape "point")
            | Some l ->
                    mkNode nid (baseMod |> inputsOutputs (string_of_label l) n m)
    in

    let main_nodes =
        ptg.nodes |> List.map (fun (x,y,z) -> main_node y x z) 
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

    [ init_rank; fin_rank; edges; main_nodes; inputs; outputs; delays; traced ]
            |> String.concat "\n"
            |> addPrelude;;


(*** DOT CONVERSION FINISHED ***)

let counter  = ref 0;;
let newid () = 
    incr counter; !counter;;

let newids n = Utils.range n |> List.map (fun _ -> newid ());;


(** Duplique un ptg **)
let replicate ptg = 
    let translate x = x + ptg.maxid in 

    let update_label m (oldid,lbl) =
        id_add (translate oldid) lbl m
    in

    counter := translate !counter; 
    
    (translate, {
        maxid = translate ptg.maxid;

        iports = List.map translate ptg.iports;
        oports = List.map translate ptg.iports;

        traced = List.map translate ptg.traced;
        delays = List.map translate ptg.delays;

        nodes  = ptg.nodes 
              |> List.map (fun (x,y,z) -> (x, translate y, z));
        
        edges  = ptg.edges |> List.map (fun (x,y,z,t) -> 
                                (translate x, y, translate z, t));
        
        labels = ptg.labels
              |> id_bindings
              |> List.fold_left update_label id_empty ;

    });;

(** Working on edges **)
let is_from ~node:n ~edge:e = 
    match e with
        | (a,_,_,_) -> a = n;;

let is_to ~node:n ~edge:e = 
    match e with
        | (_,_,a,_) -> a = n;;

let is_from_l ~nodes:l ~edge:e = 
    List.exists (fun x -> is_from x e) l;;

let is_to_l ~nodes:l ~edge:e = 
    List.exists (fun x -> is_to x e) l;;

let set_from ~node:n ~edge:(x,y,z,t) = (n,y,z,t);;
let set_to   ~node:n ~edge:(x,y,z,t) = (x,y,n,t);;

(** Split the trace of a pTG 
 *
 **)
let split_trace ptg = 
    let trids  = newids (List.length ptg.traced) in 
    let corres = List.combine ptg.traced trids   in  
    let edge_mod (oldt,newt) e =  
        if is_from ~node:oldt ~edge:e then
            set_from ~node:newt ~edge:e
        else
            e
    in
    let update_edges l p =
        l |> List.map (edge_mod p)
    in
    let traced_to_main x = (1,x,1) in
    (corres, {
        ptg with
            maxid  = !counter;
            traced = [];
            nodes  = List.map traced_to_main (ptg.traced @ trids) @ ptg.nodes;
            edges  = List.fold_left update_edges ptg.edges corres;  
    });;

(** Remove a _main_ node *)
let remove_node ~node:n t = 
    let edge_rem e =   
        not (is_from ~node:n ~edge:e || is_to ~node:n ~edge:e)
    in
    let node_rem (_,x,_) = not (x = n) in
    let simple_rem x = not (x = n) in 
    { t with
        edges  = List.filter edge_rem t.edges ;
        nodes  = List.filter node_rem t.nodes ;
        traced = List.filter simple_rem t.traced ;
        delays = List.filter simple_rem t.delays ; 
        oports = List.filter simple_rem t.oports ; 
        iports = List.filter simple_rem t.iports ;
        labels = t.labels |> id_remove n;
    };;

let pre_nodes ~node:n t = 
    t.edges |> List.filter (fun e -> is_to ~node:n ~edge:e);;

let post_nodes ~node:n t = 
    t.edges |> List.filter (fun e -> is_from ~node:n ~edge:e);;

let relabel_node ~node:n ~label:l t = 
    {
        t with
        labels = t.labels 
            |> id_remove n
            |> id_add n l 
    };;

(** adding an edge
 *
 * Does not include sanity checks
 *
 *)
let add_edge ~edge:e t = 
    {
        t with
        edges = e :: t.edges
    };;

(**
 * Try moving a node to main, 
 * does nothing if main already exists 
 *)
let move_to_main ~node:n t = 
    let try_find = List.filter (fun (_,x,_) -> x = n) t.nodes in 
    let simple_rem x = not (x = n) in 
    if try_find = [] then
        { t with
            nodes = (1,n,1) :: t.nodes;
            traced = List.filter simple_rem t.traced ;
            delays = List.filter simple_rem t.delays ; 
            oports = List.filter simple_rem t.oports ; 
            iports = List.filter simple_rem t.iports ;
        }
    else
        t;;

(**
 * Checks if a node is in the main 
 * graph  (not special set)
 *)
let is_main_node ~node:n t = 
    t.nodes |> List.map (fun (_,x,_) -> x)
            |> List.mem n;;


let delete_label ~node:n t = 
    { t with
      labels = t.labels |> id_remove n 
    };;

(**
 * Change a node's signature 
 * and the edges according to 
 * the new signature 
 *
 * adding the node if not already 
 * inside « nodes » 
 *)
let signature_node ~node:n ~ins:i ~outs:j t = 
    let new_iport = function
        | None -> None
        | Some k when k <= i -> Some k
        | Some k when k >  i -> None
    in
    let new_oport = function
        | None -> None
        | Some k when k <= j -> Some k
        | Some k when k >  j -> None
    in

    let update_edge (x,i,y,j) =  
        if x = n then 
            (x,new_oport i, y,j)
        else if y = n then
            (x,i,y, new_iport j)
        else 
            (x,i,y,j)
    in
    
    {
        t with
        edges = List.map update_edge t.edges;
        nodes = (i,n,j) :: List.filter (fun (_,x,_) -> not (x = n)) t.nodes
    };;
            

(** 
 * pass a constant node through
 * a simple node 
 *)
let propagate_constant ~node:n t = 
    match id_find n t.labels with
        | Some (Value v) ->  
                begin
                    (* get the target node *)
                    let [_,_,newN,_] = post_nodes ~node:n t in 
                    (* get number of inputs of this node *)
                    let others = pre_nodes ~node:newN t in  
                    let is_labeled = match id_find newN t.labels with
                                        | None   -> false
                                        | Some _ -> true
                    in
                    (* 
                     * replace the node if and only if there is 
                     * only us on the node, and it is a non-labeled
                     * node
                     *)
                    match others with
                        | [_] when not is_labeled -> 
                            t |> remove_node  ~node:n
                              |> relabel_node ~node:newN ~label:(Value v)
                              |> move_to_main ~node:newN
                        |  _  -> t
                end
        | Some (Gate g)  -> t
        | None           -> t;;

(**
 * Delete identity node 
 * 
 * ASSUMES: the node given is in « nodes » 
 * (main node)
 *)
let simplify_identity ~node:n t = 
    match id_find n t.labels with
        | None   -> 
                begin (* an unlabeled node is ALWAYS an identity *)
                    let [x,i,_,_] = pre_nodes  ~node:n t in 
                    let [_,_,y,j] = post_nodes ~node:n t in 
                    if is_main_node ~node:x t && is_main_node ~node:y t then
                        t |> remove_node ~node:n
                          |> add_edge ~edge:(x,i,y,j)
                    else
                        t
                end
        | Some _ -> t;;


(**
 *
 * Reduces a multiplexer 
 *
 *)
let reduce_mux ~node:mux t = 
    print_string "REDUCING MUX\n";

    let inputs = pre_nodes ~node:mux t in 
    let trait_inpt (x,j,_,i) = (j,x,i, id_find x t.labels) in  
    let compare_input (_,_,i,_) (_,_,j,_) = compare i j in 
    let real_inputs = inputs
        |> List.map trait_inpt
        |> List.sort compare_input
        |> List.map (fun (j,x,_,y) -> (j,x,y))
    in
    match real_inputs with 
        | [(_ ,p1,Some(Value v1));
           (k2,p2,_);
           (k3,p3,_)] ->
               (* TODO create new nodes, because 
                * otherwise it conflicts with traced and 
                * delays ... 
                *)
               begin 
                   (* Now we can decide the gate's behaviour because
                    * we know the control value 
                    *)
                   let first = t 
                            |> remove_node ~node:p1 
                            |> remove_node ~node:mux
                   in
                   match v1 with
                    | Top ->  
                            first |> relabel_node ~node:p2 ~label:(Gate Disconnect)
                                  |> relabel_node ~node:p3 ~label:(Gate Disconnect)
                                  |> relabel_node ~node:mux ~label:(Value Top)
                                  |> signature_node ~node:mux ~ins:0 ~outs:0
                    | Bottom ->  
                            first |> relabel_node ~node:p2 ~label:(Gate Disconnect)
                                  |> relabel_node ~node:p3 ~label:(Gate Disconnect)
                                  |> relabel_node ~node:mux ~label:(Value Bottom)
                                  |> signature_node ~node:mux ~ins:0 ~outs:0
                    | Low ->  
                            first 
                                  |> relabel_node ~node:p3 ~label:(Gate Disconnect)
                                  |> add_edge ~edge:(p2,k2,mux,None) 
                                  |> signature_node ~node:mux ~ins:0 ~outs:0
                    | High ->  
                            first |> relabel_node ~node:p2 ~label:(Gate Disconnect)
                                  |> add_edge ~edge:(p3,k3,mux,None) 
                                  |> signature_node ~node:mux ~ins:0 ~outs:0
               end
        | _ -> t;;



let reduce_times ptg1 = 
    let ptg2 = replicate ptg1 in 
    let new_inputs = ... in 

    dispatch f new_inputs ptg1.inputs ptg2.inputs

    mk_join new_outputs ptg1.outputs ptg2.outputs

    relabel ptg1.outputs DELAY

    (a,b) = split_trace 

    new_trace 

    dispatch new_trace a1 a2 

    relink new_trace b1
    ignore b2

    

    

(**
 *
 * unfolding ptg 
 *
 *)
let unfold_ptg t1 = 
    let t2 = t1 |> replicate |> reduce_times in 

    let (a,b,c) = split_trace t1 in 
    let (e,f,g) = split_trace t2 in 

    connect b,e
    connect b,a

    set_trace a
    set_forks b

    ignore c
    ignore t1.inputs

    
    retime ;;


(* TODO understand dangling nodes *)
    


