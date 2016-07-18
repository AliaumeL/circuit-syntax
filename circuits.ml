(**
 *
 * circuits.ml
 *
 * Aliaume Lopez
 *
 * TODO
 *
 * 1) rules
 *  a) Dangle : propagating disconnect nodes
 *  b) Garbage collect nodes
 *  c) fork constant 
 *  d) gate reduce (end the work)
 *
 * 3) compiling from dags 
 *
 * 4) waveforms ?
 *
 *)

open Dot;;

let rec zip_with_3 f a b c = match (a,b,c) with
    | [],[],[]             -> []
    | a1::a2,b1::b2,c1::c2 -> 
            (f a1 b1 c1) :: zip_with_3 f a2 b2 c2;;

let rec zip_with_4 f a b c d = match (a,b,c,d) with
    | [],[],[],[]                 -> []
    | a1::a2,b1::b2,c1::c2,d1::d2 -> 
            (f a1 b1 c1 d1) :: zip_with_4 f a2 b2 c2 d2;;

let rec zip_with f a b = match (a,b) with
    | [],[]         -> []
    | a1::a2,b1::b2 -> 
            (f a1 b1) :: zip_with f a2 b2;;


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


(** TEMPORARY FUNCTIONS **)
let make_arrow x y = 
    (x,None,y,None);;

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


(**
 * Create a copy of the ptg with
 * a disjoint set of nodes 
 * along with the translation function 
 *)
let replicate ptg = 
    let m = !counter in 
    let translate x = x + m + 1 in

    let update_label m (oldid,lbl) =
        id_add (translate oldid) lbl m
    in

    counter := translate m; 
    
    (translate, {
        
        iports = List.map translate ptg.iports;
        oports = List.map translate ptg.oports;

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

let pre_nodes ~node:n t = 
    t.edges |> List.filter (fun e -> is_to ~node:n ~edge:e);;

let post_nodes ~node:n t = 
    t.edges |> List.filter (fun e -> is_from ~node:n ~edge:e);;


let remove_node ~node:n t = 
    let node_rem (_,x,_) = not (x = n) in 
    let simple_rem x = not (x = n) in 
    let edge_rem e =
        (is_from ~node:n ~edge:e) || (is_to ~node:n ~edge:e)
    in

    { 
      edges  = List.filter edge_rem t.edges ;  
      nodes  = List.filter node_rem t.nodes ;
      iports = List.filter simple_rem t.iports ;
      oports = List.filter simple_rem t.oports ;
      traced = List.filter simple_rem t.traced ;
      delays = List.filter simple_rem t.delays ;

      labels = id_remove n t.labels
    };;

(** 
 *
 * Remove a _main_ node 
 *
 * Create new Disconnect for the pre
 * Create new Bottoms    for the post
 *
 * --> this way the circuit is always 
 * correct : no strange modifications
 *
 * *)
let remove_node_safe ~node:n t = 
    let bottoms = ref [] in
    let discard = ref [] in 
    let new_bottom () = 
        let x = newid () in 
        bottoms := x :: !bottoms;
        x
    in
    let new_discard () = 
        let x = newid () in 
        discard := x :: !discard;
        x
    in

    let edge_mod e =
        if is_from ~node:n ~edge:e then 
            let (_,_,x,i) = e in  
            (new_bottom (), None, x, i) 
        else if is_to ~node:n ~edge:e then 
            let (x,i,_,_) = e in 
            (x, i, new_discard (), None)
        else
            e
    in

    let node_rem (_,x,_) = not (x = n) in
    let simple_rem x = not (x = n) in 

    let add_bottoms l = 
        List.fold_left (fun a b -> id_add b (Value Bottom) a) l !bottoms
    in

    let add_discard l = 
        List.fold_left (fun a b -> id_add b (Gate Disconnect) a) l !discard
    in

    { t with
        edges  = List.map edge_mod t.edges ;
        nodes  = List.map (fun x -> (0,x,0)) !bottoms 
               @ List.map (fun x -> (0,x,0)) !discard
               @ List.filter node_rem t.nodes    ;
        traced = List.filter simple_rem t.traced ;
        delays = List.filter simple_rem t.delays ; 
        oports = List.filter simple_rem t.oports ; 
        iports = List.filter simple_rem t.iports ;
        labels = t.labels |> id_remove n |> add_bottoms |> add_discard 
    };;


let relabel_node ~node:n ~label:l t = 
    {
        t with
        labels = t.labels 
            |> id_remove n
            |> id_add n l 
    };;

let relabel_l ~nodes:ns ~label:l t = 
    List.fold_left (fun b a -> relabel_node ~node:a ~label:l b) t ns;;

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

let add_node ~node:e t = 
    { t with
        nodes = (0,e,0) :: t.nodes
    };;

let add_nodes ~nodes:l t = 
    List.fold_left 
        (fun a b -> add_node ~node:b a)
        t
        l;;

(**
 * Try moving a node to main, 
 * does nothing if main already exists 
 *)
let move_to_main ~node:n t = 
    let try_find = List.filter (fun (_,x,_) -> x = n) t.nodes in 
    let simple_rem x = not (x = n) in 
    if try_find = [] then
        { t with
            nodes = (0,n,0) :: t.nodes;
            traced = List.filter simple_rem t.traced ;
            delays = List.filter simple_rem t.delays ; 
            oports = List.filter simple_rem t.oports ; 
            iports = List.filter simple_rem t.iports ;
        }
    else
        t;;

let flatten_ptg g = 
    let others = g.iports @ g.oports @ g.traced @ g.delays in 
    List.fold_left (fun a b -> move_to_main ~node:b a) 
                   g others;;

let merger_v k x y = 
    match x with
        | Some v -> Some v
        | None   -> y;;

(** 
 * The two graphs have 
 * distinct node names 
 *)
let ptg_merge g1 g2 = 
    { 
      nodes = (flatten_ptg g1).nodes @ (flatten_ptg g2).nodes ;
      delays = [];
      traced = [];
      iports = [];
      oports = [];

      labels = id_merge merger_v g1.labels g2.labels;

      edges  = g1.edges @ g2.edges;
    };;


(** Dispatch nodes *)
let dispatch_with ~f ~from1 ~from2 ~fst ~snd g = 
    let make_edge a b c d = 
        if f c d g then 
            [ 
              (a,None,c,None);
              (b,None,d,None)
            ]
        else
            [
              (b,None,c,None);
              (a,None,d,None)
            ]
    in
    { g with
      edges = List.concat (zip_with_4 make_edge from1 from2 fst snd) @ g.edges
    };;

let set_inputs ~nodes:l ptg = 
    {
        ptg with
        iports = l
    };;

let set_outputs ~nodes:l ptg = 
    {
        ptg with
        oports = l
    };;

let set_delays ~nodes:l ptg = 
    {
        ptg with
        delays = l
    };;

let set_trace ~nodes:l ptg = 
    {
        ptg with
        traced = l
    };;
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

let delete_label_l ~nodes:n t = 
    List.fold_left 
        (fun a b -> delete_label ~node:b a) 
        t
        n;;

let connect ~from:i ~towards:j ptg =
    { ptg with
      edges = zip_with make_arrow i j @ ptg.edges
    };;

let mk_join ~towards ~fst ~snd ptg = 
    let new_joins = newids (List.length towards) in 
    ptg |> add_nodes ~nodes:new_joins
        |> connect   ~from:fst        ~towards:new_joins
        |> connect   ~from:snd        ~towards:new_joins
        |> connect   ~from:new_joins  ~towards:towards
        |> relabel_l ~nodes:new_joins ~label:(Gate Join);; 

let mk_fork ~from ~fst ~snd ptg = 
    let new_forks = newids (List.length from) in 
    ptg |> add_nodes ~nodes:new_forks
        |> connect ~from:new_forks ~towards:fst
        |> connect ~from:new_forks ~towards:snd
        |> connect ~from:from      ~towards:new_forks
        |> relabel_l ~nodes:new_forks ~label:(Gate Fork);; 
    

let rec fork_into ~node:n ~nodes:l ptg =  
    match l with
        | []  -> ptg
        | [t] -> 
                ptg |> add_edge ~edge:(n,None,t,None)
        | t :: q -> 
                let fork_node = newid () in 
                ptg |> fork_into ~node:fork_node ~nodes:q
                    |> add_node ~node:fork_node 
                    |> relabel_node ~node:fork_node ~label:(Gate Fork)
                    |> add_edge ~edge:(fork_node,None,t,None)
                    |> add_edge ~edge:(n,None,fork_node,None);;

let rec join_into ~node:n ~nodes:l ptg =  
    match l with
        | []  -> ptg
        | [t] -> 
                ptg |> add_edge ~edge:(t,None,n,None)
        | t :: q -> 
                let join_node = newid () in 
                ptg |> join_into ~node:join_node ~nodes:q
                    |> add_node ~node:join_node 
                    |> relabel_node ~node:join_node ~label:(Gate Join)
                    |> add_edge ~edge:(t,None,join_node,None)
                    |> add_edge ~edge:(join_node,None,n,None);;


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
    let traced_to_main_left  x = (0,x,0) in
    let traced_to_main_right x = (0,x,0) in
    (trids, ptg.traced, {
        ptg with
            traced = [];
            nodes  = List.map traced_to_main_left  ptg.traced 
                   @ List.map traced_to_main_right trids
                   @ ptg.nodes;
            edges  = List.fold_left update_edges ptg.edges corres;  
    });;


(***
 * The original PTG does not have any trace 
 *)
let connect_trace ~from:i ~towards:j ptg = 
    let new_trace = newids (List.length i) in 
    ptg |> connect ~from:i ~towards:new_trace 
        |> connect ~from:new_trace ~towards:j
        |> set_trace ~nodes:new_trace;;

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
 * the node can be a traced one 
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
                begin
                    try
                        let [x,i,_,_] = pre_nodes  ~node:n t in 
                        let [_,_,y,j] = post_nodes ~node:n t in 
                        if is_main_node ~node:x t && is_main_node ~node:y t then
                            t |> remove_node ~node:n
                              |> add_edge ~edge:(x,i,y,j)
                        else
                            t
                    with
                        Match_failure _ -> t
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
                            first |> relabel_node   ~node:p2  ~label:(Gate Disconnect)
                                  |> relabel_node   ~node:p3  ~label:(Gate Disconnect)
                                  |> relabel_node   ~node:mux ~label:(Value Top)
                                  |> signature_node ~node:mux ~ins:0 ~outs:0
                    | Bottom ->  
                            first |> relabel_node   ~node:p2  ~label:(Gate Disconnect)
                                  |> relabel_node   ~node:p3  ~label:(Gate Disconnect)
                                  |> relabel_node   ~node:mux ~label:(Value Bottom)
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


let join_values a b = match a,b with
    | Bottom,_ -> b
    | _,Bottom -> a
    | High,Low -> Top
    | Low,High -> Top
    | Top,_    -> Top
    | _,Top    -> Top
    |    _     -> a;; (* otherwise a = b = join a b *)

(* TODO nmos & pmos table *)
let nmos_values a b = a;;
let pmos_values a b = a;;

let function_of_gate = function
    | Join -> join_values
    | Nmos -> nmos_values
    | Pmos -> pmos_values ;;

let reduce_gate ~node:n ptg = 
    match id_find n ptg.labels with
        | Some (Gate Mux) -> reduce_mux ~node:n ptg
        | Some (Gate g) when List.mem g [Join;Nmos;Pmos] -> 
                begin 
                    let inputs = pre_nodes ~node:n ptg in 
                    let trait_inpt (x,j,_,i) = (j,x,i, id_find x ptg.labels) in  
                    let compare_input (_,_,i,_) (_,_,j,_) = compare i j in 
                    let real_inputs = inputs
                        |> List.map trait_inpt
                        |> List.sort compare_input
                        |> List.map (fun (j,x,_,y) -> (j,x,y))
                    in
                    match real_inputs with 
                        | [(_ ,p1,Some (Value v1));
                           (k2,p2,Some (Value v2)) ] -> 
                               begin
                                   ptg |> remove_node ~node:p1
                                       |> remove_node ~node:p2
                                       |> signature_node ~node:n ~ins:0 ~outs:0   (** set regular node **)
                                       |> relabel_node ~node:n ~label:(Value (function_of_gate g v1 v2))
                               end
                        (* TODO pmos AND nmos short circuit *)
                        (* TODO join short circuit too *)
                        | _ -> ptg

                end
        | _ -> ptg;;


let yank_constant ~node:n ptg = 
    match id_find n ptg.labels with
        | Some (Value v) -> 
                begin
                    match post_nodes ~node:n ptg with
                        | [(_,_,t,_)] -> 
                                if List.mem t ptg.traced then
                                    ptg |> remove_node ~node:n 
                                        |> move_to_main ~node:t 
                                        |> relabel_node  ~node:t ~label:(Value v)
                                else
                                    ptg
                        | _ -> ptg
                end
        | _ -> ptg;;
                    

let propagate_fork ~node:n ptg = 
    match id_find n ptg.labels with
        | Some (Gate Fork) -> 
                begin
                    match (pre_nodes ~node:n ptg, post_nodes ~node:n ptg) with
                        | [(x,_,_,_)], [(_,_,a,_);(_,_,b,_)] -> 
                                begin
                                    match id_find x ptg.labels with
                                        | Some (Value v) -> 
                                                let y = newid () in 

                                                ptg |> remove_node ~node:n 
                                                    |> add_node ~node:y 
                                                    |> relabel_node ~node:y ~label:(Value v)
                                                    |> connect ~from:[x;y] ~towards:[a;b]
                                        | _ -> ptg
                                end
                        | _ -> ptg
                end
        | _ -> ptg;;


let dangling_fork  ~node:n ptg = ();;
let dangling_trace ~node:n ptg = ();;

let dangling_node ~node:n ptg = 
    ptg |> dangling_fork  ~node:n
        |> dangling_trace ~node:n;;


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
                    let pre_nodes = pre_nodes ~node:t ptg
                                 |> List.map (fun (x,_,_,_) -> x)
                    in
                    mark_nodes ~seen:(t :: seen)
                               ~nexts:(pre_nodes @ q)
                               ptg;;

(**
 *
 * The mark and sweep 
 * phase
 *
 *
 * FIXME : wrong wrong and wrong
 *
 *)
let mark_and_sweep t = 
    let reachable           = mark_nodes ~seen:[] ~nexts:t.oports t in 
    let filter_func (_,x,_) = List.mem x reachable in 
    let nodes_to_delete     = List.filter filter_func t.nodes in 
    List.fold_left (fun a b -> remove_node_safe ~node:b a) 
                   t
                   (List.map (fun (_,x,_) -> x) nodes_to_delete);;

(* 
 * TODO
 * convince that it does the right thing 
 *)
let rewrite_delays g1 =
    let (_, g2) = replicate g1 in 

    let (pre1,post1,g1) = split_trace g1 in 
    let (pre2,post2,g2) = split_trace g2 in 

    (* Creating new special nodes *)
    let new_trace   = newids (List.length post1)      in 
    let new_inputs  = newids (List.length g1.iports)  in 
    let new_delays  = newids (List.length g1.oports)  in 
    let new_outputs = newids (List.length g1.oports)  in 

    let bottoms_pre  = newids (List.length post1)     in 
    let bottoms_ipts = newids (List.length g1.iports) in 


    let is_delayed a _ g = 
        match post_nodes ~node:a g with
            | [_,_,x,_] -> 
                    begin
                        match id_find x g.labels with
                            | Some (Gate Wait) -> true
                            | _                -> false
                    end
            | _         -> false
    in
                    

    ptg_merge g1 g2
        |> relabel_l      ~nodes:post2  
                          ~label:(Gate Disconnect)

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

        |> connect_trace  ~from:new_trace
                          ~towards:post1 
        |> add_nodes      ~nodes:(bottoms_pre @ bottoms_ipts)

        |> add_nodes      ~nodes:new_delays

        |> relabel_l      ~nodes:(bottoms_pre @ bottoms_ipts) 
                          ~label:(Value Bottom)

        |> delete_label_l ~nodes:g1.delays
        |> delete_label_l ~nodes:g2.delays

        |> connect        ~from:g1.oports      
                          ~towards:new_delays

        |> mk_join        ~fst:new_delays 
                          ~snd:g2.oports
                          ~towards:new_outputs

        |> set_inputs     ~nodes:new_inputs
        |> set_delays     ~nodes:[]
        |> set_outputs    ~nodes:new_outputs;;

(**
 * 
 * TODO faire plus haut nivea encore !!!
 *
 * parce que là on change le label, mais il faut 
 * aussi changer le type ?!
 *
 *)
let unfold_trace g1 = 
    let (_,g2) = replicate g1 in 

    let new_inputs   = newids (List.length g1.iports) in 

    let (pre1,post1,g1) = split_trace g1 in 
    let (pre2,post2,g2) = split_trace g2 in 

    
    ptg_merge g1 g2 
        |> relabel_l   ~nodes:post2      ~label:(Gate Disconnect)
        |> relabel_l   ~nodes:g1.oports  ~label:(Gate Disconnect)
        |> relabel_l   ~nodes:post1      ~label:(Gate Fork)
        |> mk_fork     ~from:post1       ~fst:pre2 ~snd:pre1 
        |> set_trace   ~nodes:pre1
        |> mk_fork     ~from:new_inputs  ~fst:g1.iports ~snd:g2.iports
        |> set_inputs  ~nodes:new_inputs
        |> set_outputs ~nodes:g2.oports;;

    


let example_ptg = 
    {
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

let empty_ptg = 
    { iports = []; oports = []; traced = []; delays = []; nodes = []; labels = id_empty; edges = [] };;

let example_ptg_2 = 
    counter :=  9;
    empty_ptg |> add_nodes  ~nodes:[2;3;4;5;6]
              |> set_inputs ~nodes:[1]
              |> set_trace  ~nodes:[7]
              |> add_edge   ~edge:(1,None,2,None)
              |> add_edge   ~edge:(2,None,3,None)
              |> add_edge   ~edge:(3,None,4,None)
              |> add_edge   ~edge:(4,None,5,None)
              |> add_edge   ~edge:(5,None,6,None)
              |> add_edge   ~edge:(6,None,7,None)
              |> add_edge   ~edge:(7,None,2,None)
              |> relabel_node ~node:2 ~label:(Gate Join);;

let () = 
    example_ptg_2 
            |> (fun ptg ->  
                    List.fold_left (fun a b -> simplify_identity ~node:b a)
                                   ptg
                                   (List.map (fun (_,x,_) -> x) ptg.nodes))
            |> unfold_trace 
            |> dot_of_ptg |> print_string;; 


