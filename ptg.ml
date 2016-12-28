(***
 *
 *
 * The file containing all 
 * the internal definition
 * of pTG and operations
 * on them.
 *
 * This is different from dags.ml
 * because dags is the graph structure
 * to compile (handles variables, etc ...)
 * but not optimised to do rewriting
 * (one way arrows, lists instead of maps ...)
 *
 *)


(****
 *
 * FIXME order reverse order nodes edges
 *
 * correct behavior ? 
 *
 * TODO IMPORTANT NOTE FIXME
 *
 *)


let rec zip f a b = match (a,b) with
    | [],[] -> []
    | a :: b, c :: d -> f a c :: zip f b d ;;

let rec remove_once p l = match l with
    | [] -> []
    | t :: q when p = t -> q
    | t :: q -> t :: remove_once p q;;

let rec replace_once a b l = match l with
    | [] -> []
    | t :: q when t = a -> b :: q
    | t ::q  -> t :: replace_once a b q;;



(*** THE MAP UTILITIES ***)


module ComparableInts =
struct
  type t = int
  let compare = compare
end;;


module IntegerDictionary = Map.Make (ComparableInts);;

type 'a mapping = 'a IntegerDictionary.t;;

(* node unique id *)
type nid = int;;

(* edge unique id *)
type eid = int;;

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

let id_update k f y =  
    match f (id_find k y) with
        | None -> id_remove k y
        | Some x -> id_add k x y;;

let merger_v k a b = match a, b with
  | None  , None              -> None
  | Some a, None              -> Some a
  | None  , Some b            -> Some b
  | Some a, Some b when a = b -> Some a
  | Some a, Some b            -> failwith "MERGE ERROR CONFLICTING VALUES";; (* Some a ;; (** default value ... Careful !!!*)*)


type gate = 
    | Fork
    | Join
    | Nmos
    | Pmos
    | And
    | Or
    | Box of string
    | Wait 
    | Mux;;

type value = 
    | High
    | Low 
    | Top 
    | Bottom;;

type label = 
    | Disconnect       (* dangling node *)
    | Value   of value 
    (*| Wave    of value list*)
    | Gate    of gate;;


(****
 *
 * The pTG
 *
 * iports : a connection giving the input nodes  in order
 * oports : a connection giving the output nodes in order 
 *
 * traced : a connection giving the traced nodes  with an (arbitrary) order
 * delays : a connection giving the delayed nodes with an (arbitrary) order
 *
 * labels : a mapping from nodes to their labels
 *          if a node is not found in the map,
 *          the label is the empty label
 *
 * edges  : a mapping from a node to a list 
 *          representing it's « sons » IN ORDER 
 *
 * co-edeges : a mapping from nodes to a connection
 *          representing it's « parents » 
 *          IN ORDER
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
    nodes  : nid list; 

    (* only need to store the labels to have the nodes
     * this time the labelling function is total and 
     * we have an CONNECTOR label for connecting nodes
     *)
    labels  : label mapping;
    
    (* edges in right order     *)
    edges      : eid list mapping;

    (* edges for the dual graph *)
    co_edges   : eid list mapping;

    (* arrows : the nodes that corresponds 
     * to edges
     *
     * edge -> (node, node)
     *)
    arrows     : (nid * nid) mapping;
};;

(*** 
 * INVARIANTS 
 *
 * TODO write all the invariants about 
 * the structure 
 *
 *)


(**** PRETTY PRINTING ****)

let string_of_gate = function
    | Fork  -> "F"
    | Join  -> "J"
    | Nmos  -> "N"
    | Pmos  -> "P"
    | Box s -> "B " ^ s 
    | Wait  -> "W"
    | And   -> "AND"
    | Or    -> "OR"
    | Mux   -> "M";;

let rec string_of_value = function
    | High       -> "H"
    | Low        -> "L"
    | Top        -> "T"
    | Bottom     -> "Z";;

let string_of_label = function
    | Disconnect -> "D"
    | Value v    -> string_of_value v
    | Gate  g    -> string_of_gate  g;;

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
        "\tLABELS : ";
        ptg.labels |> id_bindings
                   |> List.map (fun (a,b) -> "node " ^ string_of_int a ^ " = " ^ string_of_label b)
                   |> String.concat ", ";
        "\tEDGES";
        ptg.arrows |> id_bindings
                   |> List.map (fun (e,(x,y)) -> 
                             "\t\t  EDGE :" ^ string_of_int e ^ " = " ^ string_of_int x ^ " -> "
                                      ^ string_of_int y)
                   |> String.concat "\n";
    ] in 
    structure |> String.concat "\n";;


(* 
 * UNIQUE NODE ID PROVIDER 
 * and 
 * UNIQUE EDGE ID PROVIDER
 * *)
let counter  = ref 0;;
let newid () = 
    incr counter; !counter;;

let e_counter  = ref 0;;
let neweid () = 
    incr e_counter; !e_counter;;

let newids n  = Utils.range n |> List.map (fun _ -> newid ());;
let neweids n = Utils.range n |> List.map (fun _ -> neweid ());;


(**** BASE OPERATIONS ****)

(* Node insertion *)
let trace_add ~node:n t = 
    { t with
      traced = n :: t.traced
    };;

let delay_add ~node:n t = 
    { t with
      delays = n :: t.delays 
    };;

let iport_add ~node:n t = 
    { t with
      iports = n :: t.iports 
    };;

let oport_add ~node:n t = 
    { t with
      oports = n :: t.oports 
    };;

let main_add  ~node:n t = 
    { t with
      nodes = n :: t.nodes 
    };;

(**
 * Can fail with Not_found !!!
 * if the edge does not exist
 *)
let edge_get_nodes ~edge:e t = 
    IntegerDictionary.find e t.arrows;;




(* Node neighbourhood exploration *)

let edges_towards ~node:n t = 
    match id_find n t.co_edges with
        | Some l -> l 
        | None   -> [];;

let edges_from    ~node:n t = 
    match id_find n t.edges with
        | Some l -> l 
        | None   -> [];;


let pre_nodes ~node:n t = 
    match id_find n t.co_edges with
        | Some l -> l 
                    |> List.map (fun e -> fst (edge_get_nodes ~edge:e t))
        | None   -> [];;

let post_nodes ~node:n t = 
    match id_find n t.edges with
        | Some l -> l 
                    |> List.map (fun e -> snd (edge_get_nodes ~edge:e t))
        | None   -> [];;


(* Edge construction 
 *
 * Creates a new input and a new 
 * output port to connect the two 
 * nodes (no conflict can arise 
 * with this notation)
 *
 * *)
let edge_add ~from:n ~towards:m t = 
    let eid   = neweid () in 

    let pre_m = edges_towards  ~node:m t in 
    let pos_n = edges_from     ~node:n t in 

    let insert_edge = function
        | None   -> Some [eid] 
        | Some l -> Some (eid :: l)
    in

    { t with
      edges    = id_update n insert_edge t.edges;
      co_edges = id_update m insert_edge t.co_edges;
      arrows   = id_add eid (n,m) t.arrows;
    };;

(*
 * Removes a node from the list of neighbours 
 * WARNING: KEEPS THE ORDER FROM THE LIST, BUT DOES NOT 
 * KEEP THE ASSOCIATED PORT !!! 
 *
 * WARNING 2: removes only _one_ edge between two nodes
 * (it could be that they share several connection 
 * on different ports)
 *)
let edge_rem ~edge:eid t = 
    (* DEBUG don't check for the presence in 
     * production code ...
     *)
    let update_func x = function
        | Some l when List.mem x l -> Some (remove_once x l)
        |   _    -> failwith "edge already removed !!! (edge_rem)"
    in

    let (n,m) = edge_get_nodes ~edge:eid t in 

    { t with
      edges    = id_update n (update_func eid) t.edges;
      co_edges = id_update m (update_func eid) t.co_edges;
      arrows   = id_remove eid t.arrows;
    };;

(* Label updating *)
let label_set ~node:n ~label:l t = 
    { t with
      labels = id_add n l t.labels ;
    };;

let label_rem ~node:n t = 
    { t with
      labels = id_remove n t.labels ;
    };;

let label_update ~node:n ~f t = 
    { t with
      labels = id_update n f t.labels ;
    };;

(* Node deletion 
 *
 * NO SAFETY CHECK !!!!
 *
 * TODO should also delete 
 * the edges going in and 
 * out of the node !!!
 *
 * *)



let trace_rem ~node:n t = 
    { t with
      traced = List.filter ((<>) n) t.traced;
    };;

let delay_rem ~node:n t = 
    { t with
      delays = List.filter ((<>) n) t.delays;
      labels = id_remove n t.labels;
    };;

let iport_rem ~node:n t = 
    { t with
      iports = List.filter ((<>) n) t.iports ;
    };;

let oport_rem ~node:n t = 
    { t with
      oports = List.filter ((<>) n) t.iports ;
    };;

let main_rem  ~node:n t = 
    { t with
      nodes = List.filter ((<>) n) t.nodes ;
      labels = id_remove n t.labels;
    };;

(** some helpful function *)
let batch ~nodes:nds ~f t = 
    List.fold_left (fun a b -> f ~node:b a) t nds;;

let apply ~f ~elems:nds t = (* generic function batch *) 
    List.fold_left (fun a b -> f b a) t nds;;

(** removes all the edges going in and out of a node *)
let node_edges_rem ~node:n t = 
    let pre  = edges_towards ~node:n t in 
    let post = edges_from    ~node:n t in 
    t |> apply ~f:(fun e -> edge_rem ~edge:e) ~elems:post
      |> apply ~f:(fun e -> edge_rem ~edge:e) ~elems:pre;;

(** global pre/post disconnection *)
let pre_disconnect ~node:n t = 
    let p = edges_towards ~node:n t in 
    apply ~f:(fun e -> edge_rem ~edge:e) ~elems:p t;;

let post_disconnect ~node:n t = 
    let p = edges_from ~node:n t in 
    apply ~f:(fun e -> edge_rem ~edge:e) ~elems:p t;;

let all_disconnect ~node:n t = 
    t |> pre_disconnect  ~node:n 
      |> post_disconnect ~node:n;;

(**** HIGHER LEVEL OPERATIONS ON GRAPHS *****)

let connect ~from:l1 ~towards:l2 t = 
    List.fold_left (fun b (x,y) -> edge_add ~from:x ~towards:y b)
                   t
                   (zip (fun x y -> (x,y)) l1 l2);;

(** 
 * Replace all connections to / from this 
 * node by bottoms / disconnect nodes
 * so that removing the node 
 * afterwards does not affect the meaning 
 * of the circuit (no connections to 
 * non-existing nodes)
 *)
let inactive ~node:n t =  
    let pre  = pre_nodes  ~node:n t in 
    let post = post_nodes ~node:n t in

    let dis  = newids (List.length pre)  in
    let bot  = newids (List.length post) in 

    t |> batch ~f:(label_set ~label:Disconnect)     ~nodes:dis 
      |> batch ~f:(label_set ~label:(Value Bottom)) ~nodes:bot
      |> pre_disconnect  ~node:n
      |> post_disconnect ~node:n
      |> connect ~from:pre  ~towards:dis 
      |> connect ~from:bot  ~towards:post
      |> batch ~f:main_add  ~nodes:(dis @ bot) ;;

(**
 * Safe remove for a main node
 *)
let safe_remove ~node:n t = 
    t |> inactive ~node:n
      |> main_rem ~node:n;;


(**
 * Deletes all the specific structure for 
 * a pTG
 *)
let flatten t = 
    { t with
      nodes = t.iports @ t.oports @ t.traced @ t.delays @ t.nodes ;
      iports = [];
      oports = [];
      delays = [];
      traced = [];
    };;

(*** BATCH FORK AND JOIN ***)


let mk_join ~towards ~fst ~snd ptg = 
    let new_joins = newids (List.length towards) in 
    ptg |> batch     ~f:main_add                       ~nodes:new_joins
        |> connect   ~from:fst                         ~towards:new_joins
        |> connect   ~from:snd                         ~towards:new_joins
        |> connect   ~from:new_joins                   ~towards:towards
        |> batch     ~f:(label_set ~label:(Gate Join)) ~nodes:new_joins;;

let mk_fork ~from ~fst ~snd ptg = 
    let new_forks = newids (List.length from) in 
    ptg |> batch     ~f:main_add                       ~nodes:new_forks
        |> connect   ~from:new_forks                   ~towards:fst
        |> connect   ~from:new_forks                   ~towards:snd
        |> connect   ~from:from                        ~towards:new_forks
        |> batch     ~f:(label_set ~label:(Gate Fork)) ~nodes:new_forks;;


(*** DIAGONAL FORK and JOIN ***)
let rec fork_into ~node:n ~nodes:l ptg =  
    match l with
        | []  -> ptg
        | [t] -> 
                ptg |> edge_add ~from:n ~towards:t 
        | [a;b] -> 
                let fork_node = newid () in 
                ptg |> edge_add ~from:fork_node ~towards:a
                    |> edge_add ~from:fork_node ~towards:b
                    |> main_add ~node:fork_node
                    |> label_set ~node:fork_node ~label:(Gate Fork)
                    |> edge_add ~from:n ~towards:fork_node
        | t :: q -> 
                let fork_node = newid () in 
                ptg |> fork_into ~node:fork_node ~nodes:q
                    |> main_add  ~node:fork_node 
                    |> label_set ~node:fork_node ~label:(Gate Fork)
                    |> edge_add  ~from:fork_node ~towards:t
                    |> edge_add  ~from:n         ~towards:fork_node;;

let rec join_into ~node:n ~nodes:l ptg =  
    match l with
        | []  -> ptg
        | [t] -> 
                ptg |> edge_add ~from:t ~towards:n 
        | t :: q -> 
                let join_node = newid () in 
                ptg |> join_into ~node:join_node ~nodes:q
                    |> main_add  ~node:join_node 
                    |> label_set ~node:join_node ~label:(Gate Join)
                    |> edge_add  ~from:t  ~towards:join_node
                    |> edge_add  ~from:join_node ~towards:n;;


(**** SPLITTING THE TRACE *****)
let trace_split ptg = 
    let trids  = newids (List.length ptg.traced) in 
    let corres = List.combine ptg.traced trids   in  

    (*print_string "CORRES : ";*)
    (*corres |> List.map (fun (x,y) -> string_of_int x ^ ":" ^ string_of_int y) *)
           (*|> String.concat "   " *)
           (*|> print_string;*)
    (*print_string "\n";*)

    (* this function seems complex, but in fact
     * traced nodes have only one input and 
     * one output, so this function runs in 
     * constant time !!
     *)
    let copy_pre_conn (x,y) t = 
        let [e]  = edges_towards ~node:x t in 
        (*print_string "\n"; print_int e; print_string "\n";*)
        let n = fst (edge_get_nodes ~edge:e t) in
        t |> edge_rem ~edge:e
          |> edge_add ~from:n ~towards:y
    in

    let new_graph = {
        ptg with
            traced = [];
            nodes  = ptg.traced 
                   @ trids 
                   @ ptg.nodes;
                    }
    in
    let new_graph_2 = 
        new_graph |> apply ~f:copy_pre_conn  ~elems:corres
    in
    (ptg.traced, trids, new_graph_2);;

(**** 
 * edge merging, preserving the ordering of lists
 *
 * a -> b -> c
 *
 * removes b and connects a to c preserving 
 * the output number for a and the input 
 * number for c
 *
 * b is supposed to be a simple connecting
 * node of type 1->1
 *
 *)
let edge_remove_node ~first:e1 ~using:b ~second:e2 t = 

    let (a,_) = edge_get_nodes ~edge:e1 t in 
    let (_,c) = edge_get_nodes ~edge:e2 t in 

    let a_out = edges_from    ~node:a t in 
    let c_in  = edges_towards ~node:c t in 

    let update_func x = function
        | Some l -> Some (remove_once x l)
        | None   -> None
    in

    { t with
      edges    = t.edges    
              |> id_update b (update_func e2) ; 

      co_edges = t.co_edges 
              |> id_add c (replace_once e2 e1 c_in) 
              |> id_update b (update_func e1) ;

      arrows = t.arrows  
            |> id_remove e2
            |> id_add e1 (a,c) ;
    };; 

(***
 * Does exactly the opposite construction 
 *
 * b is supposed to be a simple connecting node
 * without ANY edges prior to this function call
 *
 *)
let edge_insert_node ~edge:e1 ~node:b ~using:e2 t = 

    let (a,c) = edge_get_nodes ~edge:e1 t in 

    let a_out = edges_from    ~node:a t in 
    let c_in  = edges_towards ~node:c t in 

    { t with
      edges = t.edges |> id_add b [e2] ;
      co_edges = t.co_edges |> id_add c (replace_once e1 e2 c_in) 
                            |> id_add b [e1];
      arrows = t.arrows 
            |> id_add e1 (a,b)
            |> id_add e2 (b,c) ;
    };;



(**** dispatching *****)

(** Dispatch nodes *)
let rec dispatch_with ~f ~from1 ~from2 ~fst ~snd g = 
    match from1,from2,fst,snd with
        | [],[],[],[] -> g
        | a1::a2,b1::b2,c1::c2,d1::d2 ->
                if f c1 d1 g then
                    g |> edge_add ~from:a1 ~towards:c1 
                      |> edge_add ~from:b1 ~towards:d1
                      |> dispatch_with ~f ~from1:a2 ~from2:b2 ~fst:c2 ~snd:d2
                else 
                    g |> edge_add ~from:a1 ~towards:d1 
                      |> edge_add ~from:b1 ~towards:c1
                      |> dispatch_with ~f ~from1:a2 ~from2:b2 ~fst:c2 ~snd:d2;;

(**
 * Create a copy of the ptg with
 * a disjoint set of nodes 
 * along with the translation function 
 *)
let replicate ptg = 
    let m = !counter   in 
    let e = !e_counter in 
    let translate x = x + m + 1   in
    let e_translate x = x + e + 1 in 


    let update_label map (oldid,value) =
        id_add (translate oldid) value map
    in

    let update_arrows map (oldid,(n1,n2)) = 
        id_add (e_translate oldid) (translate n1,translate n2) map 
    in

    let update_edges map (oldid,l) = 
        id_add (translate oldid) (List.map e_translate l) map 
    in


    counter   := translate m; 
    e_counter := e_translate e;
    
    (translate, {
        
        iports = List.map translate ptg.iports;
        oports = List.map translate ptg.oports;

        traced = List.map translate ptg.traced;
        delays = List.map translate ptg.delays;

        nodes  = List.map translate ptg.nodes ;
        
        edges  = ptg.edges     
              |> id_bindings
              |> List.fold_left update_edges id_empty;
              
        co_edges  = ptg.co_edges     
              |> id_bindings
              |> List.fold_left update_edges id_empty;

        labels = ptg.labels
              |> id_bindings
              |> List.fold_left update_label id_empty ;

        arrows = ptg.arrows
              |> id_bindings
              |> List.fold_left update_arrows id_empty ;

    });; 


(*** 
 *
 * Merging two graphs
 * with disjoint node names
 *
 * The result is flattened
 *)
let ptg_merge g1 g2 = 
    { 
      nodes = (flatten g1).nodes @ (flatten g2).nodes ;
      delays = [];
      traced = [];
      iports = [];
      oports = [];

      labels   = id_merge merger_v g1.labels g2.labels;

      edges    = id_merge merger_v g1.edges  g2.edges;

      co_edges = id_merge merger_v g1.co_edges g2.co_edges;

      arrows   = id_merge merger_v g1.arrows g2.arrows;
    };;
