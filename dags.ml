(**
 *
 * Internal representation for directed acyclic graphs
 * representing the graphical semantics of the expressions
 *
 * Aliaume Lopez
 *
 *)

open Utils;;


type nid = int;;

type label = VarI of string | VarO of string | Const of string;;

type port  = nid * int option;;

let map_port f (a,b) = (f a, b)

(** 
 * A liDAG with 
 * a placeholder for information such as 
 * node type information  
 *)
type 'a lidag = {
    (* input nodes, with an optionnal port *)
    iports   : port list;
    (* output nodes, with an optionnal port *)
    oports   : port list;
    (* all the internal nodes, with the number
     * of necessary input and output ports 
     * for the node 
     *
     * 0     => nothing 
     * n > 0 => n ports 
     *)
    nodes    : (nid * int * int) list; 
    edges    : (port * port) list;
    labels   : (nid * label) list;

    (* adding two list to add information
     * about binding nodes,
     * only usefull for the conversion to a
     * PTG
     *)
    ibinders : nid list;
    obinders : nid list;
};;

(**
 *
 * The empty dag
 *
 *)
let empty_dag = { iports = []; oports = []; nodes = []; edges = []; labels = []; ibinders = []; obinders = [] };;

(* 
 * Make an anonymous link between two nodes
 * IE: link that doesn't involve 
 * input ports and output ports for the 
 * nodes
 *
 *)
let anonym_link ~start:a ~finish:b = ((a,None), (b,None));;

(** 
 * Change the node ids in a consistent way 
 *)
let mapids f dag = {
    iports   = dag.iports   |> List.map (map_port f);
    oports   = dag.oports   |> List.map (map_port f);
    nodes    = dag.nodes    |> List.map (fun (x,y,z) -> (f x, y, z)); 
    edges    = dag.edges    |> List.map (fun (x,y) -> (map_port f x, map_port f y));
    labels   = dag.labels   |> List.map (fun (x,y) -> (f x, y));
    ibinders = dag.ibinders |> List.map f;
    obinders = dag.obinders |> List.map f;
};;


(**
 * Retruns the maximal node id 
 * in the graph
 *
 *)
let maxid dag = match dag.nodes with
    | [] -> 0
    | (t,_,_) :: q -> t;;


(**
 *
 * Merge two graphs 
 * into one 
 * by composition
 *)
let sequence ~first:p ~second:q = 
    let mp = maxid p in 
    let mq = maxid q in 
    if mq < mp then 
        let np = mapids (fun x -> x + mq) p in  
        let new_links = List.combine np.oports q.iports in  
        { iports   = np.iports                                          ;
          oports   = q.oports                                           ;
          nodes    = np.nodes @ q.nodes                                 ;
          edges    = remove_duplicates (new_links @ np.edges @ q.edges) ;
          labels   = np.labels @ q.labels                               ;
          ibinders = np.ibinders @ q.ibinders                           ;
          obinders = np.obinders @ q.obinders                           ;
        }
    else
        let nq = mapids (fun x -> x + mp) q in  
        let new_links = List.combine p.oports nq.iports in  
        { iports   = p.iports                                           ;
          oports   = nq.oports                                          ;
          nodes    = nq.nodes @ p.nodes                                 ;
          edges    = remove_duplicates (new_links @ nq.edges @ p.edges) ;
          labels   = nq.labels @ p.labels                               ;
          ibinders = nq.ibinders @ p.ibinders                           ;
          obinders = nq.obinders @ p.obinders                           ;
        };;


(**
 *
 * Compose two graph using 
 * parallel composition 
 *
 *)
let parallel ~top:p ~bottom:q = 
    let mp = maxid p in 
    let mq = maxid q in 
    if mq < mp then
        let np = mapids (fun x -> x + mq)  p in  
        { iports = np.iports @ q.iports                   ;
          oports = np.oports @ q.oports                   ;
          nodes  = np.nodes @ q.nodes                     ;
          edges  = remove_duplicates (np.edges @ q.edges) ;
          labels = np.labels @ q.labels                   ;
          ibinders = np.ibinders @ q.ibinders             ;
          obinders = np.obinders @ q.obinders             ;
        }
    else
        let nq = mapids (fun x -> x + mp) q in  
        { iports = p.iports @ nq.iports                   ;
          oports = p.oports @ nq.oports                   ;
          nodes  = nq.nodes @ p.nodes                     ;
          edges  = remove_duplicates (nq.edges @ p.edges) ;
          labels = nq.labels @ p.labels                   ;
          ibinders = nq.ibinders @ p.ibinders             ;
          obinders = nq.obinders @ p.obinders             ;
        };;

let (>>=) l f = List.concat (List.map f l);;

let link ~vars ~dag:g = 
    let m  = maxid g in 

    (* Input variable _names_ *)
    let vi = vars |> List.map snd |> remove_duplicates in

    (* Output variable _names_ *)
    let vo = vars |> List.map fst |> remove_duplicates in 
    
    (* c : vi -> node_id *)
    let c  = vi |> List.mapi (fun i v -> (v,i + m + 1)) in  
    let ci = List.length c in 
    (* d : vi -> node_id *)
    let d  = vo |> List.mapi (fun i v -> (v,i + m + ci + 1)) in  

    (* FIXME 
     * if fiberV x == [] then 
     * the binding node is just a dangling node ...
     * 
     * Need to be seen !
     *)
    let ei = vi >>= (fun x -> 
                        fiberV (VarI x) g.labels >>= (fun v -> 
                            [anonym_link (of_option (imageV x c)) v]))
    in

    let eo = vo >>= (fun x -> 
                        fiberV (VarO x) g.labels >>= (fun v -> 
                            [anonym_link v (of_option (imageV x d))]))
    in

    let eb = vars |> List.map (fun (x,y) -> anonym_link (of_option (imageV x d)) (of_option (imageV y c)))
                  |> remove_duplicates 
    in
    let check_label = function 
        | VarI x -> not (List.mem x vi)
        | VarO x -> not (List.mem x vo)
        | _      -> true
    in
    let new_labels = g.labels |> List.filter (fun x -> check_label (snd x)) in    
    {
        iports   = g.iports                                        ;
        oports   = g.oports                                        ;
        nodes    = (List.map (fun x -> (snd x, 0, 0)) d) @
                   (List.map (fun x -> (snd x, 0, 0)) c) @ g.nodes ;
        edges    = ei @ eo @ eb @ g.edges                          ;
        labels   = new_labels                                      ;
        ibinders = List.map snd c @ g.ibinders                     ;
        obinders = List.map snd d @ g.obinders                     ;
    };;
   
(* TODO: gestion des ports,
 * ajouter exactement la 
 * modification pour 
 * la compilation directe vers dot
 *)

let constant ~name ~inputs:n ~outputs:m = 
    {
        iports = replicate n 1 |> List.mapi (fun i v -> (v, Some (i+1)));
        oports = replicate m 1 |> List.mapi (fun i v -> (v, Some (i+1)));
        nodes  = [(1,n,m)];
        edges  = [];
        labels = [(1, Const name)];
        ibinders = [];
        obinders = [];
    };;

let ivar ~name = 
    {
        iports = [];
        oports = [(1,None)];
        nodes  = [(1,0,0)];
        edges  = [];
        labels = [(1, VarI name)];
        ibinders = [];
        obinders = [];
    };;

let ovar ~name = 
    {
        iports = [(1,None)];
        oports = [];
        nodes  = [(1,0,0)];
        edges  = [];
        labels = [(1, VarO name)];
        ibinders = [];
        obinders = [];
    };;

let identity ~number = 
    {
        iports = range number |> List.map (fun v -> (v, None));   
        oports = range number |> List.map (fun v -> (v, None));   
        nodes  = range number |> List.map (fun v -> (v,0,0));
        edges  = [];
        labels = [];
        ibinders = [];
        obinders = [];
    };;

(***** COMBINATORS *****)
let rec parallels = function
    | []  -> empty_dag
    | [x] -> x
    |  l  -> let (l1,l2) = split (List.length l / 2) l in 
             parallel (parallels l1) (parallels l2);;

let rec sequences = function
    | []  -> failwith "Empty sequences" 
    | [x] -> x
    |  l  -> let (l1,l2) = split (List.length l / 2) l in 
             sequence (sequences l1) (sequences l2);;

let example_circuit = link [("a","b");("c","d")] (parallels [ovar "a"; ovar "c"; ivar "d"; ivar "b" ]);;

(***** SOUNDNESS CHECKER *****)

let tightening1 ~middle:f ~first:g ~last:h = sequences [g; link [("a","b")] f; h];;
let tightening2 ~middle:f ~first:g ~last:h = link [("a","b")] (sequences [g; f; h]);;
