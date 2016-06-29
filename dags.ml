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
};;

(**
 *
 * The empty dag
 *
 *)
let empty_dag = { iports = []; oports = []; nodes = []; edges = []; labels = [] };;

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
    iports = dag.iports |> List.map (map_port f);
    oports = dag.oports |> List.map (map_port f);
    nodes  = dag.nodes  |> List.map (fun (x,y,z) -> (f x, y, z)); 
    edges  = dag.edges  |> List.map (fun (x,y) -> (map_port f x, map_port f y));
    labels = dag.labels |> List.map (fun (x,y) -> (f x, y));
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
 * Inserting a node in a sorted list 
 * List sorted with the lower numbers at the end 
 *
 * Not tail recursive.
 *
 * @f : the merging function in case of name clash
 * @a : the key
 * @b : the value
 * @l : the assoc list
 *)
let rec insertV ~merge:f ~key:a ~value:b ~func:l = 
    match l with
        | []     -> [(a,b)]
        | (c,d) :: q -> 
                if c < a then 
                    (a,b) :: (c,d) :: q
                else if c = a then 
                    (a, f b d) :: q 
                else
                    (c,d) :: insertV f a b q;;

(** 
 * Get the image of a by 
 * the partial function l
 *
 * returns an option
 *
 * Note: could be done in a more efficient fashion
 * using the ordering
 *)
let imageV ~elem:a ~func:l = 
    l |> List.filter (fun (x,y) -> x = a)
      |> (function []  -> None
                 | [x] -> Some (snd x));;

(** 
 * Get the fiber of b by
 * the partiel function l
 *
 * returns a list
 *)
let fiberV ~elem:b ~func:l = 
    l |> List.filter (fun (x,y) -> y = b)
      |> List.map fst;;

(**
 * Removes duplicates and sorts the list 
 * at the same time
 *)
let remove_duplicates l = 
    let remdup (x,q) y = match x with
        | None   -> (Some y, y :: q)
        | Some t -> if t = y then (Some t,q) else (Some y, y::q)
    in
    l |> List.sort compare 
      |> List.fold_left remdup (None, [])
      |> snd;;


(**
 * Put the list in the right order 
 *)
let correct_list l = List.sort (fun a b -> compare (fst b) (fst a)) l;; 

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
        { iports = np.iports                                          ;
          oports = q.oports                                           ;
          nodes  = np.nodes @ q.nodes                                 ;
          edges  = remove_duplicates (new_links @ np.edges @ q.edges) ;
          labels = np.labels @ q.labels                               ;
        }
    else
        let nq = mapids (fun x -> x + mp) q in  
        let new_links = List.combine p.oports nq.iports in  
        { iports = p.iports                                           ;
          oports = nq.oports                                          ;
          nodes  = nq.nodes @ p.nodes                                 ;
          edges  = remove_duplicates (new_links @ nq.edges @ p.edges) ;
          labels = nq.labels @ p.labels                               ;
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
        let np = mapids (fun x -> x + mq) p in  
        { iports = np.iports @ p.iports                   ;
          oports = np.oports @ q.oports                   ;
          nodes  = np.nodes @ q.nodes                     ;
          edges  = remove_duplicates (np.edges @ q.edges) ;
          labels = np.labels @ q.labels                   ;
        }
    else
        let nq = mapids (fun x -> x + mp) q in  
        { iports = p.iports @ nq.iports                   ;
          oports = p.oports @ nq.oports                   ;
          nodes  = nq.nodes @ p.nodes                     ;
          edges  = remove_duplicates (nq.edges @ p.edges) ;
          labels = nq.labels @ p.labels                   ;
        };;

let (>>=) l f = List.concat (List.map f l);;

let of_option = function 
    | None -> failwith "oups, option none"
    | Some x -> x;;

let link ~vars ~g = 
    let m  = maxid g in 

    let vi = vars |> List.map snd |> remove_duplicates in
    let vo = vars |> List.map fst |> remove_duplicates in 

    let c  = vi |> List.mapi (fun i v -> (v,i + m + 1)) in  
    let ci = List.length c in 
    let d  = vo |> List.mapi (fun i v -> (v,i + m + ci + 1)) in  

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
        iports = g.iports;
        oports = g.oports;
        nodes  = (List.map (fun x -> (snd x, 0, 0)) d) @ 
                 (List.map (fun x -> (snd x, 0, 0)) c) @ g.nodes;
        edges  = ei @ eo @ eb @ g.edges;
        labels = new_labels;
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
        labels = [(1, Const name)]
    };;

let ivar ~name = 
    {
        iports = [];
        oports = [(1,None)];
        nodes  = [(1,0,0)];
        edges  = [];
        labels = [(1, VarI name)]
    };;

let ovar ~name = 
    {
        iports = [(1,None)];
        oports = [];
        nodes  = [(1,0,0)];
        edges  = [];
        labels = [(1, VarO name)]
    };;



let example_circuit = link [("b","a")] (sequence (ivar "a") (ovar "b"));; 


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


(***** SOUNDNESS CHECKER *****)

let tightening1 ~middle:f ~first:g ~last:h = sequences [g; link [("a","b")] f; h];;
let tightening2 ~middle:f ~first:g ~last:h = link [("a","b")] (sequences [g; f; h]);;
