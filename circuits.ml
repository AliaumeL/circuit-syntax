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
     
(* Utils *)
let rec take n = function (* take up-to n *)
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: (take (n-1) xs)

let rec drop n = function (* drop up-to n *)
  | [] -> []
  | x :: xs -> if n = 0 then x :: xs else drop (n-1) xs

let rec rem x = function
  | [] -> []
  | y :: ys -> if x = y then rem x ys else y :: (rem x ys)

let rec zip xs ys = match xs, ys with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: (zip xs ys)
  | _ -> failwith "zip"

let rec rm_dup = function
  | [] -> []
  | [x] -> [x]
  | x :: x' :: xs when x = x' -> rm_dup (x' :: xs)
  | x :: xs -> x :: rm_dup xs

let intersect xs ys =
  List.fold_left (fun zs x ->
      if List.mem x ys then x :: zs
      else zs) [] xs
let subset xs ys =
  List.fold_left (fun b x ->
      b && List.mem x ys) true xs
    
let setify xs = xs |> List.sort compare |> rm_dup
let get_index xs y = 
  let rec get_index' y n = function
	| [] -> failwith "get_index"
	| x :: xs when x = y -> n
	| x :: xs            -> get_index' y (n+1) xs 
  in get_index' y 0 xs

(* Replace first occurrence of x with x' *)
let rec replace_first x x' = function
  | [] -> []
  | y :: ys when x = y -> x' :: ys
  | y :: ys            -> y :: (replace_first x x' ys)

(* Globals *)
let width = 10   (* the default width of a 'tile' *)
let height = 10  (* the default height of a 'tile' *)
let gap = 5      (* gap between tiles when linking or stacking *)
let vn = ref 1   (* provide fresh variable number *)

type gate = Hg | Lg | Dg | Fg | Jg | Xg | Tg | Zg | Ng | Pg | Wg | Mg 

let string_of_gate (g : gate) = match g with
  | Hg  -> "H"   (* high in        *)
  | Lg  -> "L"   (* low in         *)
  | Dg  -> "D"   (* disconnect     *)
  | Fg  -> "F"   (* fork wire      *)
  | Jg  -> "J"   (* join wires     *)
  | Xg  -> "X"   (* cross wires    *)
  | Tg  -> "T"   (* illegal out    *)
  | Zg  -> "Z"   (* high impedance *)
  | Ng  -> "N"   (* n-mos          *)
  | Pg  -> "P"   (* p-mos          *)
  | Wg  -> "W"   (* wait (delay)   *)
  | Mg  -> "MUX" (* multiplexer    *)

let arity = function 
  |  Hg -> (0, 1)
  |  Lg -> (0, 1)
  |  Zg -> (0, 1)
  |  Tg -> (0, 1)
  |  Dg -> (1, 0)
  |  Fg -> (1, 2)
  |  Jg -> (2, 1)
  |  Xg -> (2, 2)
  |  Ng -> (2, 1)
  |  Pg -> (2, 1)
  |  Wg -> (1, 1)
  |  Mg -> (3, 1)
													   
type diag =
  | Unit 
  | Id   
  | Const of gate
  | Boxi  of string * int * int * int * int (* x y m n *)
  | Tens of diag * diag
  | Comp of diag * diag
  | Trc  of diag 

let string_of_diag (d : diag) = match d with
  | Unit  -> ""
  | Id  -> "id"
  | Const g -> string_of_gate g
  | Boxi (s,_,_,_,_) -> s
  | Tens (_,_) 
  | Comp (_,_) 
  | Trc _ -> failwith "string_of_diag"

(* A PROP type is a pair of natural numbers *)
let rec (get_type : diag -> int * int) = function
  | Unit -> (0, 0)
  | Const g -> arity g
  | Id -> (1, 1)
  | Tens (d, d') ->
	 let (m, n) = get_type d in
	 let (m', n') = get_type d' in
	 (m+m', n+n')
  | Comp (d, d') ->
	 let (m, n) = get_type d in
	 let (m', n') = get_type d' in
	 if n <> m' then failwith "get_type comp"
	 else (m, n')
  | Trc d -> let (m, n) = get_type d in (m-1, n-1)
  | Boxi (_, _, _, m, n) -> (m, n)
    
	 
(*********** An embedded DSL ******************)

let ( ** ) a b = Tens (a, b)
let ( $$ ) a b =
  let (_, n) = get_type a in
  let (m, _) = get_type b in
  if m <> n then failwith "$$" 
  else Comp (a, b)
let x = Const Xg
let i = Id
let n = Const Ng
let p = Const Pg
let f = Const Fg
let j = Const Jg
let h = Const Hg
let l = Const Lg
let z = Const Zg
let t = Const Tg
let w = Const Wg
let m = Const Mg
  
let rec mki = function
  | 0 -> Unit
  | 1 -> i
  | n -> i ** (mki (n-1))
  
let rec mkx1 = function
  | 0 -> i
  | n -> (mki (n-1) ** x) $$ (mkx1 (n-1) ** i)

let rec mkx m = function 
  | 0 -> mki m
  | n -> (mkx1 m) ** (mki (n-1)) $$ (i ** (mkx m (n-1)))
  
let rec mkf = function
  | 0 -> failwith "mkf" 
  | 1 -> f
  | n ->
	 let n = n - 1 in
	 let fk = mkf n in
	 let ik = mki n in
	 let xk = mkx1 n in 
	 (fk ** f) $$ (ik ** xk ** i)

let pair d d' =
  let pair' k d d' = (mkf k) $$ (d ** d') in 
  let (m, _) = get_type d in 
  let (m', _) = get_type d' in
  if m <> m' then failwith "pair"
  else pair' m d d'

let rec mktr d = function
  | 0 -> d
  | n -> Trc (mktr d (n-1))

(* untangle a list of mixed red-black wires into red wires 
and black wires using x's *)
let rec untangle = function
  | [] -> (0, 0, Unit)
  | true :: rbs ->
	 let (m, n, d) = untangle rbs in (m+1, n, i ** d)
  | false :: rbs ->
	 let (m, n, d) = untangle rbs in
	 (m, n+1, (mkx m 1) ** (mki n) $$ (i ** d))

(* Mergesort labeled wires *)  
let split xs =
  let n = List.length xs in
  let m = n/2 in
  (take m xs, drop m xs)

let rec merge xs ys = match xs, ys with  
  | [], [] -> Unit, []
  | x, []
  | [], x -> mki (List.length x), x
  | x :: xs, y :: ys when x <= y ->
	 let d, zs = merge xs (y :: ys) in
	 i ** d, x :: zs
  | x :: xs, y :: ys ->
	 let fs = mkx (List.length xs + 1) 1 in 
	 let is = mki (List.length ys) in
	 let d, zs = merge (x :: xs) ys in
	 (fs ** is) $$ (i ** d), y :: zs
	  
let rec mergesort = function
  | [] -> (Unit, [])
  | [x] -> (Id, [x])
  | xs ->
	 let (xs1, xs2) = split xs in 
	 let (d1, xs1') = mergesort xs1 in
	 let (d2, xs2') = mergesort xs2 in
	 let (dout, xs') = merge xs1' xs2' in
	 ((d1 ** d2) $$ dout, xs')

let rec flip = function
  | Unit -> Unit
  | Id -> Id
  | Tens (d, d') -> Tens (flip d, flip d')
  | Comp (d, d') -> Comp (flip d', flip d)
  | Const Xg -> x
  | Const Jg -> f
  | Const Fg -> j								   
  | _ -> failwith "flip"
  
(* Sort an arbitrary permutation of wires *)
let route xs ys = 
  let d = mergesort xs |> fst in
  let d' = mergesort ys |> fst |> flip in
  d $$ d'
				  
(* doubly-parametric fixpoint *)
let fix d k =
  let (m, n) = get_type d in 
  let fk = mkf k in
  let im = mki (n-k) in 
  mktr (d $$ (fk ** im)) k
  
(* unfold a dp-fixpoint. note that d is only the body of the fixpoint *)
let unfold d k = (fix d k) $$ d

(***************************************************************)
(*             Operational Semantics / Rewrite                 *)

  
let _gbl_name = ref 0 									
let nu _ = _gbl_name := !_gbl_name + 1; !_gbl_name

let rec new_names = function
  | 0 -> []
  | n -> (nu ()) :: (new_names (n-1))

module ComparableInts =
struct
  type t = int
  let compare = compare
end

module IntegerDictionary = Map.Make (ComparableInts)

let id_find = IntegerDictionary.find
let id_find' key dict = try
    Some (id_find key dict)
  with _ -> None


(** FUNCTION aliases for integer dictionary **)
let id_empty     = IntegerDictionary.empty
let id_add       = IntegerDictionary.add
let id_map       = IntegerDictionary.map
let id_mapi      = IntegerDictionary.mapi
let id_filter    = IntegerDictionary.filter
let id_fold      = IntegerDictionary.fold
let id_merge     = IntegerDictionary.merge
let id_remove    = IntegerDictionary.remove
let id_mem       = IntegerDictionary.mem
let id_singleton = IntegerDictionary.singleton
let id_find_option x y = 
    try 
        Some (id_find x y)
    with
        Not_found -> None;;

let id_rem_occ key data dic =
  let datas  = id_find key dic in
  let datas' = rem data datas in
  dic |> id_remove key 
      |> id_add key datas'

let id_replace_occ key data data' dic =
  let datas  = id_find key dic in
  let datas' = data' :: (rem data datas) in
  dic |> id_remove key 
      |> id_add key datas'

(* Create a dictionary of fresh renamings *)
let rec new_name_map = function
  | []      -> id_empty
  | x :: xs -> new_name_map xs |> id_add x (nu ())

let map_of_pair_list xys =
  List.fold_left
    (fun map (x, y) -> id_add x y map) id_empty xys

(* this function does not preserve order FIXME *)
let map_to_name_list dict =
    id_fold (fun _ x xs -> x :: xs) dict [] ;;
(* FIXED version of the above function 
 * is exactly the « refresh » function 
 * below ... 
 *)
let map_to_name_list_aliaume (l : int list) (dict : int IntegerDictionary.t) = 
    List.map (fun x -> id_find x dict) l;;
               
let map_to_name_pair_list dict =
  id_fold (fun k x xs -> (k, x) :: xs) dict [] 
               
(* Planar Traced Graphs -- see paper *)  
(* Circuit labels : 
   - signals : high, low, z, illegal
   - structural : fork, join, disconnect, braid
   - gates : nmos, pmos
   - auxiliary : pin of int, to account for unassociative binary gates *)
    
type c_label = 
    High 
  | Low 
  | Z 
  | Illegal 
  | Fork 
  | Join 
  | Disconnect 
  | Braid 
  | Nmos 
  | Pmos 
  | Box 
  | Pin of int 
  | Wait
  | Mux
  
let string_of_label (l:c_label) = match l with
  | High       -> "H"
  | Low        -> "L"
  | Z          -> "Z"
  | Illegal    -> "T"
  | Fork       -> "F"
  | Join       -> "J"
  | Disconnect -> "D"
  | Braid      -> "X"
  | Nmos       -> "N"
  | Pmos       -> "P"
  | Box        -> "B"
  | Pin n      -> "P" ^ (string_of_int n)
  | Wait       -> "W"
  | Mux        -> "MUX"

let label_of_const (g : gate) = match g with
  | Hg  -> High
  | Lg  -> Low
  | Dg  -> Disconnect
  | Fg  -> Fork
  | Jg  -> Join
  | Xg  -> Braid
  | Tg  -> Illegal
  | Zg  -> Z
  | Ng  -> Nmos
  | Pg  -> Pmos
  | Wg  -> Wait
  | Mg  -> Mux

type pTG   =
  {
(* The nodes -- five disjoint sets *)
    ins    : int list;
    outs   : int list;
    pre    : int list;
    main   : int list;
    post   : int list;
    edges  : int list IntegerDictionary.t;
    segde  : (int list IntegerDictionary.t) option; (* ugh *)
    labels : c_label IntegerDictionary.t; 
  }

(* some PTG invariants *)
let vertices_from_edges e =
  let vs = map_to_name_pair_list e in
  List.fold_left
    (fun (vis', vos') (vi, vos) ->
       vi :: vis', vos @ vos') ([], []) vs
    
let wf_ptg t =
  (* the set of nodes is partitioned *)
  let vis, vos = vertices_from_edges t.edges in
  let vs       = t.ins @ t.outs @ t.pre @ t.main in
  intersect t.ins (t.outs @ t.pre @ t.main) = []
  && intersect t.outs (t.pre @ t.main) = []
  && intersect t.pre t.main = []
  && subset vis vs
  && subset vos vs


(* Aliaume's pretty-printing debug stuff *)
let pp_ptg t = 
    let ps  = print_string  in 
    let pn  = print_newline in
    let soi = string_of_int in 
    ps "BEGIN\n";
        ps "\tINS  : "; t.ins  |> List.map soi |> String.concat ", " |> ps; pn ();
        ps "\tOUTS : "; t.outs |> List.map soi |> String.concat ", " |> ps; pn ();
        ps "\tPRE  : "; t.pre  |> List.map soi |> String.concat ", " |> ps; pn ();
        ps "\tPOST : "; t.post |> List.map soi |> String.concat ", " |> ps; pn ();
        ps "\tMAIN : "; t.main |> List.map soi |> String.concat ", " |> ps; pn ();
        ps "\tEDGES\n";
            t.edges 
            |> IntegerDictionary.iter (fun n arrs -> 
                  ps "\t\tNODE "; ps (soi n); ps " -> { ";
                  arrs |> List.map soi |> String.concat ", " |> ps; ps "}"; pn ());
        ps "\tSEGDE\n";
            begin 
                match t.segde with
                    | None   -> ps "\t\t EMPTY \n"
                    | Some x -> x 
                                |> IntegerDictionary.iter (fun n arrs -> 
                                      ps "\t\tNODE "; ps (soi n); ps " -> { ";
                                      arrs |> List.map soi |> String.concat ", " |> ps; ps "}"; pn ());
            end;
        ps "\tLABELS\n";
            t.labels 
            |> IntegerDictionary.iter (fun n lbl -> 
                    ps "\t\tNODE "; ps (soi n); ps " = "; print_string (string_of_label lbl); pn ());

    ps "END\n";;

(***************************************************************)
(*                       DOT representation                    *)

let rec dot_of_edge x ys label_map =
  List.fold_left (fun s y ->
      let l1 = match id_find' x label_map
        with Some d -> (string_of_label d) ^ (string_of_int x)
           | None   -> string_of_int x in 
      let l2 = match id_find' y label_map
        with Some d -> (string_of_label d) ^ (string_of_int y)
           | None   -> string_of_int y in 
      s ^ ("  " ^ l1 ^ " -> " ^ l2 ^ ";\n")) "" ys
 
(* TODO 
 *
 * 1. Les noeuds inutiles doivent être des points
 * 2. Les noeuds doivent avoir des formes différentes
 * 3. Les noeuds doivent êtres groupés sur différents niveaux 
 *    via des subgraphs de graphviz
 *
 *)
let dot_of_ptg ptg =
  "digraph G {\n  rankdir=LR\n" 
  ^ "{ rank = min; \n"
  ^ (List.map (fun x -> (string_of_int x) ^ "; ") (ptg.ins @ ptg.pre) |> String.concat "")
  ^ "};\n" ^
  "{ rank = max; \n"
  ^ (List.map (fun x -> (string_of_int x) ^ "; ") (ptg.outs @ ptg.post) |> String.concat "")
  ^ "};\n" 
  ^ (id_fold (fun x ys s -> (dot_of_edge x ys ptg.labels) ^ s) ptg.edges "")
  ^ (List.fold_left (fun s v -> (string_of_int v) ^ "[shape = diamond];\n" ^ s) "" (ptg.ins @ ptg.outs))
  ^ (List.fold_left (fun s v -> (string_of_int v) ^ "[color = grey];\n" ^ s) "" (ptg.pre @ ptg.post))
  ^ (List.fold_left (fun s v ->
      match id_find' v ptg.labels
      with None         -> (string_of_int v) ^ "[color = black; shape=point];\n" ^ s
         | Some (Pin x) -> (string_of_label (Pin x)) ^ (string_of_int v) ^ "[color = black;label=\"" ^ string_of_label (Pin x) ^ "\"];\n" ^ s
         | Some Fork    -> (string_of_label Fork) ^ (string_of_int v)  ^ "[color = black; shape=point];\n" ^ s
         | Some Join    -> (string_of_label Join) ^ (string_of_int v)  ^ "[color = black; shape=point];\n" ^ s
         | Some l       -> (string_of_label l) ^ (string_of_int v) ^ "[shape = box; color = black;label=\"" ^ string_of_label l ^"\"];\n" ^ s) "" ptg.main)
  ^ "}\n"

let ptg_to_file fname ptg =
  let fhandle = open_out fname in
  ptg |> dot_of_ptg
      |> output_string fhandle;
  close_out fhandle


type verbosity = Quiet | Message | Dump | Verbose
let verbose = ref Verbose
let dump_index = ref 0 (* for naming the outputs *)
  
let report s t = match !verbose with
    | Quiet    -> ()
    | Message  -> print_endline s
    | Dump     ->
      dump_index := 1 + !dump_index;
      let fdot = "/tmp/dump" ^ (string_of_int !dump_index) ^ ".dot" in 
      let fpdf = "/tmp/dump" ^ (string_of_int !dump_index) ^ ".pdf" in
      ptg_to_file fdot t;
      ignore (Sys.command ("dot -Tpdf " ^ fdot ^ " -o " ^ fpdf))
    | Verbose  ->
      dump_index := 1 + !dump_index;
      let fdot = "/tmp/dump" ^ (Printf.sprintf "%003d" !dump_index) ^ ".dot" in 
      let fpdf = "/tmp/dump" ^ (Printf.sprintf "%003d" !dump_index) ^ ".pdf" in
      ptg_to_file fdot t;
      ignore (Sys.command ("dot -Tpdf " ^ fdot ^ " -o " ^ fpdf));
      print_endline ("File " ^ (string_of_int !dump_index) ^ " : " ^ s);
      pp_ptg t

(* IMPORTANT: Name management policy: To reduce the number of
   renamings we must always create PTGs using fresh names AND use
   these PTGs linearly. If need be they must be replicated using this
   function below *)

(* 
 *
 * Refresh = map_..._aliaume !
 *
 * *) 
let rec refresh xs rdic =
  List.map (fun x -> id_find x rdic) xs

(*let rec refresh dic dic' =
  IntegerDictionary.map
    (fun x -> IntegerDictionary.find x dic') dic*)

let rec refresh_edges edges dic =
  id_fold (fun x ys edges ->
      id_add (id_find x dic) (refresh ys dic) edges)
    edges id_empty

let rec refresh_labels labels dic =
  id_fold (fun x diag labels ->
      id_add (id_find x dic) diag labels)
    labels id_empty

let merger_l k a b = match a, b with
  | None  , None   -> None
  | Some a, None   -> Some a
  | None  , Some b -> Some b
  | Some a, Some b -> Some (a @ b |> setify)

let merger_v k a b = match a, b with
  | None  , None              -> None
  | Some a, None              -> Some a
  | None  , Some b            -> Some b
  | Some a, Some b when a = b -> Some a
  | Some a, Some b            -> failwith "merger_v"

let rec id_mergers = function
  | [] -> id_empty
  | x :: xs -> id_merge merger_l x (id_mergers xs)

(* Sometimes we need to do a reverse edge lookup *)
type e_type = int list IntegerDictionary.t

(* benchmark reverse_edges *)
let bmre = ref 0.

let reverse_edges (e : e_type) =
  let merger v_trg vs_src vs_src' = match vs_src, vs_src' with 
    | Some vs, Some vs' -> Some (vs @ vs')
    | Some vs, None     -> Some vs
    | None, Some vs     -> Some vs
    | None, None        -> None
  in
  let t0 = Sys.time () in 
  let e' = id_fold (fun v_src vs_trg e ->
      List.fold_left (fun e v_trg ->
          id_merge merger e (id_singleton v_trg [v_src])
        ) e vs_trg 
      ) e id_empty in
  let dt = Sys.time () -. t0 in bmre := !bmre +. dt;
  e'

let reverse_ptg (t : pTG) : pTG =
  { ins    = t.outs ;
    outs   = t.ins ;
    pre    = t.post ;
    post   = t.pre ;
    main   = t.main ;
    edges  = reverse_edges t.edges ;
    segde  = None ;
    labels = t.labels ;
  }

(* It's important that the order is kept in the node lists *)

(* MODIFIIED : the order was NOT preserved by the 
 * function map_to_name_list ... which is unfortunate 
 * because of the remark above 
 *)
let replicate ptg =
  let ins_new  = new_name_map ptg.ins  in
  let outs_new = new_name_map ptg.outs in
  let pre_new  = new_name_map ptg.pre  in
  let main_new = new_name_map ptg.main in
  let post_new = new_name_map ptg.post in
  (* the total function from the old names 
   * to the new ones 
   *)
  let all_new  = ins_new |> id_merge merger_v outs_new
                         |> id_merge merger_v pre_new
                         |> id_merge merger_v main_new
                         |> id_merge merger_v post_new
  in 
  { ins    = map_to_name_list_aliaume ptg.ins  all_new;
    outs   = map_to_name_list_aliaume ptg.outs all_new;
    pre    = map_to_name_list_aliaume ptg.pre  all_new;
    main   = map_to_name_list_aliaume ptg.main all_new;
    post   = map_to_name_list_aliaume ptg.post all_new;
    labels = refresh_labels ptg.labels all_new;
    edges  = refresh_edges ptg.edges all_new;
    segde  = None;
  }
  
let (test_ptg : pTG) =
  let a0 = nu () in 
  let a1 = nu () in 
  let a2 = nu () in 
  let a3 = nu () in 
  let a4 = nu () in 
  let a5 = nu () in 
  let es = map_of_pair_list
      [(a0,[a2]); (a1,[a2]); (a3,[a4;a5]); (a4,[a1]); (a2,[a3])] in 
  let ls =  map_of_pair_list
      [(a2, Join); (a3, Fork)] in
  {
    ins    = [a0];
    outs   = [a5];
    pre    = [a1];
    main   = [a2; a3];
    post   = [a4];
    edges  = es;
    labels = ls;
    segde  = None;
  }

let test_ptg' = replicate test_ptg

(* We assume the two graphs don't share names *)				  
let tensor_ptg ptg1 ptg2 : pTG = 
  {
    ins    = ptg1.ins  @ ptg2.ins;
    outs   = ptg1.outs @ ptg2.outs;
    pre    = ptg1.pre  @ ptg2.pre;
    main   = ptg1.main @ ptg2.main;
    post   = ptg1.post @ ptg2.post;
    edges  = id_merge merger_l ptg1.edges ptg2.edges;
    segde  = None;
    labels = id_merge merger_v ptg1.labels ptg2.labels;
  }

(* We assume the two graphs don't share names and |ptg1.outs|=|ptg2.ins| *)		  
let compose_ptg ptg1 ptg2 : pTG =
  let links =
    (zip ptg1.outs (List.map (fun x -> [x]) ptg2.ins))
    |> map_of_pair_list in 
  {
    ins    = ptg1.ins;
    outs   = ptg2.outs;
    pre    = ptg1.pre @ ptg2.pre;
    main   = ptg1.main @ ptg2.main @ ptg1.outs @ ptg2.ins;
    post   = ptg1.post @ ptg2.post;
    edges  = id_mergers
        [ptg1.edges; ptg2.edges; links];
    segde  = None;
    labels = id_merge merger_v ptg1.labels ptg2.labels;
  }

(*
let _ = tensor_ptg test_ptg test_ptg'
let _ = compose_ptg test_ptg test_ptg'
*)
  
(***** PTG representation of a morphism *************)

let empty_ptg = {
  ins    = [];
  outs   = [];
  pre    = [];
  main   = [];
  post   = [];
  edges  = id_empty;
  segde  = None;
  labels = id_empty;
}
  
let rec ptg_of_diag = function	
  | Unit -> empty_ptg
  | Id   ->
    let a0, a1, a2 = nu (), nu (), nu () in 
    let t = { empty_ptg with ins   = [a0];
                             main  = [a1];
                             outs  = [a2];
                             edges = map_of_pair_list [(a0,[a1]); (a1,[a2])]; }
    in t
  (* These are the transistors -- non commutative *)
  | Const g when List.mem g [Ng; Pg] ->
    let in1, in2, a, pin1, pin2, out
      = nu (), nu (), nu (), nu (), nu (), nu () in
    let t = { empty_ptg with ins    = [in1; in2];
                             outs   = [out];
                             main   = [a; pin1; pin2];
                             edges  = id_empty
                                      |> id_add in1 [pin1] (* Gate   *)
                                      |> id_add in2 [pin2] (* Source *)
                                      |> id_add pin1 [a]
                                      |> id_add pin2 [a]
                                      |> id_add a [out];
                             labels = id_empty
                                      |> id_add pin1 (Pin 1)
                                      |> id_add pin2 (Pin 2)
                                      |> id_add a (label_of_const g) ; }
    in t
  (* Braiding, also non-commutative *)
  | Const Xg ->
    let in1, in2, a, pin1, pin2, out1, out2
      = nu (), nu (), nu (), nu (), nu (), nu (), nu () in
    let t = { empty_ptg with ins    = [in1; in2];
                             outs   = [out1; out2];
                             main   = [a; pin1; pin2];
                             edges  = id_empty
                                      |> id_add in1 [pin1]
                                      |> id_add in2 [pin2]
                                      |> id_add pin1 [a]
                                      |> id_add pin2 [a]
                                      |> id_add a [out1; out2];
                             labels = id_empty
                                      |> id_add pin1 (Pin 1)
                                      |> id_add pin2 (Pin 2)
                                      |> id_add a Braid ; } 
    in t
  | Const g ->
    let (m, n) = arity g in 
    let nins   = new_names m in
    let nouts  = new_names n in
    let ngate  = nu () in
    let t = { empty_ptg with ins   = nins;
                             outs  = nouts;
                             main  = [ngate];
                             edges = map_of_pair_list
                                 ((ngate, nouts) ::
                                  (List.map (fun x -> (x, [ngate])) nins));
                             labels = map_of_pair_list [(ngate, label_of_const g)] } 
    in t
  | Boxi (name, x, y, m, n) -> (* NOTE: This may not be commutative, use pins! *)
    let nins  = new_names m in
    let nouts = new_names n in
    let ngate = nu () in 
    { empty_ptg with ins   = nins;
                     outs  = nouts;
                     main  = [ngate];
                     edges = map_of_pair_list
                         ((ngate, nouts) ::
                          (List.map (fun x -> (x, [ngate])) nins));
                     labels = map_of_pair_list [(ngate, Box)] }
  | Tens (d1, d2) ->
    let t = tensor_ptg (ptg_of_diag d1) (ptg_of_diag d2) in t
  | Comp (d1, d2) ->
    let t = compose_ptg (ptg_of_diag d1) (ptg_of_diag d2) in t
  | Trc d -> let ptg = ptg_of_diag d in
    let t = { ptg with ins   = List.tl ptg.ins;
                       outs  = List.tl ptg.outs;
                       pre   = (List.hd ptg.ins) :: ptg.pre;
                       post  = (List.hd ptg.outs) :: ptg.post;
                       edges = id_add
                           (List.hd ptg.outs) [List.hd ptg.ins]
                           ptg.edges; } in t

(***************************************************************)
(*                       Rewrite rules                         *)

(* As a general strategy we go through each node in the V3
   component and see if we can reduce it. When we are stuck
   we attempt an unfolding *)

let asrt b = if b then () else failwith "asrt"



(***
 * A function to consistently remove a list
 * of nodes from a graph, with a boolean
 *)
let filter_nodes ~func:f ~graph:ptg = 
    let select_reachable = List.filter f in 
    let update_map m = m |> id_mapi (fun n arrs -> 
                                        if f n then 
                                            select_reachable arrs 
                                        else  [])
                         |> id_filter (fun n arrs -> arrs <> []) 
    in
    {
        ins   = select_reachable ptg.ins  ;
        outs  = select_reachable ptg.outs ;
        pre   = select_reachable ptg.pre  ;
        post  = select_reachable ptg.post ;
        main  = select_reachable ptg.main ;
        (* Suppress the edges from non-reachable nodes 
         * AND the edges TO non-reachable nodes
         *)
        edges = update_map ptg.edges; 
        segde  = None ;
        labels = id_filter (fun n _ -> f n) ptg.labels  
    };;
      
(* return a vertex only if the edge is unique *)
let get_unique_target a d =
  match id_find a d with
  | [b] -> b
  | _   -> failwith "get_unique_target"

(* In general rules have type int -> pTG -> pTG . *)

(* Given a node attempts a yank.
   Only signals should be yanked. 
   This needs to be proved as sufficient. *)

let yank_constant a2 (t:pTG) : pTG =
  try
    let label = id_find a2 t.labels in
    asrt (List.mem label [High; Low; Illegal; Z]);
    let a3 = get_unique_target a2 t.edges in 
    asrt (List.mem a3 t.post);
    let a1 = get_unique_target a3 t.edges in
    asrt (List.mem a1 t.pre);
    let a = get_unique_target a1 t.edges in
    asrt (List.mem a t.main);
    let t = 
      { t with pre = rem a1 t.pre ;
               post = rem a3 t.post ;
               edges = t.edges
                       |> id_remove a2 
                       |> id_remove a3
                       |> id_remove a1
                       |> id_add a2 [a] ; } in 
    let s = Printf.sprintf "Yanked constant %d to %d to %d to %d !" a2 a3 a1 a
    in report s t; t
  with _ -> t

(* Given an identity node, remove it. 
   It is easy not to take an identity node but an arbitrary node
   and remove the node it points to, if its an id node. 
   a -> a' -> a'' where a' is id becomes
   a -> a'' *)
let remove_id a (t:pTG) : pTG =
  try
    let as' = id_find a t.edges in
    List.fold_left
      (fun t a' ->
         try
           asrt (not (id_mem a' t.labels));
           asrt (List.mem a' t.main);
           let a'' = get_unique_target a' t.edges in
           asrt (List.mem a'' t.main);
           let as' = id_find a t.edges in
           (* Here a'' needs to replace a' in the same order *)
           let as' = replace_first a' a'' as' in
           let t = 
             { t with main = rem a' t.main ;
                      edges = t.edges
                              |> id_remove a 
                              |> id_remove a'
                              |> id_add a as' ;} in
           (*let s = Printf.sprintf "Id node removed from %d to %d to %d!" a a' a'' in
           report s t;*)
           t 
         with _ -> t) t as'
  with _ -> t

(* Gate equations of the form
   ((in1, in2, gate), out)
   for n-transistor, p-transitor, join-wire *)
module GateEqns = Map.Make (struct
    type t = c_label * c_label * c_label
    let compare = compare
  end)

let map_of_pair_list_g xys =
  List.fold_left
    (fun map (x, y) -> (GateEqns.add) x y map)
    GateEqns.empty xys

(* Gate, Source 
 *
 * (c_label * c_label * c_label) * c_label
 *
 * Meaning : inputs + gate -> new_gate
 *
 * FIXME not really reliable way to do it
 * *)
let gate_equations =
  map_of_pair_list_g [  
    (Z, Z, Nmos), Illegal;
    (Z, Z, Pmos), Illegal;
    (Z, Z, Join), Z;
    
    (Z, High, Nmos), Illegal;
    (Z, High, Pmos), Illegal;
    (Z, High, Join), High;
    
    (Z, Low, Nmos), Illegal;
    (Z, Low, Pmos), Illegal;
    (Z, Low, Join), Low;
    
    (Z, Illegal, Nmos), Illegal;
    (Z, Illegal, Pmos), Illegal;
    (Z, Illegal, Join), Illegal;
    
    (*-------------*)
    
    (High, Z, Nmos), Z;
    (High, Z, Pmos), Z;
    (High, Z, Join), High;
    
    (High, High, Nmos), Z;
    (High, High, Pmos), Z;
    (High, High, Join), High;
    
    (High, Low, Nmos), Low;
    (High, Low, Pmos), Z;
    (High, Low, Join), Illegal;
    
    (High, Illegal, Nmos), Illegal;
    (High, Illegal, Pmos), Z;
    (High, Illegal, Join), Illegal;
    
    (*-------------*)
    
    (Low, Z, Nmos), Z;
    (Low, Z, Pmos), Z;
    (Low, Z, Join), Low;
    
    (Low, High, Nmos), Z;
    (Low, High, Pmos), High;
    (Low, High, Join), Illegal;
    
    (Low, Low, Nmos), Z;
    (Low, Low, Pmos), Z;
    (Low, Low, Join), Low;
    
    (Low, Illegal, Nmos), Z;
    (Low, Illegal, Pmos), Z;
    (Low, Illegal, Join), Illegal;
    
    (*-------------*)
    
    (Illegal, Z, Nmos), Z;
    (Illegal, Z, Pmos), Z;
    (Illegal, Z, Join), Illegal;
    
    (Illegal, High, Nmos), Illegal;
    (Illegal, High, Pmos), Illegal;
    (Illegal, High, Join), Illegal;
    
    (Illegal, Low, Nmos), Illegal;
    (Illegal, Low, Pmos), Illegal;
    (Illegal, Low, Join), Illegal;
    
    (Illegal, Illegal, Nmos), Illegal;
    (Illegal, Illegal, Pmos), Illegal;
    (Illegal, Illegal, Join), Illegal
  ]

(* Rewrite a graph by reducing a constant at a. 
   Note that labeled nodes have to be in main, so we don't check again. 
   We have several situations:
   1. commutative vs. non-commutative gates
   2. reductions determined by a single constant 
      For Nmos, G in [Z; Low] 
      For Pmos, G in [Z; High]
      For Joing, any of the pins being Illegal
   vs. reductions requiring both constants in other cases *)
let is_value (l:c_label) = match l with
  | High  
  | Low  
  | Z  
  | Illegal  -> true
  | _        -> false


(* reduce MUX 
 *
 * @ptg      : the initial graph
 * @mux_node : the multiplexer node
 *
 * @returns  : a new graph with values 
 *             propagated through 
 *             the node if possible
 * *)
let reduce_mux mux t =
    print_string "REDUCING MUX\n";
    let segde  = reverse_edges t.edges in 
    (* 
     * find the nodes that connects to the
     * MUX gate
     *)
    let inputs = id_find mux segde     in 

    (* FIXME could fail if no label ... *)
    let names  = inputs 
              |> List.map (fun x -> id_find x t.labels) 
    in
    let [Pin a; Pin b; Pin c] = names  in (* The ancestor of a MUX gate are always Pins *)
    (* 
     * order the inputs respecting the pin 
     * numbers 
     *)
    let ordered_inputs = zip [a;b;c] inputs 
               |> List.sort (fun k l -> compare (fst k) (fst l))
               |> List.map snd
    in
    let [p1;p2;p3]            = ordered_inputs in (* There are always 3 inputs values *)
    (* 
     * Now get the nodes attached to each pin number 
     * with their associated values 
     *)
    let (control_node,control_value) = ordered_inputs 
                     |> List.hd 
                     |> (fun x -> get_unique_target x segde)
                     |> (fun n -> (n, id_find_option n t.labels)) 
    in
    (* first we remove the value and the first pin *)
    let new_graph1 = filter_nodes (fun x -> x <> control_node && x <> p1) t in
    let new_graph2 = { new_graph1 with
        labels = new_graph1.labels
              |> id_remove mux
              |> id_remove p2
              |> id_remove p3;

        edges  = new_graph1.edges 
              |> id_remove p2
              |> id_remove p3
                     }
    in
    begin
        (* propagation through the MUX gate *)
        match control_value with
            | Some Illegal ->  
                    {
                        new_graph2 with 
                        labels = new_graph2.labels
                              |> id_add mux Illegal
                              |> id_add p3  Z
                              |> id_add p2  Z
                    }
            | Some Z       ->
                    {
                        new_graph2 with 
                        labels = new_graph2.labels
                              |> id_add mux Z 
                              |> id_add p3  Z
                              |> id_add p2  Z
                    }
            | Some High    -> 
                    {
                        new_graph2 with 
                        labels = new_graph2.labels
                              |> id_add p3  Z;
                        edges  = new_graph2.edges
                              |> id_add p2  [mux];

                    }
            | Some Low     ->
                    {
                        new_graph2 with 
                        labels = new_graph2.labels
                              |> id_add p2  Z;
                        edges  = new_graph2.edges
                              |> id_add p3  [mux];

                    }
            | _            -> t
    end;;


(* what is a ? I think it's the node *)
let reduce_constant a (t:pTG) : pTG =
  let segde = reverse_edges t.edges in 
  try
    let gate = id_find a t.labels in
    if (List.mem gate [ Nmos;  Pmos ]) then (* non-commutative constants *)
      begin
        let [a1; a2] = id_find a segde in
        let Pin i = id_find a1 t.labels in (* Gate *)
        let Pin j = id_find a2 t.labels in (* Source *)
        let a1, a2 = if i < j then a1, a2 else a2, a1 in (* nodes in pin order *)
        let a01 = get_unique_target a1 segde in
        let a02 = get_unique_target a2 segde in
        let v1 = id_find a01 t.labels in
        let v2 = id_find a02 t.labels in 
        if is_value v1 && is_value v2 then 
          let v = GateEqns.find (v1, v2, gate) gate_equations in
          let t = { t with main   = t.main
                                    |> rem a1
                                    |> rem a2
                                    |> rem a01
                                    |> rem a02 ;
                           edges  = t.edges
                                    |> id_remove a1
                                    |> id_remove a2 
                                    |> id_remove a01
                                    |> id_remove a02 ;
                           labels = t.labels
                                    |> id_remove a1
                                    |> id_remove a2 
                                    |> id_remove a01
                                    |> id_remove a02
                                    |> id_remove a
                                    |> id_add a v} in
          let s = Printf.sprintf "Transistor reduced from %d with %d and %d!" a a1 a2 in
          report s t; t
          (* special case reduction, Source doesn't matter *)
        else if (gate = Nmos) && (v1 = Z || v1 = Low)
             || (gate = Pmos) && (v1 = Z || v1 = High) then
          let v = GateEqns.find (v1, Illegal, gate) gate_equations in
          let t = { t with main = t.main |> rem a1 |> rem a01 ;
                           edges  = t.edges 
                                    |> id_remove a1
                                    |> id_remove a2
                                    |> id_remove a01;
                           labels = t.labels
                                    |> id_remove a1
                                    |> id_remove a2 
                                    |> id_remove a01
                                    |> id_remove a
                                    |> id_add a2 Disconnect
                                    |> id_add a v ;} in
          let s = Printf.sprintf "Transistor special reduction from %d with %d and %d!" a a1 a2 in
          report s t; t
        else t
      end
    else if gate = Join then
      let [a1; a2] = id_find a segde in
      let v1 = id_find a1 t.labels in
      asrt (List.mem v1 [ Z;  High; Low; Illegal]);
      let v2 = id_find a2 t.labels in 
      asrt (List.mem v2 [ Z;  High; Low; Illegal]);
      let v = GateEqns.find (v1, v2, gate) gate_equations in
      let t = { t with main = t.main |> rem a1 |> rem a2 ;
                       edges  = t.edges
                                |> id_remove a1
                                |> id_remove a2 ;
                       labels = t.labels
                                |> id_remove a1
                                |> id_remove a2 
                                |> id_remove a
                                |> id_add a v} in
      let s = Printf.sprintf "Join reduced from %d with %d and %d!" a a1 a2 in
      report s t; t
    else if gate = Mux then
        reduce_mux a t
    else t
  with
    _ -> t;; (* the argument given is not a gate ... *)


(**
 *
 * A function to remove labels from « false » 
 * fork nodes (with only one output)
 *
 *)
let update_fork_join_nodes node ptg = 
    let is_real_multiple k l = match l with
        | Fork -> 
                if not (id_mem k ptg.edges) || List.length(id_find k ptg.edges) <= 1 then
                    false
                else true
        | _    -> true
    in
    if id_mem node ptg.labels then
        if is_real_multiple node (id_find node ptg.labels) then
            ptg
        else
            { ptg with labels =  ptg.labels |> id_remove node }
    else
        ptg;;

(* Propagate a constant through a fork. 
   This rule can be generalised to the whole power of the diagonal rule, 
   but I suspect that is not required. *)
let fork a (t : pTG) : pTG =
  try
    let v = id_find a t.labels in
    asrt (List.mem v [ Z;  High;  Low;  Illegal]);
    let a1 = get_unique_target a t.edges in
    let g = id_find a1 t.labels in
    asrt (g = Fork);
    let [a2; a3] = id_find a1 t.edges in
    let a1', a1'' = nu (), nu () in 
    let t = { t with main = a1' :: a1'' :: t.main |> rem a |> rem a1 ;
                     edges = t.edges
                             |> id_remove a |> id_remove a1
                             |> id_add a1' [a2] |> id_add a1'' [a3] ;
                     labels = t.labels
                              |> id_remove a |> id_remove a1
                              |> id_add a1' v |> id_add a1'' v ; } in
    (* let s = Printf.sprintf "Forked at %d from %d!" a a1 in 
       report s t; *)
    t
  with _ -> t
  
(* Reduce a braiding. 
   For efficiency this rule needs a reverse lookup, like the gates. *)
let braid a (t : pTG) : pTG =
  let segde = reverse_edges t.edges in 
  try
    let x = id_find a t.labels in
    asrt (x = Braid);
    let [a1; a2] = id_find a segde in
    let [a1'; a2'] = id_find a t.edges in 
    let Pin i = id_find a1 t.labels in
    let Pin j = id_find a2 t.labels in
    let a1, a2 = if i < j then a1, a2 else a2, a1 in (* nodes in pin order *)
    let a01 = get_unique_target a1 segde in
    let a02 = get_unique_target a2 segde in
    let t = { t with 
                     main   = t.main   |> rem a1 
                                       |> rem a2 
                                       |> rem a;
                     edges  = t.edges  |> id_remove a
                                       |> id_remove a1  
                                       |> id_remove a2
                                       |> id_remove a01 
                                       |> id_remove a02
                                       |> id_add a01 [a2'] 
                                       |> id_add a02 [a1'] ;
                     labels = t.labels |> id_remove a
                                       |> id_remove a1  
                                       |> id_remove a2 ;} in
    (* let s = Printf.sprintf "Braided at %d with %d and %d!" a a01 a02 in 
       report s t; *)
    t
  with _ -> t

(* Unfold the iterator -- see the notes *)

(* Create and add an n-ary fork to a PTG *)
let mk_fork (nodes : (int * (int * int)) list) t =
  let n = List.length nodes in
  let fork_nodes = new_names n in 
  { t with main = t.main @ fork_nodes ;
           edges = List.fold_left
               (fun e (vf, (i'', (i, i')))
                 -> e
                    |> id_add i'' [vf] 
                    |> id_add vf [i; i'])
               t.edges (zip fork_nodes nodes) ;
           labels = List.fold_left
               (fun e v -> id_add v Fork e)
               t.labels fork_nodes ; }

(*** fork into n wires ***)
let rec fork_to init nodes_list ptg = 
    match nodes_list with
        | []     -> ptg
        | [t]    -> {
            ptg with
            edges = ptg.edges |> id_merge merger_l (map_of_pair_list [init, [t]])
        }
        | t :: q -> 
                let [fork_node] = new_names 1 in 
                let new_graph = fork_to fork_node q ptg in  
                let new_edges = [ (fork_node, [t]);
                                  (init, [fork_node]) ]
                in
                { new_graph with
                    main  = fork_node :: new_graph.main;
                    edges = new_graph.edges 
                         |> id_merge merger_l (map_of_pair_list new_edges) ;
                    labels = new_graph.labels |> id_add fork_node Fork
                };;


(*** join n wires into one ***)
let rec join_to final nodes_list (ptg : pTG) = 
    match nodes_list with
        | []     -> ptg
        | [t]    -> {
            ptg with
            edges = ptg.edges |> id_merge merger_l (map_of_pair_list [t, [final]])
        }
        | t :: q -> 
                let [join_node]  = new_names 1 in 
                let new_graph = join_to join_node q ptg in  
                let new_edges = [ (t, [join_node]);
                                  (join_node, [final]) ]
                in
                { new_graph with
                    main  = join_node :: new_graph.main;
                    edges = new_graph.edges 
                         |> id_merge merger_l (map_of_pair_list new_edges) ;
                    labels = new_graph.labels |> id_add join_node Join 
                };;

(***
 *
 *
 * There is no need to have this function 
 * so complex. 
 *
 * The checking and duplication in case 
 * of different cardinals for pre/post
 * is unnecessary because it cannot happen
 *
 * (if it happens the graph is ill formed)
 *
 * NOTE the code is still valid with these checks 
 * but just more complex
 *
 *)
let unfold_ptg (t1:pTG) =
    (* If there is no trace ... then no need
     * to unfold it ...
     *)
    if t1.pre = [] or t1.post = [] then
        t1
    else
  let t2    = replicate t1 in
    print_newline ();
  (* Creating new names for the circuit's 
   * new input nodes 
   *)
  let n_inputs = new_names (List.length t1.ins)   in

  (* Creating new names for the circuit's
   * new global post nodes 
   *)
  let n_posts     = new_names (List.length t1.post)  in
  
  (* Creating new names for the circuit's 
   * « ghost » t1.post that will connect 
   * to the copy's (t2) pre elements
   *)
  let n_pre_pre   = new_names (List.length t1.post)  in

  (* Creating new nodes that will be labeled to _|_
   * used to ignore the output of the first graph t1
   *)
  let ignore_outs = new_names (List.length t1.outs)  in

  (* Creating new nodes that will be labeled 
   * to _|_ used to ignore the output of the 
   * post nodes from t2
   *)
  let ignore_post = new_names (List.length t2.post) in

  (* EDGES *)
  let ignore_outs_e   = zip t1.outs (List.map (fun x -> [x]) ignore_outs) in 
  let ignore_post_e   = zip t2.post (List.map (fun x -> [x]) ignore_post) in 

  let remove_feedback = fun e -> List.fold_left (fun e v -> id_remove v e) e (t1.post @ t2.post) in 

  let new_feedback    = zip n_posts t1.post 
                     |> List.map (fun (new_p,old_p) -> 
                             (new_p, id_find old_p t1.edges))
  in

  let new_false_feedback = zip n_pre_pre t2.post 
                     |> List.map (fun (new_p,old_p) -> 
                             (new_p, id_find old_p t2.edges))
  in

  let calcul_edges    = id_merge merger_l t1.edges t2.edges
                     |> remove_feedback 
                     |> id_merge merger_l (map_of_pair_list ignore_outs_e)
                     |> id_merge merger_l (map_of_pair_list ignore_post_e)
                     |> id_merge merger_l (map_of_pair_list new_feedback) 
                     |> id_merge merger_l (map_of_pair_list new_false_feedback) 
  in

  (*
   * A  first construction of the graph 
   * without the forking of inputs and of t1.post 
   *)
  let t_no_forks = 
  { ins  = n_inputs  ; (* the new set of input nodes *)
    outs = t2.outs   ; (* keep the outs of t2        *) 
    pre  = t1.pre    ; (* keep the pre of t1         *)
    post = n_posts   ; (* the new post nodes         *)
    main = ignore_outs 
                 @ ignore_post 
                 @ n_pre_pre
                 (* the internal nodes comming from t1 *)
                 @ t1.ins  
                 @ t1.outs
                 @ t1.main 
                 @ t1.post 
                 (* the internal nodes comming from t2 *)
                 @ t2.ins 
                 @ t2.main 
                 @ t2.post 
                 @ t2.pre ;
    edges = calcul_edges ; 
    segde = None;
    labels = (let l = id_merge merger_v t1.labels t2.labels in
              List.fold_left
                (fun e v -> id_add v Disconnect e)
                l (ignore_outs @ ignore_post)) ;
  } in

  (** forking the inputs **)
  let test_prout = zip n_inputs (zip t1.ins t2.ins) in  

  let t_forks  = mk_fork test_prout t_no_forks in 

  let test_prout = zip t1.post (zip n_pre_pre n_posts) in  
  let t_forks  = mk_fork test_prout t_forks in 

  report "Unfolded " t_forks;
  t_forks

(* Reduce a dangling node 
   Obs: We don't handle braids and identity here.
        They are handled elsewhere in separate rules. *)
let dangle (a:int) (t:pTG) :pTG =
  let segde = reverse_edges t.edges in
  try (* DO WE NEED TO YANK THE DANGLE AROUND THE LOOP ? *)
    if id_find a t.labels = Disconnect then
      begin
        let a0 = get_unique_target a segde in
        let l0 = id_find a0 t.labels in (* see obs re. identity *)
        if List.mem l0 [High; Low; Z; Illegal] then
          begin (* delete an input *)
            let t = {t with main = t.main |> rem a |> rem a0;
                            edges = t.edges |> id_remove a |> id_remove a0;
                            labels = t.labels |> id_remove a |> id_remove a0;
                    } in
            report ("Dangling input at " ^ (string_of_int a0)) t;
            t
          end
        else if l0 = Fork then
          begin (* Replace a fork with a direct connection *)
            let a1 = get_unique_target a0 segde in
            let [a'] = rem a (id_find a0 t.edges) in
            let t = { t with main = t.main |> rem a |> rem a0 ;
                             edges = t.edges
                                     |> id_remove a0
                                     |> id_replace_occ a1 a0 a' ; 
                             labels = t.labels
                                      |> id_remove a0
                                      |> id_remove a ; } in
            report ("Dangle fork at "^ (string_of_int a)) t;
            t
          end
        else if l0 <> Braid then 
          begin (* Propagate the disconnect see obs re. braid *)
            let ais = id_find a0 segde in
            let aisd = new_names (List.length ais) in
            let ed = zip ais
                (List.map (fun x -> [x]) aisd) |> map_of_pair_list in 
            let t = { t with main = (t.main |> rem a |> rem a0) @ aisd ;
                             edges =
                               (let e = t.edges |> id_remove a0 in
                                let e = List.fold_left
                                    (fun e v -> id_rem_occ v a0 e) e ais in
                                id_merge merger_l e ed) ;
                             labels =
                               (let l = t.labels
                                        |> id_remove a
                                        |> id_remove a0 in
                                List.fold_left
                                  (fun l v -> id_add v Disconnect l) l aisd) ;
                    }
            in report ("Dangle general at " ^ (string_of_int a)) t;
            t
          end
        else t
      end
    else t
  with _ -> t

(* Eliminate nodes that don't have impact on output *)
let rec mark segde visited = function (* on the fringe *)
  | [] -> visited
  | n :: ns ->
    if List.mem n visited then mark segde visited ns 
    else let visited = n :: visited in 
      (match id_find' n segde with
       | None -> mark segde visited ns
       | Some ns' -> mark segde visited (List.rev_append ns ns'))
     

let mark_and_sweep t =
  let segde = reverse_edges t.edges in
  let reachable = mark segde [] (t.outs) in 
  let filter_func x = List.mem x reachable in 
  let t = filter_nodes filter_func t in 
  let s = "Garbage collect!" in
  report s t;
  t;;

(* Apply all the rules to all the nodes *)
let rewrite_tpg' rules t =
  let allnodes = t.ins @ t.outs @ t.pre @ t.post @ t.main in
  List.fold_left
    (fun t a ->
       List.fold_left (fun t r -> r a t) t rules) t allnodes

let rewrite_tpg t =
  rewrite_tpg' [dangle;
                remove_id;
                yank_constant;
                reduce_constant;
                fork;
                update_fork_join_nodes; 
                braid] t

(*
First apply all the local rules until stuck.
Then unfold the circuit.
Repeat n times. 
*)
let rec rewrite_tpg_nf t = function
  | 0 -> mark_and_sweep t
  | n -> 
    let t' = rewrite_tpg t in
    if t == t' then
      let t' = mark_and_sweep t in 
      print_endline "Unfolding!";
      rewrite_tpg_nf (unfold_ptg t') (n - 1)
    else rewrite_tpg_nf t' n
    
(***************************************************************)
(*                             Testing                         *)

let n' = x $$ n
let p' = x $$ p

(* We read the lines 1-lowest to highest going up. *)
let inva = h ** f ** l
let invb = p' ** n
let inv = inva $$ invb $$ j

(* Line 1 control ; line 2 data : (control ** data) $$ c *)
let c = (f ** f)
        $$ (inv ** i ** i ** i)
        $$ (i ** x ** i)
        $$ (p ** n)
        $$ j
      
(* line 1 on SEL ; line 2 on NOTSEL; line 3 SEL *)
let mux = (f ** i ** i)
          $$ (i ** x ** i)
          $$ (i ** i ** inv ** i)
          $$ (c ** c)
          $$ j

(* Simple feedback diagrams *)
let loop = Trc (j $$ f $$ (w ** i))


(* latch seems buggy *)
let latch = (i ** f) $$ (c ** inv) $$ (Trc ((i ** x) $$ (i ** inv ** i) $$ (i ** c) $$ j $$ inv $$ f))

let boxf = Boxi ("F", width*4, height*4, 1, 1)
let boxg = Boxi ("G", width*4, height*4, 1, 1)
  
let bsm = (fix (
				(x ** f)
				$$ (f ** x ** i)
				$$ (f ** (mux $$ boxg $$ f) ** i)
				$$ (i ** i ** i ** x)
				$$ (i ** (mux $$ boxf) ** i)
				$$ (x ** i)) 1) $$ (x ** i) $$ mux


let bsm'' = (fix (
				(x ** f)
				$$ (f ** x ** i)
				$$ (f ** (mux $$ boxg $$ f) ** i)
				$$ (i ** i ** i ** x)
				$$ (i ** (mux $$ boxf) ** i)
				$$ (x ** i)) 1)

             
let diag_to_file fname diag =
  diag |> ptg_of_diag |> ptg_to_file fname

let run_test t f n =
  print_endline ("Testcase : " ^ f);
  let f1 = "/tmp/tests/" ^ f ^ "_pre.dot" in
  let f1' = "/tmp/tests/" ^ f ^ "_pre.pdf" in
  ptg_to_file f1 t;
  ignore (Sys.command ("dot -Tpdf " ^ f1 ^ " -o " ^ f1'));
  let f3 = "/tmp/tests/" ^ f ^ "_post.dot" in
  let f3' = "/tmp/tests/" ^ f ^ "_post.pdf" in
  let t = rewrite_tpg_nf t n in
  ptg_to_file f3 t;
  ignore (Sys.command ("dot -Tpdf " ^ f3 ^ " -o " ^ f3'))

let run_nullary gate name n = 
  run_test (gate |> ptg_of_diag) name n

let run_unary gate name n =
  List.iter 
    (fun v ->
       run_test
         (v $$ gate |> ptg_of_diag)
         (name ^ (string_of_diag v)) n)
    [h; l; t; z]

let run_binary gate name n =
  List.iter (fun v' ->
      (List.iter
         (fun v -> run_test
             ((v ** v') $$ gate |> ptg_of_diag)
             (name ^ (string_of_diag v) ^ (string_of_diag v')) n)
         [h; l; t; z])) [h; l; t; z]
      
let run_ternary gate name n =
  List.iter (fun v'' ->
      (List.iter (fun v' ->
           (List.iter
              (fun v -> run_test
                  ((v ** v' ** v'') $$ gate |> ptg_of_diag)
                  (name ^ (string_of_diag v)
                   ^ (string_of_diag v')
                   ^ (string_of_diag v'')) n)
              [h; l; t; z]))
          [h; l; t; z]))
    [h; l; t; z]



(******** ALIAUME HOOK INTO THE CODE **********)

open Dags;;

(* Converting the labels 
 * for constants into PTG labels 
 * *)  
let convert_lab = function
    | VarI x -> Box 
    | VarO x -> Box
    | Const g ->
            begin 
                match g with
                   | "MUX"  -> Mux 
                   | "BOT"  -> Z
                   | "TOP"  -> Illegal
                   | "NMOS" -> Nmos
                   | "PMOS" -> Pmos
                   | "HIGH" -> High
                   | "LOW"  -> Low
                   |   _    -> Box
            end;;

(* Converting from a liDAG to a ptg
 *
 * Lots of conversions 
 * 1. Inputs are not handled the same way
 * 2. Labels are not « compatible » 
 * 3. The notion of « port » needs to be 
 *    translated into the notion of « pin » 
 *    (which is the bad part of the code)
 *
 * *)

let trace_of_binders dag (ptg : pTG) =  
    (* add the pre binding nodes to the ptg *)
    let add_pre_binders (ptg : pTG) inode = (* function to add the pre_binders of a ibind node *)
        let inputs = dag.edges |> List.filter (fun e -> fst (snd e) = inode)   
                               |> List.map (fun e -> fst (fst e))
        in
        let pre_nodes = new_names (List.length inputs) in 
        let new_graph = join_to inode pre_nodes ptg in
        let new_edges = zip inputs (List.map (fun x -> [x]) pre_nodes) in 
        { new_graph with 
            pre   = pre_nodes @ new_graph.pre ;
            edges = new_graph.edges 
                 |> id_merge merger_l (map_of_pair_list new_edges)  
        }
    in
    
    (* this time we work using the edges inside the pTG
     * because we linked the pre_nodes to 
     * the obinder nodes during the construction, therefore,
     * we can get the list of nodes to connect just using 
     *
     * id_find inode ptg.edges 
     *)
    let add_post_binder (ptg : pTG) inode = 
        let connections = id_find inode ptg.edges in 
        let new_graph   = { ptg with
                                edges = ptg.edges 
                                     |> id_remove inode
        } in 
        let post_nodes = new_names (List.length connections) in 
        let new_graph2 = fork_to inode post_nodes new_graph  in 
        let new_edges  = zip post_nodes (List.map (fun x -> [x])connections) in 
        {
            new_graph2 with
            post = post_nodes @ new_graph2.post;
            edges = new_graph2.edges 
                 |> id_merge merger_l (map_of_pair_list new_edges)
        }
    in

    (*
     * Add the big fork from ibinder to the input variable 
     * nodes 
     *)
    let fork_ibinder (ptg : pTG) inode = 
        let connect = dag.edges |> List.filter (fun e -> fst (fst e) = inode)   
                                |> List.map (fun e -> fst (snd e))
        in
        fork_to inode connect ptg
    in

    (*
     * Add the big join from obinder to the outputs variable 
     * nodes 
     *)
    let join_obinder (ptg : pTG) inode = 
        let connect = dag.edges |> List.filter (fun e -> fst (snd e) = inode)   
                                |> List.map (fun e -> fst (fst e))
        in
        join_to inode connect ptg
    in

    let folding a b c = List.fold_left a c b in 
    ptg |> folding add_pre_binders dag.ibinders
        |> folding add_post_binder dag.obinders
        |> folding fork_ibinder    dag.ibinders
        |> folding join_obinder    dag.obinders;;

let ptg_of_dag dag =  
    (* Starts by ignoring all the additional information
     * inside the dag.
     *)
    let nodes  = dag.nodes  |> List.map (fun (x,y,z) -> x) in 
    let iport  = dag.iports |> List.map (fun (x,y) -> x)   in 
    let oport  = dag.oports |> List.map (fun (x,y) -> x)   in 
    let ibind  = dag.ibinders in
    let obind  = dag.obinders in 

    (* the « inside nodes » of the DAG !!! Theses are NOT
     * the inside nodes of the whole ptg
     *)
    let inside = Utils.remove_list nodes (ibind @ obind) in
    
    (* Creating the input nodes and outputs nodes *)
    let ins    = iport |> List.mapi (fun k _ -> k + Dags.maxid dag + 3) in
    let outs   = oport |> List.mapi (fun k _ -> k + Dags.maxid dag + List.length ins + 3) in
    (* now we have the 5 disjoint sets of nodes *)

    (* update the global counter *)
    _gbl_name := Dags.maxid dag + List.length ins + List.length outs + 7;

    (**** PINS ****)
    (* we need pins to convert from « ports » to 
     * regular nodes, while keeping the notion of order 
     *
     * WE CREATE ONLY INPUT PINS because 
     * the design of pTG does NOT allow 
     * specific outputs ... 
     *)
    let pins   = dag.nodes 
              |> List.map (fun (x,a,b) -> (x, new_names a)) 
    in
    let pins_nodes = pins |> List.map snd |> List.concat in

    let pins_edges = pins
                  |> List.map (fun (x,i_pins) ->  
                          List.map (fun pin -> (pin,x)) i_pins)
                  |> List.concat
    in

    let pins_labels = pins 
                  |> List.map (fun (x,i_pins) -> 
                          List.mapi (fun i pin -> (pin, Pin i)) i_pins)
                  |> List.concat
    in

    let find_pin ~node ~pin ~pos = 
        Utils.imageV ~elem:node ~func:pins
                  |> Utils.of_option
                  |> (fun x -> 
                          List.nth x (pin-1))
    in

    let find_pin_right n x = find_pin ~node:n ~pin:x ~pos:true  in 
    let find_pin_left  n x = n in (* There can be no pin for an output *)

    (* The edges trying to keep port information 
     * if the node specifies a port, we fetch the corresponding one 
     * in the pins table 
     * *)
    let convert_edge ((a,b),(c,d)) = match (b,d) with
        | None, None     -> (a,c) 
        | Some x, None   -> (find_pin_left a x, c) 
        | None, Some x   -> (a, find_pin_right c x)
        | Some x, Some y -> (find_pin_left a x, find_pin_right c y)
    in
    let edges1 = (List.combine dag.oports (List.map (fun x -> (x,None)) outs)) 
               @ (List.combine (List.map (fun x -> (x,None)) ins) dag.iports) 
               @ dag.edges
    in
    let edges2 = edges1 |> List.map convert_edge |> (fun x -> x @ pins_edges) in 
    List.iter (fun (x,y) -> print_int x; print_string " -> "; print_int y; print_newline ())
              edges2;
    let get_neighbours x = edges2 |> List.filter (fun (a,b) -> a = x) |> List.map snd in 
    (* The edges organised in an adjency list [complexity = awfull] 
     * NOTE don't forget the nodes from the ins and outs !
     **)
    let edges3 = (pins_nodes @ ins @ outs @ inside) (* we ignore edges for the ibinds/obinds ! *)
              |> List.map (fun x -> (x, get_neighbours x))
              |> List.filter (fun x -> not (snd x = []))
    in
    (* Now we have the edges *)


    let labs1 = dag.labels |> List.map (fun (x,y) -> (x, convert_lab y)) in  
    (* NOTE only the nodes (inside) can be labeled, it is safe to ignore the ins/outs *)
    let labs2 = nodes |> List.map (fun x -> (x, Utils.imageV x labs1))   in
    let pair_opt (a,b) = match b with
        | None   -> None
        | Some x -> Some (a,x)
    in
    let labs3 = labs2 |> List.map pair_opt |> Utils.list_of_options  in 


    let t = {
        ins    = ins                     ;
        outs   = outs                    ;
        pre    = []                   ;
        main   = ibind @ obind @inside @ pins_nodes     ;
        post   = []                   ;
        edges  = map_of_pair_list edges3 ;
        segde  = None                    ;
        labels = map_of_pair_list (labs3 @ pins_labels)  ;
    }
    in
    report "ALIAUME INIT" t;
    
    (* The current graph has NO trace (because edges for ibinds/obinds were 
     * ignored)
     *
     * We now call the function to add them 
     *)
    let t2 = trace_of_binders dag t in
    report "ALIAUME TRACED" t2;
    t2;;


let example_expr = 
    let ic    = open_in "lines.txt" in 
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



let run =
  let t0 = Sys.time () in 
  dump_index := 2;
  verbose := Verbose;
  (* run_ternary mux "mux" *)
  run_test (ptg_of_dag example_expr) "test-aliaume" 6;
  (* run_nullary ((h ** h) $$ bsm) "berry-mendler" 5; *)
  let dt = Sys.time () -. t0 in
  Printf.printf "Running time %f out of which reversing %f.\n" dt !bmre
    
(* 
   TODO
   1. Connect to the syntax that has variables.
   2. Deal with WAVEFORMS (delays). 
   3. Efficient implementation/refactoring (if we have time).
 *)
