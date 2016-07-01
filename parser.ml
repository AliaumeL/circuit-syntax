(***
 *
 * Parser 
 *
 * Aliaume Lopez
 *
 *)




(* ocaml is terrible *)
let string_of_char c = String.make 1 c;;

let is_upper c = 
    let i = Char.code c in
    i <= 90 && i >= 65;;

let is_lower c = 
    let i = Char.code c in
    i <= 172 && i >= 97;;

let is_numeric c = 
    let i = Char.code c in
    i <= 57 && i >= 48;;

let is_alpha c    = is_upper c || is_lower c;;
let is_alphanum c = is_alpha c || is_numeric c;;

(* ocaml is REALLY terrible *)
let append x y = x :: y;;

(* little parsing utilities, to make the 
 * all thing simpler to write *)
type 'a parse = string -> int -> ('a * int) option;; 

let pure x = fun _ i -> Some (x,i);;

let fmap f p = fun s i ->  
    match p s i with
        | None -> None
        | Some (x,j) -> Some (f x, j);;

let (<|>) p q = fun s i -> 
    match p s i with
        | None -> q s i 
        | Some (x,j) -> Some (x,j);;

let (<*>) p q = fun s i -> 
    match p s i with
        | None -> None 
        | Some (x,j) -> begin 
            match q s j with
                | None -> None 
                | Some (y,t) -> Some (x y, t)
        end;;

let (<*>>) p q = pure (fun x y -> y) <*> p <*> q;;
let (<<*>) p q = pure (fun x y -> x) <*> p <*> q;;

(***** NOW LET'S BEGIN ******)

let parse_char c s i = 
    if i >= String.length s then
        None
    else
        if s.[i] = c then 
            Some (s.[i],i+1)
        else
            None;;

let look_char s i = 
    if i >= String.length s then
        None
    else
        Some (s.[i],i);;

let parse_string str s i = 
    if i + String.length str >= String.length s then
        None
    else
        if String.sub s i (String.length str) = str then
            Some (str, i + String.length str)
        else
            None;;


let many_while p s i =  
    let j = ref i in 
    let max = String.length s in 
    while !j < max && p s.[!j] do
        incr j;
    done;
    Some (String.sub s i ((min !j max) - i), (min !j max));;

let some_while p s i =  
    match many_while p s i with
        | None -> None
        | Some (s,j) -> if j > i then Some (s,j) else None;;

let my_int_of_string s = 
    print_string s; print_newline ();
    int_of_string s;;

let parse_pred p s i = match look_char s i with
                        | None -> None
                        | Some (c,k) -> if p c then
                                            Some (c,i+1)
                                        else 
                                            None;;

let parse_lower s i = (pure string_of_char <*> parse_pred is_lower) s i;;
let parse_upper s i = (pure string_of_char <*> parse_pred is_upper) s i;;
    
let parse_int s i = (pure my_int_of_string <*> some_while is_numeric) s i;; 

let ign_space s i = (parse_char ' ' <|> pure ' ') s i;; 

let parse_space = parse_pred (fun c -> c = ' ');;


let parse_schar c s i   = (ign_space <*>> parse_char c <<*> ign_space) s i;; 
let parse_sstring c s i = (ign_space <*>> parse_string c <<*> ign_space) s i;; 

    
type ast = Par of ast * ast | Seq of ast * ast | Link of ((string * string) list) * ast | Id | Int of int | 
           VarI of string | VarO of string | Circ of string ;;


(**
 * Composing with a second argument 
 * that is possibly not present 
 *)
let compose_with f x y = match y with
    | None   -> x
    | Some e -> f x e;;

(**** THE GRAMMAR 
 *
 * Just as defined in the pdf, the rules are exactly the same, a
 * variable of the grammar is just converted into a 
 * recursive function that parses (because the grammar is LL)
 *
 *)
let rec parse_parallel s i = (* P *) 
    let compose x y = match y with
        | None   -> x
        | Some e -> Par (x,e)
    in
    (pure compose <*> parse_sequential <*> parse_parallel_rec) s i

and parse_sequential s i = (* S *) 
    let compose x y = match y with
        | None -> x 
        | Some e -> Seq (x,e)
    in 
    (pure compose <*> parse_base <*> parse_sequential_rec) s i 

and parse_parallel_rec s i = (* P' *)
    let compose _ s p = 
        Some (match p with
                | None -> s
                | Some e -> Par (s,e))
    in
    begin 
        (pure compose <*> parse_schar '|' <*> parse_sequential <*> parse_parallel_rec) <|>
        pure None 
    end s i 

and parse_sequential_rec s i =  (* S' *)
    let compose _ e s = 
        Some (match s with
                | None -> e 
                | Some k -> Seq (e,k))
    in
    begin 
        (pure compose <*> parse_schar '.' <*> parse_base <*> parse_sequential_rec) <|>
        pure None
    end s i

(* VAR *)
and parse_var_name s i = (pure (^) <*> parse_lower <*> many_while is_alphanum) s i

(* VARS *)
and parse_couple s i = 
    begin 
        pure (fun x y -> (x,y)) <*> 
        (parse_var_name <<*> parse_char ':') <*> 
        parse_var_name 
    end s i

and parse_vars s i =
    (pure append <*> parse_couple <*> (parse_space <*>> parse_vars_rec)) s i 

and parse_vars_rec s i = 
    begin 
        (pure append <*> parse_couple <*> (parse_space <*>> parse_vars_rec)) <|>
        (pure (fun _ -> []) <*> (ign_space <*>> parse_string "for" <<*> parse_space))
    end s i




(* CIRC *)
and parse_circ_name s i = (pure (^) <*> parse_upper <*> many_while is_alphanum) s i

(* E *)
and parse_base s i = 
    begin
        (pure (fun _ x _ -> x)        <*> parse_char '(' <*> parse_parallel <*> parse_char ')')               <|>
        (pure (fun x -> Int x)        <*> parse_int)                                                          <|>
        (pure (fun x -> Circ x)       <*> parse_circ_name)                                                    <|>
        (pure (fun x y -> Link (x,y)) <*> (parse_string "link"          <*>>
                                           parse_space                  <*>>
                                           parse_vars)                  <*>
                                           parse_parallel)                                                    <|>
        (pure (fun x -> VarO x)       <*> (ign_space <*>> parse_var_name <<*> parse_char ':' <<*> ign_space)) <|>
        (pure (fun x -> VarI x)       <*> (ign_space <*>> parse_char ':' <*>> parse_var_name <<*> ign_space)) <|>
        (pure (fun _ -> Id)           <*> parse_string "id")
    end s i ;;
    
let test = "1.(8|9)";;

let () = parse_parallel test 0; print_string "OK";;
