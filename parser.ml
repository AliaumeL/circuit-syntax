(***
 *
 * Parser 
 *
 * Aliaume Lopez
 *
 *
 * This file contains 
 * 1. small functions to deal with characters 
 * 2. a small parser combinator type over strings 
 * 3. minimal definition of basic parser 
 * 4. parser of constants of the language
 * 5. parser directly derived from the grammar in the PDF
 *    (set of mutually recursive functions over strings)
 *
 * The goal of this file is to go from a « good » (well-formated)
 * string to an AST (circ) 
 * 
 * TODO
 *
 * a) Don't use the (===) and (|||) combinators 
 * directly but rather use accumulating lists and 
 * a function that generates a well balanced tree 
 * 
 *)

open Ast;;

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
 * all thing simpler to write
 *
 * TODO: use ocamllex / ocamlyacc instead ... 
 * *)
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



(***** THE HELPER FUNCTION 
 * to assing circuits to specific circuit names 
 *
 * TODO 
 * this function should look inside a file 
 * containing the mappings ... 
 *)
let circuit_of_name = function
    | "F"    -> const "F"    1 1
    | "G"    -> const "G"    1 1
    | "H"    -> const "H"    2 2
    | "PMOS" -> const "PMOS" 2 1
    | "NMOS" -> const "NMOS" 2 1
    | "HIGH" -> const "HIGH" 0 1
    | "LOW"  -> const "LOW"  0 1
    | "MUX"  -> const "MUX"  3 1
    | "BOT"  -> const "BOT"  0 1
    | "WAIT" -> const "WAIT" 1 1
    | "DISC" -> const "DISC" 1 0
    | "FORK" -> const "FORK" 1 2
    | "JOIN" -> const "JOIN" 2 1
    |  x     -> const x      1 1;;

(**** THE GRAMMAR 
 *
 * Just as defined in the pdf, the rules are exactly the same, a
 * variable of the grammar is just converted into a 
 * recursive function that parses (because the grammar is LL)
 *
 *)
let rec parse_parallel s i = (* P *) 
    begin 
        pure (|||) <*> parse_sequential <*> parse_parallel_rec
    end s i

and parse_sequential s i = (* S *) 
    begin
        pure (===) <*> parse_base <*> parse_sequential_rec
    end s i 

and parse_parallel_rec s i = (* P' *)
    begin 
        (pure (|||) <*> (parse_schar '|' <*>> parse_sequential) <*> parse_parallel_rec) <|>
        pure empty 
    end s i 

and parse_sequential_rec s i =  (* S' *)
    begin 
        (pure (===) <*> (parse_schar '.' <*>> parse_base) <*> parse_sequential_rec) <|>
        pure idpoly 
    end s i

(* VAR *)
and parse_var_name s i = 
    begin
        pure (^) <*> parse_lower <*> many_while is_alphanum 
    end s i

(* VARS *)
and parse_couple s i = 
    begin 
        pure (fun x y -> (x,y)) <*> 
        (parse_var_name <<*> parse_char ':') <*> 
        parse_var_name 
    end s i

and parse_vars s i =
    begin 
        pure append <*> parse_couple <*> (parse_space <*>> parse_vars_rec)
    end s i 

and parse_vars_rec s i = 
    begin 
        (pure append <*> parse_couple <*> (parse_space <*>> parse_vars_rec)) <|>
        (pure (fun _ -> []) <*> (ign_space <*>> parse_string "for" <<*> parse_space))
    end s i


(* CIRC *)
and parse_circ_name s i = 
    begin
        pure (^) <*> parse_upper <*> many_while is_alphanum
    end s i

(* E *)
and parse_base s i = 
    begin
        (parse_char '('                <*>> parse_parallel       
                                      <<*>  parse_char ')')                  
        <|>
        (pure id                       <*>  parse_int)                      
        <|>
        (pure circuit_of_name          <*>  parse_circ_name)
        <|>
        (pure links                    <*>  (parse_string "link" 
                                       <*>> parse_space 
                                       <*>> parse_vars)  
                                       <*>  parse_parallel)               
        <|>
        (pure varo                     <*>  (ign_space    
                                       <*>> parse_var_name
                                      <<*>  parse_char ':'
                                      <<*>  ign_space))                  
        <|>
        (pure vari                     <*>  (ign_space    
                                       <*>> parse_char ':' 
                                       <*>> parse_var_name 
                                      <<*>  ign_space))                 
        <|>
        (pure idpoly                  <<*> parse_string "_")
    end s i ;;
    
let parse_ast s = match parse_parallel s 0 with 
    | None       -> failwith "parsing failed"
    | Some (x,k) -> x;; 


(******* TESTS DE PARSING ********)

let test1 = "F";;
let test2 = "F.G";;
let test3 = "F|G";;
let test4 = "F.(G|H)";;
let test5 = "(F.G)|H";;
let test6 = "F . G | H";;
let test7 = "link a:b for F . G . H";;
let test8 = "F . link a:b c:d for :a | b:";;

let tests = 
    [ ("Constant" , test1); 
      ("Sequence" , test2); 
      ("Parallel" , test3); 
      ("Mixed (1)", test4); 
      ("Mixed (2)", test5); 
      ("Spaces"   , test6); 
      ("Link (1)" , test7); 
      ("Link (2)" , test8) ] 
    |> List.map (fun (n,t) -> (n, fun () -> parse_ast t));;
