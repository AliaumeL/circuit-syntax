(***** 
 *
 * Lexing 
 *
 * Aliaume Lopez 
 *
 *)

(**
 *  Remove consecutive 
 *  spaces in a string 
 *)
let remove_spaces s = 
    let buf = Buffer.create (String.length s) in 
    let sp  = ref false in
    for i = 0 to String.length s - 1 do
        if s.[i] = ' ' || s.[i] = '\t' then
            begin
                (if not !sp then
                    Buffer.add_char buf ' ');
                sp := true
            end
        else
            begin 
                Buffer.add_char buf s.[i];
                sp := false
            end
    done;
    Buffer.contents buf;;



type res = SEQ | END | PAR;;

let try_read_op s n i =  
    if !i + 4 >= n then
        try
            match String.sub s !i 3 with
                | "END" -> i := !i + 3; Some END
                |   _   -> None
        with
            _ -> None

    else
        match String.sub s !i 4 with
            | "SEQ "  -> i := !i + 4; Some SEQ
            | "SEQ\t" -> i := !i + 4; Some SEQ
            | "SEQ\n" -> i := !i + 4; Some SEQ
            | "PAR "  -> i := !i + 4; Some PAR 
            | "PAR\t" -> i := !i + 4; Some PAR 
            | "PAR\n" -> i := !i + 4; Some PAR 
            | "END "  -> i := !i + 4; Some END 
            | "END\t" -> i := !i + 4; Some END 
            | "END\n" -> i := !i + 4; Some END
            | _       -> None;;

(**
 * Consturire une liste 
 * de tokens à partir d'une 
 * chaine de caractères 
 *)
let interpret_seq str =  
    let i = ref 0 in 
    let n = String.length str - 1 in
    let p = ref [] in 
    let w = ref false in 
    let buf = Buffer.create (String.length str) in 
    let write_newline () = 
        if !w = false then
            () 
        else
            begin
                (match !p with
                    | [] -> Buffer.add_char buf ' '
                    | SEQ :: q -> Buffer.add_string buf ").("
                    | PAR :: q -> Buffer.add_string buf ")|(");
                w := false
            end
    in
    while !i <= n do 
        match try_read_op str n i with 
            | Some SEQ -> write_newline (); p := SEQ :: !p; Buffer.add_char buf '('
            | Some PAR -> write_newline (); p := PAR :: !p; Buffer.add_char buf '('
            | Some END -> p := List.tl !p; Buffer.add_char buf ')'
            | None     -> 
                    if str.[!i] = '\n' then
                        (w := true; incr i)
                    else
                        (write_newline (); Buffer.add_char buf str.[!i]; incr i)
    done;
    Buffer.contents buf;;




let () = 
    let ic  = open_in "lines.txt" in 
    let buf = Buffer.create 80 in  
    Stream.of_channel ic |> Stream.iter (Buffer.add_char buf);
    let input = Buffer.contents buf in
    input |> interpret_seq |> remove_spaces |> print_string;; 

