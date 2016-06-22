
open Typesystem;;
open Utils;;
open Ast;;
open Compiler;;

let example1 = 
    links [("c", "f"); 
           ("c", "a");
           ("g", "d");
           ("b", "d");
           ("e", "h") ]
          ( ((vari "e" === varo "f") ||| varo "a") === vari "c" === const "F" 1 1 === varo "d" === ((vari "g" === varo "h") ||| vari "b"));;

let example2 = 
    links [("x","y")] (const "F" 1 1 === varo "y" === (vari "x" ||| vari "x"));;


let example3 = 
    links [("x","y"); ("z","y"); ("x","t"); ("z","t")] ( (varo "y" ||| varo "t") === (vari "x" ||| vari "z"));;

let example4 = 
    links [("x","y")] ((varo "y" ||| varo "y") === (vari "x" ||| vari "x"));;

let () = 
    compile "example1.dot" example1;
    compile "example2.dot" example2;
    compile "example3.dot" example3;
    compile "example4.dot" example4;;
