
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

let example5 = 
    (const "F" 1 1 ||| id 1) === j === f === (const "G" 1 1 ||| id 1);;

let example6 = 
    const "F" 1 2 === (id 1 ||| const "H" 1 1) === bindi "x" (bindo "y" (vari "x" ||| varo "y")) === const "G" 2 1 === id 1;; 

let example7 = 
    trace (trace ((id 1 ||| id 1 ||| const "F" 1 1) === (id 1 ||| bindi "x" (bindo "y" (vari "x" ||| varo "y"))) === (j ||| id 1) === (f ||| id 1)));;

let () = 
    compile "example1.dot" example1;
    compile "example2.dot" example2;
    compile "example3.dot" example3;
    compile "example4.dot" example4;
    compile "example5.dot" example5;
    compile "example6.dot" example6;
    compile "example7.dot" example7;;
