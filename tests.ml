
let runTests module_name functions =  
    print_string ("# RUNNING TESTS FOR " ^ module_name);
    print_newline ();
    List.iter (fun (s,f) -> print_string ("\t Testing : " ^ s ^ "..."); f(); print_string "[OK]\n") functions;;

let () = 
    runTests "utils.ml" Utils.tests;
    runTests "dot.ml" Dot.tests;;
    runTests "solver.ml" Solver.tests;
    runTests "typesystem.ml" Typesystem.tests;
    runTests "compiler.ml" Compiler.tests;;

