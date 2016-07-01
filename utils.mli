val split : int -> 'a list -> 'a list * 'a list
val range : int -> int list
val replicate : int -> 'a -> 'a list
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val zipWith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val surround : string -> string -> string -> string
val permute_lignes : int -> int -> 'a array -> unit
val array_find : ('a -> bool) -> 'a array -> (int * 'a) option
val ( <|> ) : 'a option -> 'a option -> 'a option
val insertV :
  merge:('a -> 'a -> 'a) ->
  key:'b -> value:'a -> func:('b * 'a) list -> ('b * 'a) list
val imageV : elem:'a -> func:('a * 'b) list -> 'b option
val fiberV : elem:'a -> func:('b * 'a) list -> 'b list
val remove_duplicates : 'a list -> 'a list
val of_option : 'a option -> 'a
val list_of_options : 'a option list -> 'a list
val remove_list : 'a list -> 'a list -> 'a list
val correct_list : ('a * 'b) list -> ('a * 'b) list
val tests : (string * (unit -> unit)) list
