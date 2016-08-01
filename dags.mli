type nid = int
type label = VarI of string | VarO of string | Const of string
type port = nid * int option
val map_port : ('a -> 'b) -> 'a * 'c -> 'b * 'c
type 'a lidag = {
  iports : port list;
  oports : port list;
  nodes : (nid * int * int) list;
  edges : (port * port) list;
  labels : (nid * label) list;
  ibinders : nid list;
  obinders : nid list;
}
val empty_dag : 'a lidag
val anonym_link :
  start:'a -> finish:'b -> ('a * 'c option) * ('b * 'd option)
val mapids : (nid -> nid) -> 'a lidag -> 'b lidag
val maxid : 'a lidag -> nid
val sequence : first:'a lidag -> second:'b lidag -> 'c lidag
val parallel : top:'a lidag -> bottom:'b lidag -> 'c lidag
val ( >>= ) : 'a list -> ('a -> 'b list) -> 'b list
val link : vars:(string * string) list -> dag:'a lidag -> 'b lidag
val constant : name:string -> inputs:int -> outputs:int -> 'a lidag
val ivar : name:string -> 'a lidag
val ovar : name:string -> 'a lidag
val identity : number:int -> 'a lidag
val parallels : 'a lidag list -> 'a lidag
val sequences : 'a lidag list -> 'a lidag
val example_circuit : 'a lidag
val tightening1 :
  middle:'a lidag -> first:'b lidag -> last:'b lidag -> 'b lidag
val tightening2 :
  middle:'a lidag -> first:'a lidag -> last:'a lidag -> 'b lidag
