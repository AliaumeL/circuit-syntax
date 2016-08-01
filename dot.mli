module SM :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type dot = string
type node_mod = string SM.t
type uid = int
val addPrelude : string -> string
val count : int ref
val uid : unit -> int
val renderMods : string SM.t -> string
val mod_shape : 'a -> 'a SM.t -> 'a SM.t
val mod_color : 'a -> 'a SM.t -> 'a SM.t
val mod_label : string -> string SM.t -> string SM.t
val mod_style : string -> string SM.t -> string SM.t
val mod_width : float -> string SM.t -> string SM.t
val mod_height : float -> string SM.t -> string SM.t
val mod_fixedsize : bool -> string SM.t -> string SM.t
val mkLabel : int -> int option -> string -> string
val mkNode : int -> string SM.t -> string
val emptyMod : 'a SM.t
val baseMod : string SM.t
val inputsOutputs : string -> int -> int -> string SM.t -> string SM.t
val mkLink : int -> int option -> int -> int option -> string
val rank_group : string -> int list -> string
val addDot : string -> string -> string
val addDots : string list -> string
val tests : 'a list
