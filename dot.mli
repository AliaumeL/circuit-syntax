(** The interface to compile dot expressions *)


(** The type of a dot graphic *)
type dot

(** The type of dot modifiers *)
type node_mod


(** Dot uid *)
type uid = int

(** Append the prelude and compiles to 
 * the string representation in dot language
 *)
val addPrelude : dot -> string 

val uid : unit -> uid 

(* val renderMods : node_mod -> string *)

val emptyMod : node_mod
val baseMod : node_mod 
val mod_shape  : string -> node_mod -> node_mod
val mod_label  : string -> node_mod -> node_mod 
val mod_style  : string -> node_mod -> node_mod 
val mod_width  : float -> node_mod -> node_mod 
val mod_height : float -> node_mod -> node_mod 
val mod_fixedsize : bool -> node_mod -> node_mod 
val inputsOutputs : string -> int -> int -> node_mod -> node_mod


val mkLabel : uid -> int option -> string -> dot 

val mkNode : node_mod -> uid * dot 


val link       : uid -> int option -> uid -> int option -> dot 
val shadowLink : uid -> int option -> uid -> int option -> dot 

val same_rankdir : uid list -> dot

val addDot : dot -> dot -> dot 
val addDots : dot list -> dot 

val tests : (string * (unit -> unit)) list 
