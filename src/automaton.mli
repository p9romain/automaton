(* For the states *)
module type OrderedType = sig

  type t

  val compare : t -> t -> int

end

(* For the alphabet (with empty letter !) *)
module type OrderedEmptyType = sig

  type t

  val compare : t -> t -> int

  val empty : t
  val is_empty : t -> bool

end

module type S = sig

  type lt
  type st
  type t

  val empty : t
  val create : lt list -> t

  val add_state : t -> st -> t
  val add_trans : t -> st -> lt -> st -> t
  val add_start : t -> st -> t
  val add_end : t -> st -> t

  val remove_state : t -> st -> t
  val remove_one_trans : t -> st -> lt -> st -> t
  val remove_all_trans : t -> st -> lt -> t
  val remove_start : t -> st -> t
  val remove_end : t -> st -> t

  val is_deterministic : t -> bool
  (* val determinize : t -> t *)
  
  (* val check_word : t -> lt Seq.t -> bool *)

  (* val to_regex : t -> string *)
  (* val from_regex : string -> t *)

  (* val to_dot : t -> string -> unit *)

end

module Make (Lt : OrderedEmptyType) (St : OrderedType) : S with type lt = Lt.t and type st = St.t