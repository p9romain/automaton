(* For the states *)
module type State = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

end

(* For the alphabet (with empty letter !) *)
module type Letter = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

end

module type S = sig

  type lt
  type t

  val empty : t
  val create : lt list -> t

  val add_state : t -> int -> t
  val add_states : t -> int list -> t
  val add_trans : t -> int -> lt option -> int -> t
  val add_transitions : t -> (int * lt option * int) list -> t
  val add_start : t -> int -> t
  val add_starts : t -> int list -> t
  val add_end : t -> int -> t
  val add_ends : t -> int list -> t

  val remove_state : t -> int -> t
  val remove_states : t -> int list -> t
  val remove_trans : t -> int -> lt option -> int -> t
  val remove_all_trans_between : t -> int -> int -> t
  val remove_start : t -> int -> t
  val remove_starts : t -> int list -> t
  val remove_end : t -> int -> t
  val remove_ends : t -> int list -> t

  val replace_trans : t -> int -> lt option -> int -> t

  val is_deterministic : t -> bool

  (* val determinize : t -> t *)
  (* val get_rid_of_unreachable_set : t -> t *)
  (* val minimize : t -> t *)

  val check_word : t -> lt list -> bool

  (* val to_regex : t -> string *)
  (* val from_regex : string -> t *)

  val to_dot : t -> string -> unit

end

module Make (Lt : Letter) : S with type lt = Lt.t