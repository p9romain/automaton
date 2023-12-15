(* (* For the states *)
module type OrderedType = sig

  type t

  val compare : t -> t -> int

end

(* For the alphabet (with empty letter !) *)
module type OrderedEmptyType = sig

  type t

  val compare : t -> t -> int
  val is_empty : t -> bool

end

module type S = sig

  type lt
  type st
  type t

  val empty : t

end

module Make (Lt : OrderedEmptyType) (St : OrderedType) : S with type lt = Lt.t and type st = St.t *)