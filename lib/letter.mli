(* For the alphabet *)
module type Symbol = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string
  (* val of_string : string -> t *)

end

module type Letter = sig

  type symbol

  include Symbol

  val epsilon : t
  val is_epsilon : t -> bool

  val get : t -> symbol option
  val symbol : symbol -> t

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t