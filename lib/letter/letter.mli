module type Symbol = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

end

module type Letter = sig

  type symbol
  type t

  val compare : t -> t -> int
  val to_string : t -> string

  val epsilon : t
  val is_epsilon : t -> bool

  val get : t -> symbol option
  val symbol : symbol -> t

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t