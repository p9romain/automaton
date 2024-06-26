module type Symbol = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

  val sample : t

end

module type Letter = sig

  type symbol
  type t

  val compare : t -> t -> int
  val to_string : t -> string

  val epsilon : t
  val is_epsilon : t -> bool

  val sample : t

  val get : t -> symbol option
  val symbol : symbol -> t

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t = struct

  type symbol = Sym.t
  type t = symbol option

  let compare (letter : t) 
              (letter' : t) : int =
    match letter, letter' with
    | Some letter, Some letter' -> Sym.compare letter letter'
    | None, Some _ -> -1
    | None, None -> 0
    | Some _, None -> 1 

  let to_string (letter : t) : string =
    match letter with
    | None -> "ε"
    | Some letter -> Sym.to_string letter

  let epsilon = None
  let is_epsilon (letter : t) : bool =
    match letter with
    | None -> true 
    | _ -> false

  let get (letter : t) : symbol option = letter
  let symbol (s : symbol) : t = Some s

  let sample = symbol Sym.sample
  
end