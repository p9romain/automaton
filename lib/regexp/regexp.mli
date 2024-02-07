module type S = sig

  type lt
  type t

  (* [letter l] creates a regex from the letter [l] *)
  val letter : lt -> t
  (* [concat r1 r2] creates a regex by concatenating [r1] and [r2] *)
  val concat : t -> t -> t
  (* [concat r1 r2] creates a regex by uniting [r1] and [r2] *)
  val union : t -> t -> t
  (* [star r] creates a star regex from [r] *)
  val star : t -> t

  (* [equals r1 r2] checks if [r1] is the same as [r2] *)
  val equals : t -> t -> bool

  (* [normalize reg] normalizes [reg]
    For example, normalize Concat(r1, Concat(r2, r3)) -> Concat(Concat(r1, r2), r3)
              or normalize Union(r1, Union(r2, r3)) -> Uniont(Union(r1, r2), r3)
  *)
  (* val normalize : t -> t *)
  (* [simplify reg] simplifies [reg] as much as possible *)
  val simplify : t -> t

  (* [to_string reg] converts [reg] to a string (e.g. for debuging)
     There is too much parenthesis to avoid ambiguities
  *)
  val to_string : t -> string
  (* [from_string rstring alphabet] parses [rstring] into a regex*)
  (* val from_string : string -> lt list -> t *)

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t