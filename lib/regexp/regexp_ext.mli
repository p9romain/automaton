module type S = sig

  type lt
  type t

  val letter : lt -> t
  val concat : t -> t -> t
  val union : t -> t -> t
  val star : t -> t
  val plus : t -> t
  val option : t -> t

  val simplify : t -> t

  val to_string : t -> string

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t