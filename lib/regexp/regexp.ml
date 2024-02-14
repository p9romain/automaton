module type S = sig

  type lt
  type t

  val letter : lt -> t
  val concat : t -> t -> t
  val union : t -> t -> t
  val star : t -> t

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t
  type t =
    | Letter of lt
    | Concat of t * t
    | Union of t * t
    | Star of t

  let letter (l : lt) : t =
    Letter l

  let concat (r1 : t) 
             (r2 : t) : t =
    Concat (r1, r2)

  let union (r1 : t) 
            (r2 : t) : t =
    Union (r1, r2)

  let star (r : t) : t =
    Star r
end