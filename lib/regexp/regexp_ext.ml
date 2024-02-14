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

module Make (Lt : Letter.Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t
  type t =
    | Letter of lt
    | Concat of t list
    | Union of t list
    | Star of t
    | Plus of t
    | Option of t

  let letter (l : lt) : t =
    Letter l

  let concat (r1 : t) 
             (r2 : t) : t =
    Concat [r1; r2]

  let union (r1 : t) 
            (r2 : t) : t =
    Union [r1; r2]

  let star (r : t) : t =
    Star r

  let plus (r : t) : t =
    Plus r

  let option (r : t) : t =
    Option r



  let simplify (r : t) : t =
    r



  let rec to_string (r : t) : string =
    match r with
    | Letter l ->
      Lt.to_string l
    | Concat l ->
      String.concat "" @@ List.map to_string l
    | Union l ->
      "(" ^ (String.concat "|" @@ List.map to_string l) ^ ")"
    | Star r ->
      "(" ^ to_string r ^ ")*"
    | Plus r ->
      "(" ^ to_string r ^ ")+"
    | Option r ->
      "(" ^ to_string r ^ ")?"

end