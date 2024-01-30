module type S = sig

  type lt
  type t

  (* [equals r1 r2] checks if [r1] is the same as [r2] *)
  val equals : t -> t -> bool

  (* [from_string rstring alphabet] parses [rstring] into a t*)
  (* val from_string : string -> lt list -> t *)

  (* [simplify reg] simplfies [reg] as much as possible *)
  val simplify : t -> t

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t