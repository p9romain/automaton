module type S = sig

  (* The letter type *)
  type lt
  (* A simple implementation of a regex *)
  type t_simp
  (* An extended version of the regex (with the option and plus) *)
  type t_ext

  (* [letter l] creates a regex with the letter [l] *)
  val letter : lt -> t_simp
  (* [concat r1 r2] creates a regex by concatenating [r1] with [r2] *)
  val concat : t_simp -> t_simp -> t_simp
  (* [union r1 r2] creates a regex by uniting [r1] with [r2] *)
  val union : t_simp -> t_simp -> t_simp
  (* [star r] creates a regex with the Kleene star *)
  val star : t_simp -> t_simp

  (* [to_string r] cast the regex [r] into a string *)
  val to_string : t_ext -> string

  (* [simp_to_ext r] extends the simple regex [r], in order to simplify or print it *)
  val simp_to_ext : t_simp -> t_ext

  (* [simplify r] normalizes and simplfies the regex [r] as much as possible *)
  val simplify : t_ext -> t_ext

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t