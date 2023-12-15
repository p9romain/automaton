module Letter = struct
  
  type t = string

  let compare c1 c2 = String.compare c1 c2

  let empty = ""
  let is_empty c = (String.compare c empty) = 0

end

module A = Automaton.Make(Letter)(Int)

let test = A.empty