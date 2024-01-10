module StringS = struct

  type t = string

  let compare = String.compare
  let to_string s = s

end

module A = Automaton.Make(StringS)(Int)