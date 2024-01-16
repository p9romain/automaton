module StringS = struct

  type t = string

  let compare : t -> t -> int = String.compare
  let to_string (s : t) : string = s
  let of_string (s : string) : t = s

end

let string_to_string_list (s : string) : string list =
  List.map (fun c -> String.make 1 c) @@ List.of_seq @@ String.to_seq s

module A = Automaton.Make(StringS)

let auto = A.create ["a"; "b"]
let auto = A.add_states auto [1; 2; 3]
let auto = A.add_start auto 1
let auto = A.add_end auto 3
let auto = A.add_transitions auto [(1, Some "a", 1); (1, Some "b", 1); (1, Some "b", 2); (2, Some "b", 3)]
let () = A.to_dot auto "nfa"
let () = Printf.printf "%s\n" (if A.is_deterministic auto = true then "true" else "false")
let dfauto = A.determinize auto
let () = Printf.printf "%s\n" (if A.is_deterministic dfauto = true then "true" else "false")
let () = A.to_dot dfauto "dfa"