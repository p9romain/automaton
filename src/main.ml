module StringS = struct

  type symbol = string
  type t = string

  let compare : t -> t -> int = String.compare
  let to_string (s : t) : string = s

  let epsilon = ""
  let is_epsilon (s : t) : bool = 
    s = epsilon

  let get (s : t) : symbol option =
    match s with
    | "" -> None
    | _ -> Some s
  let symbol (s : symbol) : t = s

end

let string_to_string_list (s : string) : string list =
  List.map (fun c -> String.make 1 c) @@ List.of_seq @@ String.to_seq s

module A = Automaton.Make(StringS)

let auto = A.create @@ List.map StringS.symbol ["a"; "b"]
let auto = A.add_states auto [1; 2; 3]
let auto = A.add_start auto 1
let auto = A.add_end auto 3
let auto = A.add_transitions auto [(1, StringS.symbol "a", 1); (1, StringS.symbol "b", 1); (1, StringS.symbol "b", 2); (2, StringS.symbol "b", 3)]
let () = A.to_dot auto "nfa"
let () = assert(not @@ A.is_deterministic auto)
let dfauto = A.determinize auto
let () = assert(A.is_deterministic dfauto)
let () = A.to_dot dfauto "dfa"

let () = assert(not @@ A.check_word auto @@ List.map StringS.symbol @@ string_to_string_list "a")
let () = assert(A.check_word auto @@ List.map StringS.symbol @@ string_to_string_list "bb")
let () = assert(not @@ A.check_word auto @@ List.map StringS.symbol @@ string_to_string_list "")
let () = assert(not @@ A.check_word auto @@ List.map StringS.symbol @@ string_to_string_list "aabbaaba")
let () = assert(A.check_word auto @@ List.map StringS.symbol @@ string_to_string_list "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

let () = assert(not @@ A.check_word dfauto @@ List.map StringS.symbol @@ string_to_string_list "a")
let () = assert(A.check_word dfauto @@ List.map StringS.symbol @@ string_to_string_list "bb")
let () = assert(not @@ A.check_word dfauto @@ List.map StringS.symbol @@ string_to_string_list "")
let () = assert(not @@ A.check_word dfauto @@ List.map StringS.symbol @@ string_to_string_list "aabbaaba")
let () = assert(A.check_word dfauto @@ List.map StringS.symbol @@ string_to_string_list "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

module Regex = Regexp.Make(StringS)

let r = Regex.(simp_to_ext @@ concat (letter "a") @@ concat (letter "a") @@ union (letter "b") @@ concat (union (union (letter "b") @@ concat (union (letter "a") @@ letter "b") @@ letter "a") @@ letter "b") @@ letter "a")
let () = Printf.printf "%s\n" @@ Regex.(to_string r)
let () = Printf.printf "%s\n" @@ Regex.(to_string @@ simplify r)