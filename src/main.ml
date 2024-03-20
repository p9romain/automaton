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

(* (a|b)*bb *)
let nfa = A.create @@ List.map StringS.symbol ["a"; "b"]
let nfa = A.add_states nfa [1; 2; 3]
let nfa = A.add_start nfa 1
let nfa = A.add_end nfa 3
let nfa = A.add_transitions nfa [(1, StringS.symbol "a", 1); (1, StringS.symbol "b", 1); (1, StringS.symbol "b", 2); (2, StringS.symbol "b", 3)]
let () = A.to_dot nfa "nfa"
let () = assert(not @@ A.is_deterministic nfa)
let dfa = A.determinize nfa
let () = assert(A.is_deterministic dfa)
let () = A.to_dot dfa "dfa"

let () = assert(not @@ A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list "a")
let () = assert(A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list "bb")
let () = assert(not @@ A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list "")
let () = assert(not @@ A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list "aabbaaba")
let () = assert(A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

let () = assert(not @@ A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list "a")
let () = assert(A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list "bb")
let () = assert(not @@ A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list "")
let () = assert(not @@ A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list "aabbaaba")
let () = assert(A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

module Regex = A.R

let regexp_of_nfa = A.to_regex_my nfa
(* let regexp_of_dfa = A.to_regex_my dfa *)

let () = Printf.printf "NFA : %s" @@ Regex.(to_string @@ simp_to_ext regexp_of_nfa)
(* let () = Printf.printf "DFA : %s" @@ Regex.(to_string @@ simp_to_ext regexp_of_dfa) *)