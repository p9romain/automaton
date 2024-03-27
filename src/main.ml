module StringS = struct

  type symbol = string
  type t = string

  let compare : t -> t -> int = String.compare
  let to_string (s : t) : string = s

  let epsilon = "EPS"
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
let nfa = A.add_states nfa [0; 5; 3]
let nfa = A.add_start nfa 0
let nfa = A.add_end nfa 3
let nfa = A.add_transitions nfa StringS.(
    [
      (0, symbol "a", 0); 
      (0, symbol "b", 0); 
      (0, symbol "b", 5); 
      (5, symbol "b", 3)
    ]
  )
let () = A.to_dot nfa "nfa"
let () = assert(not @@ A.is_deterministic nfa)
let dfa = A.determinize nfa
let () = assert(A.is_deterministic dfa)
let () = A.to_dot dfa "dfa"

let check_nfa (s : string) = A.check_word nfa @@ List.map StringS.symbol @@ string_to_string_list s
let check_dfa (s : string) = A.check_word dfa @@ List.map StringS.symbol @@ string_to_string_list s

let () = assert(not @@ check_nfa "a")
let () = assert(check_nfa "bb")
let () = assert(not @@ check_nfa "")
let () = assert(not @@ check_nfa "aabbaaba")
let () = assert(check_nfa "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

let () = assert(not @@ check_dfa "a")
let () = assert(check_dfa "bb")
let () = assert(not @@ check_dfa "")
let () = assert(not @@ check_dfa "aabbaaba")
let () = assert(check_dfa "abababaaabbababbaababbaaababaabbbababbabbababababbababb")

module Regex = A.R

let regexp_of_nfa = A.to_regex_my nfa
let regexp_of_dfa = A.to_regex_my dfa

let () = Printf.printf "NFA : %s\n" @@ Regex.(to_string @@ simp_to_ext regexp_of_nfa)
let () = Printf.printf "DFA : %s\n" @@ Regex.(to_string @@ simp_to_ext regexp_of_dfa)