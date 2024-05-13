let string_to_symbol_list (s : string) : Parser.S.t list =
  List.map Parser.S.symbol @@ List.map (fun c -> String.make 1 c) @@ List.of_seq @@ String.to_seq s

module A = Parser.A
module R = A.R

(* (a|b)*bb *)
let (regexp, nfa) = Parser.parse_file "src/nfa.txt"
let () = assert(not @@ A.is_deterministic nfa)
let dfa = A.determinize nfa
let () = assert(A.is_deterministic dfa)

let () = A.to_dot nfa "src/nfa"
let () = A.to_dot dfa "src/dfa"

let check_nfa (s : string) = A.check_word nfa @@ string_to_symbol_list s
let check_dfa (s : string) = A.check_word dfa @@ string_to_symbol_list s

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

let regexp_of_nfa = A.to_regex_my nfa
let regexp_of_dfa = A.to_regex_my dfa

let () = Printf.printf "Regexp from file %s\n" regexp
let () = Printf.printf "NFA : %s\n" @@ R.(to_string @@ simp_to_ext regexp_of_nfa)
let () = Printf.printf "NFA (simplified) : %s\n" @@ R.(to_string @@ simplify @@ simp_to_ext regexp_of_nfa)
let () = Printf.printf "DFA : %s\n" @@ R.(to_string @@ simp_to_ext regexp_of_dfa)
let () = Printf.printf "DFA (simplified) : %s\n" @@ R.(to_string @@ simplify @@ simp_to_ext regexp_of_dfa)

(* let () = Printf.printf "\n\n\n"

let () = 
  for i = 0 to 8 do
    let (regexp, nfa) = Parser.parse_file (Printf.sprintf "test/auto00%d.txt" i) in
    let () = A.to_dot nfa (Printf.sprintf "test/nfa_%d" i) in
    let dfa = A.determinize nfa in
    let () = A.to_dot dfa (Printf.sprintf "test/dfa_%d" i) in
    let regexp_of_nfa = A.to_regex_my nfa in
    let regexp_of_dfa = A.to_regex_my dfa in
    let () = Printf.printf "Regexp from file \"%s\"\n" regexp in
    let () = Printf.printf "NFA              : %s\n" @@ R.(to_string @@ simp_to_ext regexp_of_nfa) in
    let () = Printf.printf "NFA (simplified) : %s\n" @@ R.(to_string @@ simplify @@ simp_to_ext regexp_of_nfa) in
    let () = Printf.printf "DFA              : %s\n" @@ R.(to_string @@ simp_to_ext regexp_of_dfa) in
    Printf.printf "DFA (simplified) : %s\n\n" @@ R.(to_string @@ simplify @@ simp_to_ext regexp_of_dfa)
  done *)