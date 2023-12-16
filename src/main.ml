module Letter = struct
  
  type t = string

  let compare c1 c2 = String.compare c1 c2

  let to_string s = s

  let empty = ""
  let is_empty c = (String.compare c empty) = 0

end

module A = Automaton.Make(Letter)(Int)

let test = A.create ["a" ; "b"]
let test = A.(add_state (add_state (add_state (add_state test 4) 3) 2) 1)
let test = A.add_start test 1
let test = A.add_end test 4
let test = A.add_trans test 1 "a" 1
let test = A.add_trans test 1 "b" 1
let test = A.add_trans test 1 "a" 2
let test = A.add_trans test 2 "b" 3
let test = A.add_trans test 3 "a" 4
let () = A.to_dot test "test"