module StringS = struct

  type t = string

  let compare = String.compare
  let to_string s = s

end

module A = Automaton.Make(StringS)(Int)

let auto = A.create ["a"; "b"]
let auto = List.fold_left (fun acc x -> A.add_state auto x) auto [1; 2; 3; 4]
(* let auto = List.fold_left (fun (s1, l, s2) -> A.add_trans auto s1 l s2) auto [(); (); (); (); (); (); (); ();] *)
