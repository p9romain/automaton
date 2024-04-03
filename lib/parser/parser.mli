(* the Letter module : just a string type *)
module S : Letter.Letter with type symbol = string

(* the Automaton module *)
module A : Automaton.S with type lt = S.t
                       and module R = Regexp.Make(S) 
                       and type regexp = Regexp.Make(S).t_simp

(* [parse_file file_name] parses the file named [file_name] to create the corresponding automaton.

    The file should looks like this, otherwise it will raise an exception.
    This is an example:

    (a|b)*bb
    6
    4,5
    0,a,0
    0,b,0
    ...
 *)
val parse_file : string -> string * A.t