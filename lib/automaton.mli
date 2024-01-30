(* For the alphabet *)
module type Symbol = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string
  (* val of_string : string -> t *)

end

module type Letter = sig

  type symbol

  include Symbol

  val epsilon : t
  val is_epsilon : t -> bool

  val get : t -> symbol option
  val symbol : symbol -> t

end

module AddEpsilon(Sym : Symbol) : Letter with type symbol = Sym.t

module type S = sig

  type lt

  type t

  (* The empty automaton *)
  val empty : t
  (* [create alphabet] creates an empty automaton with the given alphabet [alphabet] *)
  val create : lt list -> t

  (* [add_state automaton state] adds [state] in [automaton]. 
     If it's already a state in [automaton], does nothing *)
  val add_state : t -> int -> t
  (* [add_states automaton states] adds several states in [automaton] *)
  val add_states : t -> int list -> t
  (* [add_trans automaton state1 letter state2] adds a transition labelled [letter] between [state1] and [state2] in [automaton].

     Raises : [state1] and [state2] must be [automaton]'s states.

     [letter] is either [None] for an epsilon-transition, either [Some l] otherwise.
     Raises : [l] must be in the [automaton]'s alphabet.

     If the transition is already in [automaton], does nothing *)
  val add_trans : t -> int -> lt -> int -> t
  (* [add_transitions automaton transitions] adds several transitions in [automaton] *)
  val add_transitions : t -> (int * lt * int) list -> t
  (* [add_start automaton state] sets [state] as a start state in [automaton].
     If it's already the case, does nothing

     Raise : [state] must be an [automaton]'s state *)
  val add_start : t -> int -> t
  (* [add_starts automaton states] sets several states as start states in [automaton] *)
  val add_starts : t -> int list -> t
  (* [add_end automaton state] sets [state] as an end state in [automaton].
     If it's already the case, does nothing

     Raise : [state] must be an [automaton]'s state *)
  val add_end : t -> int -> t
  (* [add_ends automaton states] sets several states as end states in [automaton] *)
  val add_ends : t -> int list -> t

  (* [remove_state automaton state] removes [state] from [automaton]'s states.
     If it isn't an [automaton]'s states, does nothing *)
  val remove_state : t -> int -> t
  (* [remove_states automaton states] removes several states in [automaton] *)
  val remove_states : t -> int list -> t
  (* [remove_trans automaton state1 letter state2] removes the given transitions from [automaton]'s transitions.
     If it isn't an [automaton]'s transitions, does nothing *)
  val remove_trans : t -> int -> lt -> int -> t
  (* [remove_all_trans_between automaton state1 state2] removes all transitions between [state1] and [state2] in [automaton] *)
  val remove_all_trans_between : t -> int -> int -> t
  (* [remove_start automaton state] unsets [state] as a start state in [automaton].
     If it's already the case, does nothing *)
  val remove_start : t -> int -> t
  (* [remove_starts automaton states] unsets several states as start state in [automaton] *)
  val remove_starts : t -> int list -> t
  (* [remove_end automaton state] unsets [state] as an end state in [automaton].
     If it's already the case, does nothing *)
  val remove_end : t -> int -> t
  (* [remove_ends automaton states] unsets several states as end state in [automaton] *)
  val remove_ends : t -> int list -> t

  (* [to_dot automaton file_name] creates a dot file named "[file_name].dot" representing [automaton] *)
  val to_dot : t -> string -> unit

  (* [is_deterministic automaton] checks if [automaton] is deterministic.

     A deterministic finite automaton (or DFA) follows these 3 rules :
     - Has one start state ;
     - Has no epsilon-transitions ;
     - For all states, there exists one and only one transition labelled by a same letter from it.
        For instance, if we have (1, "a", 2) and (1, "a", 3), then it isn't deterministic *)
  val is_deterministic : t -> bool

  (* [determinize automaton] returns an automaton which is the DFA version of [automaton].
     If [automaton] has n states, the DFA automaton has at most 2^n states.

     A deterministic finite automaton (or DFA) follows these 3 rules :
     - Has one start state ;
     - Has no epsilon-transitions ;
     - For all states, there exists one and only one transition labelled by a same letter from it.
        For instance, if we have (1, "a", 2) and (1, "a", 3), then it isn't deterministic *)
  val determinize : t -> t
  (* [get_rid_of_unreachable_states automaton] returns [automaton] without unreachable states *)
  (* val get_rid_of_unreachable_states : t -> t *)
  (* [minimize automaton] returns a minimized version of [automaton], the smallest possible 

     [automaton] must be a DFA without unreachable states *)
  (* val minimize : t -> t *)

  (* [check_word automaton word] checks if [word] is recognized by [automaton] *)
  val check_word : t -> lt list -> bool
  (* [to_regex_mcn_yama automaton] returns the regex representing [automaton] using the McNaughton-Yamada method. 
     The returned value might be unsimplified *)
  (* val to_regex_mcn_yama : t -> regexp *)
  (* [to_regex_brzo_mcc automaton] returns the regex representing [automaton] using the Brzozowski-McCluskey method,
      also know as the "state elimination method". 
     The returned value might be unsimplified *)
  (* val to_regex_brzo_mcc : t -> regexp *)
  (* [from_regex reg alphabet] creates a NFA (Non-deterministic Finite Automaton) recognizing [reg] *)
  (* val from_regex : regexp -> lt list -> t *)

end

module Make (Lt : Letter) : S with type lt = Lt.t