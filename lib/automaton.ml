module type Letter = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t

end

module type S = sig

  type lt

  type t

  val empty : t
  val create : lt list -> t

  val add_state : t -> int -> t
  val add_states : t -> int list -> t
  val add_trans : t -> int -> lt option -> int -> t
  val add_transitions : t -> (int * lt option * int) list -> t
  val add_start : t -> int -> t
  val add_starts : t -> int list -> t
  val add_end : t -> int -> t
  val add_ends : t -> int list -> t

  val remove_state : t -> int -> t
  val remove_states : t -> int list -> t
  val remove_trans : t -> int -> lt option -> int -> t
  val remove_all_trans_between : t -> int -> int -> t
  val remove_start : t -> int -> t
  val remove_starts : t -> int list -> t
  val remove_end : t -> int -> t
  val remove_ends : t -> int list -> t

  val to_dot : t -> string -> unit

  val is_deterministic : t -> bool

  val determinize : t -> t
  val get_rid_of_unreachable_states : t -> t
  (* val minimize : t -> t *)
  
  (* val check_word : t -> lt list -> bool *)

  (* val to_regex : t -> string *)
  (* val from_regex : string -> lt list -> t *)

end

module Make (Lt : Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t
  module Letter = struct

    include Lt

    (* For eps transitions (None) *)
    let compare_opt (letter : t option) 
                    (letter' : t option) : int =
      match letter, letter' with
      | Some letter, Some letter' -> compare letter letter'
      | None, Some _ -> -1
      | None, None -> 0
      | Some _, None -> 1 

  end
  module LetterSet = Set.Make(Letter)
  type alphabet = LetterSet.t


  type state = int
  module State = struct

    type t = state

    let compare : t -> t -> int = Int.compare

  end
  module StateSet = Set.Make(State)
  type states = StateSet.t
  (* useful for determinize *)
  module StateSetSet = Set.Make(StateSet)
  type states_set = StateSetSet.t
  module StateSetHash = struct

    type t = states

    let equal : states -> states -> bool = StateSet.equal

    let hash (states : t) : int =
      StateSet.fold (
        fun (state : state) 
            (acc : int) : int ->
          Hashtbl.hash @@ state lxor acc
      ) states 0
     
  end
  module StateSetHashtbl = Hashtbl.Make(StateSetHash)


  type trans = state * lt option * state
  module Trans = struct

    type t = trans

    let compare (state1, letter, state2 : t) 
                (state1', letter', state2' : t) : int =
      let c1 = State.compare state1 state1' in
      match c1 with
      | 0 ->
        begin
          let c2 = Letter.compare_opt letter letter' in
          match c2 with
          | 0 -> State.compare state2 state2'
          | _ -> c2
        end
      | _ -> c1

  end
  module TransSet = Set.Make(Trans)
  type transitions = TransSet.t


  type t = { 
              alphabet : alphabet ; 
              states : states ; 
              starts : states ; 
              trans : transitions ; 
              ends : states ;
           }

  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)

  let get_transition_from (automaton : t) 
                          (state : state) : transitions =
    TransSet.filter (
      fun (state', _, _) -> 
        State.compare state state' = 0
    ) 
    automaton.trans

  let get_transition_between (automaton : t) 
                             (state1 : state)
                             (state2 : state) : transitions =
    TransSet.filter (
      fun (state1', _, state2') -> 
        State.compare state1 state1' = 0
        && State.compare state2 state2' = 0
    ) 
    automaton.trans

  let get_transition_to (automaton : t) 
                        (state : state) : transitions =
    TransSet.filter (
      fun (_, _, state') -> 
        State.compare state state' = 0
    ) 
    automaton.trans              


  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let empty : t = { 
                    alphabet = LetterSet.empty ; 
                    states = StateSet.empty ; 
                    starts = StateSet.empty ; 
                    trans = TransSet.empty ;
                    ends = StateSet.empty ; 
                  }

  let create (alphabet : lt list) : t = 
    { empty with alphabet = LetterSet.of_list alphabet }



  let add_state (automaton : t) 
                (state : state) : t =
    { automaton with states = StateSet.add state automaton.states }

  let add_states (automaton : t) 
                 (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t ->
        add_state acc state
    ) 
    automaton states

  let add_trans (automaton : t) 
                (state1 : state) 
                (letter : lt option)
                (state2 : state) : t =
    match StateSet.find_opt state1 automaton.states, StateSet.find_opt state2 automaton.states with
    | Some _, Some _ ->
      begin
        let trans = (state1, letter, state2) in
        match letter with
        | None -> 
          { automaton with trans = TransSet.add trans automaton.trans }
        | Some letter ->
          begin
            match LetterSet.find_opt letter automaton.alphabet with
            | Some _ -> 
              { automaton with trans = TransSet.add trans automaton.trans }
            | None -> failwith "given letter isn't in the automaton's alphabet"
          end
      end
    | _ -> failwith "both given states must be automaton's states"

  let add_transitions (automaton : t) 
                      (transitions : trans list) : t =
    List.fold_left (
      fun (acc : t) 
          (state1, letter, state2 : trans) : t -> 
        add_trans acc state1 letter state2
    ) 
    automaton transitions

  let add_start (automaton : t) 
                (state : state) : t =
    match StateSet.find_opt state automaton.states with
    | Some _ ->
      { automaton with starts = StateSet.add state automaton.starts }
    | None -> failwith "given state must be an automaton's state"

  let add_starts (automaton : t) 
                 (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        add_start acc state
    ) 
    automaton states

  let add_end (automaton : t) 
              (state : state) : t =
    match StateSet.find_opt state automaton.states with
    | Some _ ->
      { automaton with ends = StateSet.add state automaton.ends }
    | None -> failwith "given state must be an automaton's state"

  let add_ends (automaton : t) 
               (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        add_end acc state
    ) 
    automaton states



  let remove_state (automaton : t) 
                   (state : state) : t =
    { automaton with states = StateSet.remove state automaton.states }

  let remove_states (automaton : t) 
                    (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        remove_state acc state
    ) 
    automaton states

  let remove_trans (automaton : t) 
                   (state1 : state) 
                   (letter : lt option) 
                   (state2 : state) : t =
    { automaton with trans = TransSet.remove (state1, letter, state2) automaton.trans }

  let remove_all_trans_between (automaton : t) 
                               (state1 : state) 
                               (state2 : state) : t =
    let transitions = TransSet.filter (
      fun (s1, _, s2 : trans) : bool -> 
        State.compare state1 s1 <> 0 || State.compare state2 s2 <> 0 
      ) 
      automaton.trans
    in
    { automaton with trans = transitions }

  let remove_start (automaton : t) 
                   (state : state) : t =
    { automaton with starts = StateSet.remove state automaton.starts }

  let remove_starts (automaton : t) 
                    (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        remove_start acc state
    ) 
    automaton states

  let remove_end (automaton : t) 
                 (state : state) : t =
    { automaton with ends = StateSet.remove state automaton.ends }

  let remove_ends (automaton : t) 
                  (states : state list) : t =
    List.fold_left (
      fun (acc : t) 
          (state : state) : t -> 
        remove_end acc state
    ) 
    automaton states



  let to_dot (automaton : t)
             (file_name : string) : unit =
    let file = open_out (file_name ^ ".dot") in
    Printf.fprintf file "digraph automaton\n{\n" ;
    let i = ref 0 in
    StateSet.iter ( 
      fun (state : state) : unit -> 
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ [label= \"\", shape=none,height=.0,width=.0] ;\n" !i ;
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ -> %s ;\n" !i @@ string_of_int state ;
        i := !i + 1
    ) 
    automaton.starts ;
    StateSet.iter ( 
      fun (state : state) : unit ->
        Printf.fprintf file "  %s [peripheries=2] ;\n" @@ string_of_int state 
    ) 
    automaton.ends ;
    StateSet.iter (
      fun (state1 : state) : unit ->
        StateSet.iter (
          fun (state2 : state) : unit ->
            let letters = List.map (
              fun (_, letter, _ : trans) : string ->
                match letter with
                | None -> "ε"
                | Some letter -> Lt.to_string letter
            ) 
              @@ TransSet.to_list 
              @@ get_transition_between automaton state1 state2 
            in
            let letter = String.concat ", " letters in
            if letter <> "" then 
              Printf.fprintf file "  %s -> %s [label=\"%s\"] ;\n" (string_of_int state1) (string_of_int state2) letter
          )
          automaton.states
      ) 
      automaton.states ;
    Printf.fprintf file "}" ;
    close_out file



  let is_deterministic (automaton : t) : bool =
    StateSet.cardinal automaton.starts = 1 (* Only one start state *)
    (* No epsilon transition or same letter transition from a state *)
    && 
    StateSet.for_all (
      fun state ->
        (* Get all transitions from the state *)
        let transitions = get_transition_from automaton state in
        (* Sort all the letters to see if there is more than once a letter *)
        let letters = List.sort Letter.compare_opt @@ TransSet.fold (
          fun (_, letter, _ : trans)
              (acc : lt option list) : lt option list ->
            letter :: acc
        ) 
        transitions [] 
        in
        (* Check *)
        let rec check (letters : lt option list) 
                      (old_elt : lt option) : bool =
          match letters with
          | [] -> true
          | elt :: letters ->
            begin
              match old_elt, elt with
              (* epsilon transition *)
              | _, None -> false
              (* first letter *)
              | None, _ ->
                check letters elt
              (* else : comparing previous letter checked with current letter*)
              | Some letter, Some letter' ->
                if Letter.compare letter letter' = 0 then
                  false
                else
                  check letters elt
            end
        in
        check letters None
    )
    automaton.states



  let determinize (automaton : t) : t =
    (* Only one start state (state nb = min-1)*)
    let start_state = (+) (-1) 
      @@ StateSet.fold min automaton.states 
      @@ StateSet.choose automaton.states 
    in
    (* Get its state name *)
    let automaton = add_state automaton start_state in
    (* Link the new start state with previous start states *)
    let automaton = StateSet.fold (
      fun (state : state)
          (automaton : t) : t ->
        add_trans automaton start_state None state
    ) 
    automaton.starts automaton 
    in
    (* Removes old start states *)
    let automaton = { automaton with starts = StateSet.empty } in
    (* Add new start state *)
    let automaton = add_start automaton start_state in
    (* To get rid of eps transitions *)
    let eps_closure = Hashtbl.create 16 in
    let () = StateSet.iter (
      fun state ->
        let rec get_accessible_states_with_eps_trans (state1 : state) 
                                                     (acc, already_done : states * states) : states =
          (* all transitions *)
          let transitions = get_transition_from automaton state1 in
          (* keep only the eps transitions *)
          let transitions = TransSet.filter (
            fun (_, letter, _ : trans) : bool -> 
              match letter with
              | None -> true
              | _ -> false
          ) 
          transitions 
          in
          if TransSet.is_empty transitions then
            acc
          else
            let states = TransSet.fold (
              fun (_, _, state2 : trans) 
                  (acc : states) : states -> 
                StateSet.add state2 acc
            ) 
            transitions StateSet.empty 
            in
            StateSet.fold (
              fun (state2 : state)
                  (acc : states) : states ->
                (* next state already done *)
                if StateSet.mem state2 already_done then
                  acc
                (* we call on each next state [state2] and we add all his eps neighbours to [state1]'s ones*)
                else
                  StateSet.union acc
                    @@ StateSet.add state2
                    @@ get_accessible_states_with_eps_trans state2
                    @@ (StateSet.empty, StateSet.add state1 already_done)
            ) 
            states StateSet.empty
        in
        (* eps_closure is a map from a state to a StateSet (the set of all the accessible states with eps transitions) *)
        Hashtbl.replace eps_closure state 
          @@ StateSet.add state
          @@ get_accessible_states_with_eps_trans state 
          @@ (StateSet.empty, StateSet.empty)
    ) 
    automaton.states
    in
    let () = Hashtbl.iter (
      fun k v ->
        Printf.printf "\n%d :" k ;
        StateSet.iter (
          fun s ->
            Printf.printf " %d" s
        ) v
    ) eps_closure
    in
    (* Merging states 
      
      key : states
      value : (lt option * states) list

      expl : {1, 2, 5} -> [ ("a", {1}) ; ("b", {1, 2, 5}) ; ("c", {}) ; ("d", {3, 4}) ]
    *)
    let new_trans = StateSetHashtbl.create 16 in
    (* Starting with the StateSets of the start_state (stack) *)
    let states_to_do = StateSetSet.add (Hashtbl.find eps_closure start_state) StateSetSet.empty in
    let rec merge_transitions (states_to_do : states_set) 
                              (states_done : states_set) : unit =
      if not @@ StateSetSet.is_empty states_to_do then
        let states = StateSetSet.choose states_to_do in
        let states_to_do = StateSetSet.remove states states_to_do 
        in
        (* i_transitions : (lt option * states) list 
            = list of all possible next states
          
           n_to_do : states_set 
            = sett of all states we need to apply the algorithm
        *)
        let i_transitions, n_to_do = LetterSet.fold (
          (* For all letter *)
          fun (letter : lt)
              (i_transitions, stack : (lt option * states) list * states_set ) : ((lt option * states) list * states_set) ->
            (* For all states in states, we gather all the next states the accesible states with a transitions labelled letter *)
            let next_states = StateSet.fold (
              fun state1 acc ->
                (* Get all transitions *)
                let transitions = get_transition_from automaton state1 in
                (* All transitions labelled [letter] *)
                let transitions = TransSet.filter (
                  fun (_, letter', _ : trans) : bool -> 
                    Letter.compare_opt letter' @@ Some letter = 0
                ) 
                transitions 
                in
                (* Sets of all possible next states *)
                let next_states = TransSet.fold (
                  fun (_, _, state2 : trans)
                      (acc : states) : states ->
                    StateSet.add state2 acc
                )
                transitions StateSet.empty 
                in
                (* We join with the other accesible states *) 
                StateSet.union acc next_states
            ) 
            states StateSet.empty 
            in
            let n_stack =
              (* We don't apply the function if
                  - there aren't any transitions
                  - or we already did the job
                  - or it's a loop
               *)
              if StateSet.is_empty next_states || StateSetSet.mem next_states states_done || StateSet.compare next_states states = 0 then
                stack
              else
                StateSetSet.add next_states stack
            in
            (* We add the transitions labelled [letter] from [states] to [next_states], 
               and we also return the new to_do_stack *)
            (Some letter, next_states) :: i_transitions, n_stack
        ) 
        automaton.alphabet ([], states_to_do) 
        in
        let () = StateSetHashtbl.replace new_trans states i_transitions in
        (* We keep_going the algo with the StateSet in the to_do list, and we add the current states to the done list since we just applied the algo to it*)
        merge_transitions n_to_do @@ StateSetSet.add states states_done
    in
    let () = merge_transitions states_to_do StateSetSet.empty in
    (* Renaming StateSet into a state 
       This is all the new states
    *)
    let states_name = StateSetHashtbl.create 16 in
    let state_nb = ref 0 in
    let () = StateSetHashtbl.iter (
      fun (states : states) 
          (_ : (lt option * states) list) : unit -> 
        StateSetHashtbl.replace states_name states !state_nb ;
        (* Small counter *)
        state_nb := !state_nb + 1
    ) 
    new_trans 
    in
    (* Gather all new transitions *)
    let transitions = StateSetHashtbl.fold (
      fun (state : states)
          (i_transitions : (lt option * states) list) 
          (acc : transitions) ->
        (* State name ([state] is a set)*)
        let state1 = StateSetHashtbl.find states_name state in
        (* Add to the acc the new transitions *)
        let transitions = List.fold_left (
          fun (acc' : transitions) 
              (letter, states : lt option * states) : transitions ->
            (* Transitions *)
            if StateSet.is_empty states then
              acc'
            else
              let trans = (state1, letter, StateSetHashtbl.find states_name states) in
              TransSet.add trans acc'
        ) 
        TransSet.empty i_transitions
        in 
        TransSet.union acc transitions
    )
    new_trans TransSet.empty 
    in
    (* Gather all end states *)
    let end_states = StateSetHashtbl.fold (
      fun (states : states) 
          (state_name : state) 
          (acc : states) : states ->
        (* If one of the state in states (which is a StateSet) is an end state, then the state name of states is an end state *)
       if StateSet.exists (fun (state : state) : bool -> StateSet.mem state states) automaton.ends then
          StateSet.add state_name acc
        else
          acc
    ) 
    states_name StateSet.empty
    in
    (* The DFA automaton *)
    { 
      (* Same alphabet *)
      alphabet = automaton.alphabet ; 
      (* Get all new states *)
      states = StateSet.of_seq @@ StateSetHashtbl.to_seq_values states_name ;
      (* Get the name of the state from its "set name" *)
      starts = StateSet.add (StateSetHashtbl.find states_name @@ Hashtbl.find eps_closure start_state) StateSet.empty ; 
      (* Previously calculated transitions *)
      trans = transitions ;
      (* Previously calculated end states *)
      ends = end_states ; 
    }

  let get_rid_of_unreachable_states (automaton : t) : t =
    let states = StateSet.fold (
      fun (state : state)
          (acc : states) : states ->
        let rec get_all_accessible_states (state : state) : states =
          let transitions = get_transition_from automaton state in
          TransSet.fold (
            fun (state1, _, state2 : trans)
                (acc' : states)  : states ->
              StateSet.union acc' 
                @@ StateSet.add state1 
                @@ get_all_accessible_states state2
          ) 
          transitions StateSet.empty
        in
        StateSet.union acc @@ get_all_accessible_states state
    ) 
     automaton.starts StateSet.empty
    in
    { 
      alphabet = automaton.alphabet ; 
      states = states ;
      starts = StateSet.filter (fun state -> StateSet.mem state states) automaton.starts ; 
      trans = TransSet.filter (fun (state, _, _) -> StateSet.mem state states) automaton.trans ;
      ends = StateSet.filter (fun state -> StateSet.mem state states) automaton.ends ; 
    }
end