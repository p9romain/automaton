module type Letter = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

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
  (* val get_rid_of_unreachable_states : t -> t *)
  (* val minimize : t -> t *)
  
  (* val check_word : t -> lt list -> bool *)

  (* val to_regex : t -> string *)
  (* val from_regex : string -> lt list -> t *)

end

module Make (Lt : Letter): S with type lt = Lt.t = struct

  type lt = Lt.t
  type lang = lt list

  type states = int list

  type tr = int * lt option * int
  type transitions = tr list

  type t = { 
              alphabet : lang ; 
              states : states ; 
              starts : states ; 
              trans : transitions ; 
              ends : states
           }

  module StateSet = Set.Make(Int)
  module StateSetHash = struct

    type t = StateSet.t

    let equal s1 s2 = StateSet.equal s1 s2

    let hash s =
      StateSet.fold (
        fun state acc ->
          Hashtbl.hash @@ state lxor acc
      ) s 0 
  end

  module StateSetHashtbl = Hashtbl.Make(StateSetHash)

  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let find_state (states : states)
                 (state : int) : int option =
    List.find_opt (fun state' -> compare state state' = 0) states

  let rec remove_first_state_from_list (l : states)
                                       (elt : int) : states =
    match l with
    | [] -> []
    | e :: l' ->
      if compare elt e = 0 then
        l'
      else
        e :: remove_first_state_from_list l' elt



  let letter_compare (letter : lt option)
                     (letter' : lt option) : int =
    match letter, letter' with
    | Some letter, Some letter' -> Lt.compare letter letter'
    | None, Some _ -> -1
    | None, None -> 0
    | Some _, None -> 1 

  let trans_compare (state1, letter, state2 : tr) 
                    (state1', letter', state2' : tr) : int =
    let c1 = compare state1 state1' in
    match c1 with
    | 0 ->
      begin
        let c2 = letter_compare letter letter' in
        match c2 with
        | 0 -> compare state2 state2'
        | _ -> c2
      end
    | _ -> c1



  let get_transition_from (automaton : t) 
                          (state : int) : transitions =
    List.fold_left (
      fun acc trans -> 
        let state', _, _ = trans in 
        if compare state state' = 0 then
          trans :: acc
        else
          acc
      ) 
      [] automaton.trans

  let get_transition_between (automaton : t) 
                             (state1 : int)
                             (state2 : int) : transitions =
    List.fold_left (
      fun acc trans -> 
        let state1', _, state2' = trans in 
        if compare state1 state1' = 0 && compare state2 state2' = 0 then
          trans :: acc
        else
          acc
      ) 
      [] automaton.trans

(*   let get_transition_to (automaton : t) 
                        (state : int) : transitions =
    List.fold_left (
      fun acc trans -> 
        let _, _, state' = trans in 
        if compare state state' = 0 then
          trans :: acc
        else
          acc
      ) 
      [] automaton.trans  *)                 


  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let empty : t = { 
                    alphabet = [] ; 
                    states = [] ; 
                    starts = [] ; 
                    trans = [] ;
                    ends = [] ; 
                  }

  let create (alphabet : lang) : t = 
    { empty with alphabet = alphabet }



  let add_state (automaton : t) 
                (state : int) : t =
    match find_state automaton.states state with
    | None -> { automaton with states = state :: automaton.states }
    | Some _ -> automaton

  let add_states (automaton : t) 
                 (states : states) : t =
    List.fold_left (fun acc state -> add_state acc state) automaton states

  let add_trans (automaton : t) 
                (state1 : int) 
                (letter : lt option)
                (state2 : int) : t =
    match find_state automaton.states state1, find_state automaton.states state2 with
    | Some _, Some _ ->
      begin
        let trans = (state1, letter, state2) in
        match letter with
        | None -> 
          begin
            match List.find_opt (fun trans' -> trans_compare trans trans' = 0 ) automaton.trans with
            | None -> { automaton with trans = trans :: automaton.trans }
            | Some _ -> automaton
          end
        | Some letter ->
          begin
            match List.find_opt (fun letter' -> Lt.compare letter letter' = 0 ) automaton.alphabet with
            | Some _ -> 
              begin
                match List.find_opt (fun trans' -> trans_compare trans trans' = 0 ) automaton.trans with
                | None -> { automaton with trans = trans :: automaton.trans }
                | Some _ -> automaton
              end
            | None -> failwith "given letter isn't in the automaton's alphabet"
          end
      end
    | _ -> failwith "both given states must be automaton's states"

  let add_transitions (automaton : t) 
                      (transitions : transitions) : t =
    List.fold_left (fun acc (state1, letter, state2) -> add_trans acc state1 letter state2) automaton transitions

  let add_start (automaton : t) 
                (state : int) : t =
    match find_state automaton.states state with
    | Some _ ->
      begin
        match find_state automaton.starts state with
        | None -> { automaton with starts = state :: automaton.starts }
        | Some _ -> automaton
      end
    | None -> failwith "given state must be an automaton's state"

  let add_starts (automaton : t) 
                 (states : states) : t =
    List.fold_left (fun acc state -> add_start acc state) automaton states

  let add_end (automaton : t) 
              (state : int) : t =
    match find_state automaton.states state with
    | Some _ ->
      begin
        match find_state automaton.ends state with
        | None -> { automaton with ends = state :: automaton.ends }
        | Some _ -> automaton
      end
    | None -> failwith "given state must be an automaton's state"

  let add_ends (automaton : t) 
               (states : states) : t =
    List.fold_left (fun acc state -> add_end acc state) automaton states



  let remove_state (automaton : t) 
                   (state : int) : t =
    { automaton with states = remove_first_state_from_list automaton.states state }

  let remove_states (automaton : t) 
                    (states : states) : t =
    List.fold_left (fun acc state -> remove_state acc state) automaton states

  let remove_trans (automaton : t) 
                   (state1 : int) 
                   (letter : lt option) 
                   (state2 : int) : t =
    let rec remove_first_trans_from_list (l : transitions)
                                         (elt : tr) : transitions =
      match l with
      | [] -> []
      | e :: l' ->
        if trans_compare elt e = 0 then
          l'
        else
          e :: remove_first_trans_from_list l' elt
    in
    { automaton with trans = remove_first_trans_from_list automaton.trans (state1, letter, state2) }

  let remove_all_trans_between (automaton : t) 
                               (state1 : int) 
                               (state2 : int) : t =
    let transitions = List.filter (
      fun (s1, _, s2) -> 
        compare state1 s1 <> 0 || compare state2 s2 <> 0 
      ) 
      automaton.trans
    in
    { automaton with trans = transitions }

  let remove_start (automaton : t) 
                   (state : int) : t =
    { automaton with starts = remove_first_state_from_list automaton.starts state }

  let remove_starts (automaton : t) 
                    (states : states) : t =
    List.fold_left (fun acc state -> remove_start acc state) automaton states

  let remove_end (automaton : t) 
                 (state : int) : t =
    { automaton with ends = remove_first_state_from_list automaton.ends state }

  let remove_ends (automaton : t) 
                  (states : states) : t =
    List.fold_left (fun acc state -> remove_end acc state) automaton states



  let to_dot (automaton : t)
             (file_name : string) : unit =
    let file = open_out (file_name ^ ".dot") in
    Printf.fprintf file "digraph automaton\n{\n" ;
    List.iteri ( 
      fun i state -> 
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ [label= \"\", shape=none,height=.0,width=.0] ;\n" i ;
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ -> %s ;\n" i (string_of_int state)
      ) 
      automaton.starts ;
    List.iter ( fun state -> Printf.fprintf file "  %s [peripheries=2] ;\n" (string_of_int state) ) automaton.ends ;
    List.iter (
      fun state1 ->
        List.iter (
          fun state2 ->
            match get_transition_between automaton state1 state2 with
            | [] -> ()
            | transitions ->
              let letters = List.map (
                fun (_, letter, _) ->
                  match letter with
                  | None -> "ε"
                  | Some letter -> Lt.to_string letter
              ) transitions 
              in
              let letter = String.concat ", " letters in
              Printf.fprintf file "  %s -> %s [label=\"%s\"] ;\n" (string_of_int state1) (string_of_int state2) letter
          ) 
          automaton.states
      ) 
      automaton.states ;
    Printf.fprintf file "}" ;
    close_out file



  let is_deterministic (automaton : t) : bool =
    List.length automaton.starts = 1 (* Only one start state *)
    (* No epsilon transition or same letter transition from a state *)
    && 
    List.for_all (
      fun state ->
        (* Get all transitions from the state *)
        let transitions = get_transition_from automaton state in
        (* Sort all the letters to see if there is more than once a letter *)
        let letters = List.sort letter_compare @@ List.map (fun (_, l, _) -> l) transitions in
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
                if Lt.compare letter letter' = 0 then
                  false
                else
                  check letters elt
            end
        in
        check letters None
    )
    automaton.states



  let determinize (automaton : t) : t =
    (* Only one start state *)
    let start_state = (List.fold_left min (List.nth automaton.states 0) automaton.states) - 1 in
    (* Get its state name *)
    let automaton = add_state automaton @@ start_state in
    (* Link the new start state with previous start states *)
    let automaton = add_transitions automaton @@ List.map (fun state -> (start_state, None, state)) automaton.starts in
    (* Removes old start states *)
    let automaton = remove_starts automaton automaton.starts in
    (* Add new start state *)
    let automaton = add_start automaton start_state in
    (* To get rid of eps transitions *)
    let eps_closure = Hashtbl.create 16 in
    let () = List.iter (
      fun state ->
        let rec get_accessible_states_with_eps_trans (state1 : int) 
                                                     (acc, already_done : states * states) : states =
          (* all transitions *)
          let transitions = get_transition_from automaton state1 in
          (* keep only the eps transitions *)
          let transitions = List.filter (
            fun (_, letter, _) -> 
              match letter with
              | None -> true
              | _ -> false
          ) 
          transitions 
          in
          match transitions with
          | [] -> acc (* no eps transitions *)
          | _ ->
            (* next states *)
            let states = List.map (fun (_, _, state) -> state) transitions in
            (* returns all the accessible states with eps transition (with transivity) *)
            List.flatten @@ List.cons states @@ List.fold_left (
              fun acc state' -> 
                (* continue eps-transition path if it isn't in already_done*)
                match List.find_opt (fun state -> compare state state' = 0) already_done with
                | None -> (get_accessible_states_with_eps_trans state' ([], state1 :: already_done)) :: acc
                | Some _ -> acc
            ) [] states
        in
        (* eps_closure is a map from a state to a StateSet (the set of all the accessible states with eps transitions) *)
        Hashtbl.replace eps_closure state @@ StateSet.of_list @@ List.cons state @@ get_accessible_states_with_eps_trans state ([], [])
    ) 
    automaton.states
    in
    (* Merging states *)
    let new_trans = StateSetHashtbl.create 16 in
    (* Starting with the StateSets of the start_state *)
    let states_to_do = [ Hashtbl.find eps_closure start_state ] in
    let rec merge_transitions (states_to_do : StateSet.t list) 
                              (states_done : StateSet.t list) : unit =
      match states_to_do with
      | [] -> ()
      | states :: states_to_do ->
        (* i_transitions : (lt option, StateSet.t) list 
            = list of all possible next states
          
           n_to_do : StateSet.t list 
            = list of all StateSet we need to apply the algorithm
        *)
        let i_transitions, n_to_do = List.fold_left (
          (* For all letter *)
          fun (i_transitions, stack) letter ->
            (* For all states in states, we gather all the next states the accesible states with a transitions labelled letter *)
            let next_states = StateSet.fold (
              fun state1 acc ->
                (* Get all transitions *)
                let transitions = get_transition_from automaton state1 in
                (* Filter all next states labelled letter *)
                let next_states = List.map (fun (_, _, state2) -> state2) @@ List.filter (fun (_, letter', _) -> letter_compare letter' @@ Some letter = 0) transitions in
                (* We join with the other accesible states *) 
                StateSet.union acc @@ StateSet.of_list next_states
            ) 
            states StateSet.empty 
            in
            let n_stack =
              (* If there isn't any next states or if it's a loop, we don't apply the algo on them *)
              if StateSet.is_empty next_states || StateSet.compare next_states states = 0 then
                (* No transitions *)
                stack
              else
                begin
                  (* If the algo with [next_states] was already done, we don't add it in the [states_to_do]*)
                  match List.find_opt (fun s -> StateSet.compare s next_states = 0) states_done with
                  | None -> 
                    begin
                      (* if it's already in the to_do list *)
                      match List.find_opt (fun s -> StateSet.compare s next_states = 0) stack with
                      | None -> next_states :: stack
                      | Some _ -> stack
                    end
                  | Some _ -> stack
                end
            in
            (* We add the transitions labelled [letter] from [states] to [next_states], 
               and we also return the new to_do_stack *)
            (Some letter, next_states) :: i_transitions, n_stack
        ) 
        ([], states_to_do) automaton.alphabet 
        in 
        (* new_trans is a map from a StateSet to a (lt option, StateSet) list : it map the future state to all the transitions (the letter and the next new state)*)
        let () = StateSetHashtbl.replace new_trans states i_transitions in
        (* We keep_going the algo with the StateSet in the to_do list, and we add the current states to the done list since we just applied the algo to it*)
        merge_transitions n_to_do @@ states :: states_done
    in
    let () = merge_transitions states_to_do [] in
    (* Renaming StateSet into a state 
       This is all the new states
    *)
    let states_name = StateSetHashtbl.create 16 in
    let state_nb = ref 0 in
    let () = StateSetHashtbl.iter (
      fun states _ -> 
        StateSetHashtbl.replace states_name states !state_nb ;
        (* Small counter *)
        state_nb := !state_nb + 1
    ) 
    new_trans 
    in
    (* Gather all new transitions *)
    let transitions = StateSetHashtbl.fold (
      fun state i_transitions acc ->
        (* State name ([state] is a set)*)
        let state1 = StateSetHashtbl.find states_name state in
        (* Add to the acc the new transitions *)
        let transitions = List.fold_left (
          fun acc' (Some letter, states) ->
            (* Transitions *)
            if StateSet.is_empty states then
              acc'
            else
              let trans = (state1, Some letter, StateSetHashtbl.find states_name states) in
              trans :: acc'
        ) 
        [] i_transitions
        in 
        List.append acc transitions
    )
    new_trans [] 
    in
    (* Gather all end states *)
    let end_states = StateSetHashtbl.fold (
      fun states state_name acc ->
        (* If one of the state in states (which is a StateSet) is an end state, then the state name of states is an end state *)
        let b = List.exists (
          fun state ->
            match StateSet.find_opt state states with
            | None -> false (* the end state isn't in the set *)
            | Some _ -> true
        ) 
        automaton.ends
        in
        if b then
          state_name :: acc
        else
          acc
    ) 
    states_name [] 
    in
    (* let () = List.iter (fun s -> Printf.printf "%s " @@ Lt.to_string s) automaton.alphabet in
    let () = Printf.printf "\n" in
    let () = List.iter (Printf.printf "%d ") @@ List.of_seq @@ StateSetHashtbl.to_seq_values states_name in
    let () = Printf.printf "\n" in
    let () = Printf.printf "%d" @@ StateSetHashtbl.find states_name @@ Hashtbl.find eps_closure start_state in
    let () = Printf.printf "\n" in
    let () = List.iter (fun (s1, Some l, s2) -> Printf.printf "(%d, %s, %d) " s1 (Lt.to_string l) s2) transitions in
    let () = Printf.printf "\n" in
    let () = List.iter (Printf.printf "%d ") end_states in
    let () = Printf.printf "\n"
    in *)
    (* The DFA automaton *)
    { 
      (* Same alphabet *)
      alphabet = automaton.alphabet ; 
      (* Get all new states *)
      states = List.of_seq @@ StateSetHashtbl.to_seq_values states_name ;
      (* Get the name of the state from its "set name" *)
      starts = [ StateSetHashtbl.find states_name @@ Hashtbl.find eps_closure start_state ] ; 
      (* Previously calculated transitions *)
      trans = transitions ;
      (* Previously calculated end states *)
      ends = end_states ; 
    }
end