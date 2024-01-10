module type State = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

end

module type Letter = sig

  type t

  val compare : t -> t -> int
  val to_string : t -> string

end

module type S = sig

  type lt
  type st
  type t

  val empty : t
  val create : lt list -> t

  val add_state : t -> st -> t
  val add_trans : t -> st -> lt option -> st -> t
  val add_start : t -> st -> t
  val add_end : t -> st -> t

  val remove_state : t -> st -> t
  val remove_one_trans : t -> st -> lt option -> st -> t
  val remove_all_trans : t -> st -> st -> t
  val remove_start : t -> st -> t
  val remove_end : t -> st -> t

  val replace_trans : t -> st -> lt option -> st -> t

  val is_deterministic : t -> bool
  (* val determinize : t -> t *)
  
  val check_word : t -> lt list -> bool

  (* val to_regex : t -> string *)
  (* val from_regex : string -> t *)

  val to_dot : t -> string -> unit

end

module Make (Lt : Letter) (St : State) : S with type lt = Lt.t and type st = St.t = struct

  type lt = Lt.t
  type lang = lt list
  
  type st = St.t
  type states = st list

  type tr = st * lt option * st
  type transitions = tr list

  type t = { 
              alphabet : lang ; 
              states : states ; 
              starts : states ; 
              trans : transitions ; 
              ends : states
           }


  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let find_state (states : states)
                 (state : st) : st option =
    List.find_opt (fun state' -> St.compare state state' = 0) states

  let is_there_empty (trans : transitions) : bool =
    List.exists (
      fun (_, letter, _) -> 
        match letter with 
        | None -> true 
        | Some _ -> false
    ) trans

  let trans_compare (state1, letter, state2 : tr) 
                    (state1', letter', state2' : tr) : int =
    let c1 = St.compare state1 state1' in
    match c1 with
    | 0 ->
      begin
        match letter, letter' with
        | Some letter, Some letter' ->
          begin
            let c2 = Lt.compare letter letter' in
            match c2 with
            | 0 -> St.compare state2 state2'
            | _ -> c2
          end
        | None, None ->
          St.compare state2 state2'
        | _ -> 
          -1 (* arbitrary : i just need a not zero value *)
      end
    | _ -> c1

  let letter_compare (letter : lt option)
                     (letter' : lt option) : int =
    match letter, letter' with
    | Some letter, Some letter' -> Lt.compare letter letter'
    | None, Some _ -> -1
    | None, None -> 0
    | Some _, None -> 1 



  let get_transition_from (automaton : t) 
                          (state : st) : transitions =
    List.fold_left (
      fun acc trans -> 
        let state, _, _ = trans in 
        if St.compare state state = 0 then
          trans :: acc
        else
          acc
      ) 
      [] automaton.trans

  let get_transition_between (automaton : t) 
                             (state1 : st)
                             (state2 : st) : transitions =
    List.fold_left (
      fun acc trans -> 
        let state1', _, state2' = trans in 
        if St.compare state1 state1' = 0 && St.compare state2 state2' = 0 then
          trans :: acc
        else
          acc
      ) 
      [] automaton.trans                       


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

  let create (alphabet : lang) : t = { empty with alphabet = alphabet }



  let add_state (automaton : t) 
                (state : st) : t =
    match find_state automaton.states state with
    | None -> { automaton with states = state :: automaton.states }
    | Some _ -> automaton

  let add_trans (automaton : t) 
                (state1 : st) 
                (letter : lt option)
                (state2 : st) : t =
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

  let add_start (automaton : t) 
                (state : st) : t =
    match find_state automaton.states state with
    | Some _ ->
      begin
        match find_state automaton.starts state with
        | None -> { automaton with starts = state :: automaton.starts }
        | Some _ -> automaton
      end
    | None -> failwith "given state must be an automaton's state"

  let add_end (automaton : t) 
              (state : st) : t =
    match find_state automaton.states state with
    | Some _ ->
      begin
        match find_state automaton.ends state with
        | None -> { automaton with ends = state :: automaton.ends }
        | Some _ -> automaton
      end
    | None -> failwith "given state must be an automaton's state"



  let rec remove_first_state_from_list (l : states)
                                       (elt : st) : states =
    match l with
    | [] -> []
    | e :: l' ->
      if St.compare elt e = 0 then
        l'
      else
        e :: remove_first_state_from_list l' elt

  let remove_state (automaton : t) 
                   (state : st) : t =
    { automaton with states = remove_first_state_from_list automaton.states state }

  let remove_one_trans (automaton : t) 
                       (state1 : st) 
                       (letter : lt option) 
                       (state2 : st) : t =
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

  let remove_all_trans (automaton : t) 
                       (state1 : st) 
                       (state2 : st) : t =
    let transitions = List.filter (
      fun (s1, _, s2) -> 
        St.compare state1 s1 <> 0 && St.compare state2 s2 <> 0 
      ) 
      automaton.trans
    in
    { automaton with trans = transitions }

  let remove_start (automaton : t) 
                   (state : st) : t =
    { automaton with starts = remove_first_state_from_list automaton.starts state }

  let remove_end (automaton : t) 
                 (state : st) : t =
    { automaton with ends = remove_first_state_from_list automaton.ends state }



  let replace_trans (automaton : t) 
                    (state1 : st) 
                    (letter : lt option) 
                    (state2 : st) : t =  
    add_trans (remove_all_trans automaton state1 state2) state1 letter state2



  let is_deterministic (automaton : t) : bool =
    List.length automaton.starts = 1 (* Only one start state *)
    (* No same letter transition from a start state or epsilon *)
    && 
    List.fold_left (
      fun acc state ->
        (* Get all transitions from the state *)
        let trans = get_transition_from automaton state in
        (* Sort all the letters to see if there is more than once a letter *)
        let letters = List.sort letter_compare @@ List.map (fun (_, l, _) -> l) trans in
        (* Check *)
        let rec check l old_elt =
          match l with
          | [] -> true
          | elt :: l ->
            begin
              match old_elt, elt with
              (* epsilon transition *)
              | _, None -> false
              (* first letter *)
              | None, _ ->
                check l elt
              (* else *)
              | Some letter, Some letter' ->
                if Lt.compare letter letter' = 0 then
                  false
                else
                  check l elt
            end
        in
        check letters None && acc
    )
    true automaton.states



  let check_word (automaton : t)
                 (word : lt list) : bool =
    let open Set.Make(St) in
    let get_transition_from = get_transition_from automaton in
    (* parrallel search *)
    let end_states = List.fold_left (
      fun acc letter ->
        (* getting all transitions from all current states *)
        let get_trans = List.flatten @@ List.map get_transition_from @@ to_list acc in
        (* getting all next states *)
        let rec next_states transitions =
          match transitions with
          | [] -> []
          | trans :: transitions ->
            begin
              let (_, letter', state) = trans in
              match letter' with
              (* epsilon transition *)
              | None ->
                (* all transitions after this epsilon transition *)
                let next_eps_trans = get_transition_from state in
                next_states transitions @ next_states next_eps_trans
              (* else *)
              | Some letter' ->
                (* check if we need to check this path *)
                if Lt.compare letter letter' = 0 then
                  state :: next_states transitions
                (* next transition *)
                else
                  next_states transitions
            end
        in
        of_list @@ next_states get_trans
    )
    (of_list automaton.starts) word
    in
    (* if one end state is an end state of the automaton *)
    exists (
      fun state -> 
        match List.find_opt (fun state' -> St.compare state state' = 0) automaton.ends with 
        | None -> false 
        | Some _ -> true 
    )
    end_states


  let to_dot (automaton : t)
             (file_name : string) : unit =
    let file = open_out (file_name ^ ".dot") in
    Printf.fprintf file "digraph automaton\n{\n" ;
    List.iteri ( 
      fun i state -> 
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ [label= \"\", shape=none,height=.0,width=.0] ;\n" i ;
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ -> %s ;\n" i (St.to_string state)
      ) 
      automaton.starts ;
    List.iter ( fun state -> Printf.fprintf file "  %s [peripheries=2] ;\n" (St.to_string state) ) automaton.ends ;
    List.iter (
      fun state1 ->
        List.iter (
          fun state2 ->
            match get_transition_between automaton state1 state2 with
            | [] -> ()
            | trans ->
              let letters = List.map (
                fun (_, letter, _) ->
                  match letter with
                  | None -> ""
                  | Some letter -> Lt.to_string letter
              ) trans 
              in
              let letter = String.concat ", " letters in
              Printf.fprintf file "  %s -> %s [label=\"%s\"] ;\n" (St.to_string state1) (St.to_string state2) letter
          ) 
          automaton.states
      ) 
      automaton.states ;(* TODO : merge in one arrow per states pair *)
    Printf.fprintf file "}" ;
    close_out file ;

end