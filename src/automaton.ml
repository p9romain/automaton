module type State = sig

  type t

  val compare : t -> t -> int

  val to_string : t -> string

end

module type Letter = sig

  type t

  val compare : t -> t -> int

  val to_string : t -> string

  val empty : t
  val is_empty : t -> bool

  val string_to_list : string -> t list

end

module type S = sig

  type lt
  type st
  type t

  val empty : t
  val create : lt list -> t

  val add_state : t -> st -> t
  val add_trans : t -> st -> lt -> st -> t
  val add_start : t -> st -> t
  val add_end : t -> st -> t

  val remove_state : t -> st -> t
  val remove_one_trans : t -> st -> lt -> st -> t
  val remove_all_trans : t -> st -> st -> t
  val remove_start : t -> st -> t
  val remove_end : t -> st -> t

  val replace_trans : t -> st -> lt -> st -> t

  val is_deterministic : t -> bool
  (* val determinize : t -> t *)
  
  val check_word : t -> string -> bool

  (* val to_regex : t -> string *)
  (* val from_regex : string -> t *)

  val to_dot : t -> string -> unit

end

module Make (Lt : Letter) (St : State) : S with type lt = Lt.t and type st = St.t = struct

  type lt = Lt.t
  type st = St.t

  type trans = st * lt * st
  type trans_list = trans list

  type t = { 
              alphabet : lt list ; 
              states : st list ; 
              starts : st list ; 
              trans : trans_list ; 
              ends : st list
           }


  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let eps : lt = Lt.empty



  let find_state (states : st list)
                 (state : st) : st option =
    List.find_opt (fun s -> St.compare s state = 0) states

  let is_there_empty (trans : trans_list) : bool =
    List.exists (fun (_, letter, _) -> Lt.compare letter eps = 0) trans

  let compare (s1, l, s2 : trans) (s1', l', s2' : trans) : int =
    let c1 = St.compare s1 s1' in
    match c1 with
    | 0 ->
      begin
        let c2 = Lt.compare l l' in
        match c2 with
        | 0 -> St.compare s2 s2'
        | _ -> c2
      end
    | _ -> c1



  let get_transition_from (auto : t) 
                          (state : st) : trans_list =
    List.fold_left (
      fun acc t -> 
        let s, _, _ = t in 
        if St.compare s state = 0 then
          t :: acc
        else
          acc
      ) 
      [] auto.trans

  let get_transition_between (auto : t) 
                             (state1 : st)
                             (state2 : st) : trans_list =
    List.fold_left (
      fun acc t -> 
        let s1, _, s2 = t in 
        if St.compare s1 state1 = 0 && St.compare s2 state2 = 0 then
          t :: acc
        else
          acc
      ) 
      [] auto.trans                       


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

  let create (alphabet : lt list) : t = { empty with alphabet = alphabet }



  let add_state (auto : t) 
                (state : st) : t =
    match find_state auto.states state with
    | None -> { auto with states = state :: auto.states }
    | Some _ -> auto

  let add_trans (auto : t) 
                (state1 : st) 
                (letter : lt)
                (state2 : st) : t =
    let trans = (state1, letter, state2) in
    match List.find_opt (fun (s1, l, s2) -> compare (s1, l, s2) trans = 0 ) auto.trans with
    | None ->     { auto with trans = trans :: auto.trans }
    | Some _ -> auto

  let add_start (auto : t) 
                (state : st) : t =
    match find_state auto.starts state with
    | None -> { auto with starts = state :: auto.starts }
    | Some _ -> auto

  let add_end (auto : t) 
              (state : st) : t =
    match find_state auto.ends state with
    | None -> { auto with ends = state :: auto.ends }
    | Some _ -> auto



  let rec remove_first_state_from_list (l : st list)
                                       (elt : st) : st list =
    match l with
    | [] -> []
    | e :: l' ->
      if St.compare elt e = 0 then
        l'
      else
        e :: remove_first_state_from_list l' elt

  let remove_state (auto : t) 
                   (state : st) : t =
    { auto with states = remove_first_state_from_list auto.states state }

  let remove_one_trans (auto : t) 
                       (state1 : st) 
                       (letter : lt) 
                       (state2 : st) : t =
    let rec remove_first_trans_from_list (l : trans_list)
                                         (elt : trans) : trans_list =
      match l with
      | [] -> []
      | e :: l' ->
        if compare elt e = 0 then
          l'
        else
          e :: remove_first_trans_from_list l' elt
    in
    { auto with trans = remove_first_trans_from_list auto.trans (state1, letter, state2) }

  let remove_all_trans (auto : t) 
                       (state1 : st) 
                       (state2 : st) : t =
    let transitions = List.filter (
      fun (s1, _, s2) -> 
        St.compare state1 s1 <> 0 && St.compare state2 s2 <> 0 
      ) 
      auto.trans
    in
    { auto with trans = transitions }

  let remove_start (auto : t) 
                   (state : st) : t =
    { auto with starts = remove_first_state_from_list auto.starts state }

  let remove_end (auto : t) 
                 (state : st) : t =
    { auto with ends = remove_first_state_from_list auto.ends state }



  let replace_trans (auto : t) 
                    (state1 : st) 
                    (letter : lt) 
                    (state2 : st) : t =  
    add_trans (remove_all_trans auto state1 state2) state1 letter state2



  let is_deterministic (auto : t) : bool =
    List.length auto.starts = 1 (* Only one start state *)
    (* No epsilon transition *)
    && not (is_there_empty auto.trans)
    (* No same letter transition from a start state *)
    && List.fold_left (
        fun acc s ->
          (* Get all transitions from the state *)
          let trans = get_transition_from auto s in
          (* Sort all the letters to see if there is more than once a letter *)
          let letters = List.sort Lt.compare (List.map (fun (_, l, _) -> l) trans) in
          (* Check *)
          let rec check l old_elt =
            match l with
            | [] -> true
            | elt :: l ->
              if Lt.compare old_elt elt = 0 then
                false
              else
              check l elt
          in
          check letters eps && acc
      )
      true auto.states



  let check_word (auto : t)
                 (word : string) : bool =
    let rec read_word (remaining_word : lt list)
                       (current_state : st) : bool =
      match remaining_word with
      | [] ->
        begin
          match find_state auto.ends current_state with
          | None -> false
          | Some _ -> failwith "true"
        end
      | letter :: remaining_word ->
        begin
          (* Get all transitions from [current_state] *)
          let trans = get_transition_from auto current_state in
          (* Get all transitions labeled [letter] *)
          let trans = List.filter (fun (current_state, l, next_state) -> Lt.compare l letter = 0) trans in
          (* Get all possible next states *)
          let states = List.map (fun (_, _, next_state) -> next_state) trans in
          (* If states is empty, backtracking (and returns false) *)
          List.fold_left (fun acc next_state -> graph_path remaining_word next_state) false states
        end
    in 
    try
      let word = Lt.string_to_list word in
      List.fold_left (fun acc first_state -> (graph_path word first_state) || acc) false auto.starts
    with _ -> true



  (* 
    TODO : ocamllex and menhir? (i think yes)

    let from_regex (reg : string) : t =
    let read_regex (reg : char list)
                   (auto : t) : t =
      empty
    in
    read_regex (List.of_seq @@ String.to_seq reg) empty
   *)


  let to_dot (auto : t)
             (file_name : string) : unit =
    let file = open_out (file_name ^ ".dot") in
    Printf.fprintf file "digraph automaton\n{\n" ;
    List.iteri ( 
      fun i state -> 
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ [label= \"\", shape=none,height=.0,width=.0] ;\n" i ;
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ -> %s ;\n" i (St.to_string state)
      ) 
      auto.starts ;
    List.iter ( fun state -> Printf.fprintf file "  %s [peripheries=2] ;\n" (St.to_string state) ) auto.ends ;
    List.iter (
      fun state1 ->
        List.iter (
          fun state2 ->
            match get_transition_between auto state1 state2 with
            | [] -> ()
            | trans ->
              let letters = List.map (fun (_, l, _) -> Lt.to_string l) trans in
              let letter = String.concat ", " letters in
              Printf.fprintf file "  %s -> %s [label=\"%s\"] ;\n" (St.to_string state1) (St.to_string state2) letter
          ) 
          auto.states
      ) 
      auto.states ;(* TODO : merge in one arrow per states pair *)
    Printf.fprintf file "}" ;
    close_out file ;

end