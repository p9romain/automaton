module type OrderedPrintableType = sig

  type t

  val compare : t -> t -> int

  val to_string : t -> string

end

module type OrderedEmptyPrintableType = sig

  type t

  val compare : t -> t -> int

  val to_string : t -> string

  val empty : t
  val is_empty : t -> bool

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
  
  (* val check_word : t -> lt Seq.t -> bool *)

  (* val to_regex : t -> string *)
  (* val from_regex : string -> t *)

  val to_dot : t -> string -> unit

end

module Make (Lt : OrderedEmptyPrintableType) (St : OrderedPrintableType) : S with type lt = Lt.t and type st = St.t = struct

  type lt = Lt.t
  type st = St.t
  let eps : lt = Lt.empty   

  module StateLetter = struct
    type t = st * st

    let compare (s1, s1') (s2, s2') =
      match St.compare s1 s2 with
      | 0 -> St.compare s1' s2'
      | c -> c
  end
  module Trans = Map.Make(StateLetter)

  type t = { 
              alphabet : lt list ; 
              states : st list ; 
              starts : st list ; 
              trans : (lt list) Trans.t ; 
              ends : st list
           }        



  let empty : t = { 
                alphabet = [] ; 
                states = [] ; 
                starts = [] ; 
                trans = Trans.empty ;
                ends = [] ; 
              }

  let create (alphabet : lt list) : t = { empty with alphabet = alphabet }



  let rec is_there_empty (l : lt list) : bool =
    match l with
    | [] -> false
    | e :: l ->
      if Lt.compare e eps = 0 then
        true
      else
        is_there_empty l



  let add_state (auto : t) 
                (state : st) : t =
    match List.find_opt (fun s -> St.compare s state = 0) auto.states with
    | None -> { auto with states = state :: auto.states }
    | Some _ -> auto

  let add_trans (auto : t) 
                (state1 : st) 
                (letter : lt)
                (state2 : st) : t =
    let transitions =
      begin
        match Trans.find_opt (state1, state2) auto.trans with
        | None -> Trans.add (state1, state2) [ letter ] auto.trans
        | Some letter_list -> 
          if Lt.is_empty letter then
            failwith "Trying to add ε over already existing transitions : cannot choose the outcome"
          else
            Trans.add (state1, state2) (letter :: letter_list) auto.trans
      end
    in
    { auto with trans = transitions }

  let add_start (auto : t) 
                (state : st) : t =
    match List.find_opt (fun s -> St.compare s state = 0) auto.starts with
    | None -> { auto with starts = state :: auto.starts }
    | Some _ -> auto

  let add_end (auto : t) 
              (state : st) : t =
    match List.find_opt (fun s -> St.compare s state = 0) auto.ends with
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
    let transitions =
      begin
        match Trans.find_opt (state1, state2) auto.trans with
        | None -> auto.trans
        | Some letter_list ->
          begin
            let rec remove_first_letter_from_list (l : lt list)
                                                  (elt : lt) : lt list =
              match l with
              | [] -> []
              | e :: l' ->
                if Lt.compare elt e = 0 then
                  l'
                else
                  e :: remove_first_letter_from_list l' elt
            in
            match remove_first_letter_from_list letter_list letter with
            | [] -> Trans.remove (state1, state2) auto.trans
            | letter_list -> Trans.add (state1, state2) letter_list auto.trans
          end
      end
    in
    { auto with trans = transitions }

  let remove_all_trans (auto : t) 
                       (state1 : st) 
                       (state2 : st) : t =
    let transitions = Trans.remove (state1, state2) auto.trans
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
    && Trans.fold (
        fun _ letters acc -> 
          not (is_there_empty letters) && acc
        ) 
        auto.trans true
    (* No same letter transition from a start state *)
    && List.fold_left (
        fun acc state1 -> 
          (* All the transitions from state1 *)
          let letter_list = List.fold_left (
            fun acc' state2 ->
              match Trans.find_opt (state1, state2) auto.trans with
              | None -> acc'
              | Some letter_list -> letter_list @ acc'
            )
            [] auto.states
          in
          (* Sorting to see if each letter is unique or not *)
          let letter_list = List.sort Lt.compare letter_list in
          let rec check l acc =
            match l with
            | [] -> true
            | elt :: l ->
              if Lt.compare elt acc = 0 then
                false
              else
                check l elt
          in
          check letter_list eps && acc
        ) 
        true auto.states

  let to_dot (auto : t)
             (file_name : string) : unit =
    let file = open_out (file_name ^ ".dot") in
    Printf.fprintf file "digraph automaton\n{\n" ;
    List.iteri ( 
      fun i state -> 
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ [label= \"\", shape=none,height=.0,width=.0]\n" i ;
        Printf.fprintf file "  __INVISIBLE_NODE_%d__ -> %s [peripheries=2]\n" i (St.to_string state)
      ) 
      auto.starts ;
    List.iter ( fun state -> Printf.fprintf file "  %s [peripheries=2]\n" (St.to_string state) ) auto.ends ;
    Trans.iter (
      fun (state1, state2) letters -> 
        if is_there_empty letters then
          Printf.fprintf file "  %s -> %s [label=\"ε\"] ;\n" (St.to_string state1) (St.to_string state2)
        else
          let letters = String.concat ", " (List.map Lt.to_string letters) in
          Printf.fprintf file "  %s -> %s [label=\"%s\"] ;\n" (St.to_string state1) (St.to_string state2) letters
      ) 
      auto.trans ;
    Printf.fprintf file "}" ;
    close_out file ;

end