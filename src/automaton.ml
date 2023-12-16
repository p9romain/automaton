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

  type t = { 
              alphabet : lt list ; 
              states : st list ; 
              starts : st list ; 
              trans : (st * lt * st) list ; 
              ends : st list
           }


  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)


  let eps : lt = Lt.empty



  let is_there_empty (trans : (st * lt * st) list) : bool =
    List.exists (fun (_, letter, _) -> Lt.compare letter eps = 0) trans

  let compare (s1, l, s2 : st * lt * st) (s1', l', s2' : st * lt * st) : int =
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
    match List.find_opt (fun s -> St.compare s state = 0) auto.states with
    | None -> { auto with states = state :: auto.states }
    | Some _ -> auto

  let add_trans (auto : t) 
                (state1 : st) 
                (letter : lt)
                (state2 : st) : t =
    let trans = (state1, letter, state2) in
    match List.find_opt (fun (s1, l, s2) -> compare (s1, l, s2) trans = 0 ) auto.states with
    | None ->     { auto with trans = trans :: auto.trans }
    | Some _ -> auto

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
    let rec remove_first_trans_from_list (l : (st * lt * st) list)
                                         (elt : st * lt * st) : (st * lt * st) list =
      match l with
      | [] -> []
      | e :: l' ->
        if compare elt e = 0 then
          l'
        else
          e :: remove_first_state_from_list l' elt
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
    && true

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
    (* TODO : merge in one arrow per states pair *)
    Printf.fprintf file "}" ;
    close_out file ;

end