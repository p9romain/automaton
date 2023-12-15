module type OrderedType = sig

  type t

  val compare : t -> t -> int

end

module type OrderedEmptyType = sig

  type t

  val compare : t -> t -> int

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
  val remove_all_trans : t -> st -> lt -> t
  val remove_start : t -> st -> t
  val remove_end : t -> st -> t

  (* val clear : t -> t


  val is_deterministic : t -> bool
  val determinize : t -> t
  
  val check_word : t -> lt Seq.t -> bool

  val to_regex : t -> string
  val from_regex : string -> t 

  val to_dot : t -> string -> unit *)

end

module Make (Lt : OrderedEmptyType) (St : OrderedType) : S with type lt = Lt.t and type st = St.t = struct

  type lt = Lt.t
  type st = St.t
  let eps : lt = Lt.empty   

  module StateLetter = struct
    type t = st * lt

    let compare (s1, l1) (s2, l2) =
      match compare s1 s2 with
      | 0 -> compare l1 l2
      | c -> c
  end
  module Trans = Map.Make(StateLetter)

  type t = { 
              alphabet : lt list ; 
              states : st list ; 
              starts : st list ; 
              trans : (st list) Trans.t ; 
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



  let add_state (auto : t) 
                (state : st) : t =
    match List.find_opt (fun s -> St.compare s state = 0) auto.states with
    | None -> { auto with states = state :: auto.states }
    | Some _ -> auto

  let add_trans (auto : t) 
                (state1 : st) 
                (letter : lt )
                (state2 : st) : t =
    let transitions =
      begin
        match Trans.find_opt (state1, letter) auto.trans with
        | None -> Trans.add (state1, letter) [ state2 ] auto.trans
        | Some state_list -> Trans.add (state1, letter) (state2 :: state_list) auto.trans
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
        match Trans.find_opt (state1, letter) auto.trans with
        | None -> auto.trans
        | Some state_list ->
          begin
            match remove_first_state_from_list state_list state2 with
            | [] -> Trans.remove (state1, letter) auto.trans
            | state_list -> Trans.add (state1, letter) state_list auto.trans
          end
      end
    in
    { auto with trans = transitions }

  let remove_all_trans (auto : t) 
                       (state : st) 
                       (letter : lt) : t =
    let transitions = Trans.remove (state, letter) auto.trans
    in
    { auto with trans = transitions }

  let remove_start (auto : t) 
                   (state : st) : t =
    { auto with starts = remove_first_state_from_list auto.starts state }

  let remove_end (auto : t) 
                 (state : st) : t =
    { auto with ends = remove_first_state_from_list auto.ends state }

end