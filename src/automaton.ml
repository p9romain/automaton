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

  (* val remove_state : t -> st -> t
  val remove_one_trans : t -> st -> lt -> st -> t
  val remove_all_trans : t -> st -> lt -> t
  val remove_start : t -> st -> t
  val remove_end : t -> st -> t

  val clear : t -> t


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
                (s : st) : t =
    { auto with states = s :: auto.states }

  let add_trans (auto : t) 
                (s1 : st) 
                (letter : lt )
                (s2 : st) : t =
    let transitions =
      begin
        match Trans.find_opt (s1, letter) auto.trans with
        | None -> Trans.add (s1, letter) [ s2 ] auto.trans
        | Some stl -> Trans.add (s1, letter) (s2 :: stl) auto.trans
      end
    in
    { auto with trans = transitions }

  let add_start (auto : t) 
                (s : st) : t =
    { auto with starts = s :: auto.starts }

  let add_end (auto : t) 
              (s : st) : t =
    { auto with ends = s :: auto.ends }

end