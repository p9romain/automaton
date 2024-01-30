module type S = sig

  type lt
  type t

  val equals : t -> t -> bool

  (* val from_string : string -> lt list -> t *)

  val simplify : t -> t

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t
  type t =
    | Letter of lt
    | Concat of t * t
    | Union of t * t
    | Star of t
    | Plus of t
    | Option of t

  let rec equals (r1 : t)
                 (r2 : t) : bool =
    match r1, r2 with
    | Letter l1, Letter l2 ->
      Lt.compare l1 l2 = 0
    | Concat (r11, r12), Concat (r21, r22) -> 
      equals r11 r21 && equals r12 r22
    | Union (r11, r12), Union (r21, r22) ->
         (equals r11 r21 && equals r12 r22)
      || (equals r11 r22 && equals r12 r21)
      || (equals r12 r22 && equals r11 r21)
      || (equals r12 r21 && equals r11 r22)
    | Star r1, Star r2
    | Plus r1, Plus r2
    | Option r1, Option r2 ->
      equals r1 r2
    | _, _ ->
      false

  let rec simplify (reg : t) : t =
    match reg with
    | Letter _ -> reg
    | Concat (r1, r2) ->
      begin
        match simplify r1, simplify r2 with
        | Letter l1, Letter l2 ->
          if Lt.is_epsilon l1 && Lt.is_epsilon l2 then
            Letter l1 (* Letter (Lt.epsilon) *)
          else if Lt.is_epsilon l1 then
            Letter l2
          else if Lt.is_epsilon l2 then
            Letter l1
          else
            Concat (Letter l1, Letter l2)
        | Star r1, Star r2 ->
          if equals r1 r2 then
            Star r1
          else
            Concat (Star r1, Star r2)
        | r1, Star r ->
          if equals r1 r then
            Plus r
          else
            Concat (r1, Star r)
        | Star r, r2 ->
          if equals r r2 then
            Plus r
          else
            Concat (Star r, r2)
        | r1, r2 -> Concat (r1, r2)
      end
    | Union (r1, r2) ->
      begin
        match simplify r1, simplify r2 with
        | Letter l, r ->
          if Lt.is_epsilon l then
            Option r
          else
            Union (Letter l, r)
        | r, Letter l ->
          if Lt.is_epsilon l then
            Option r
          else
            Union (r, Letter l)
        | r1, r2 -> 
          if equals r1 r2 then
            r1
          else
            Union (r1, r2)
      end
    | Star r ->
      begin
        match simplify r with
        | Letter l ->
          if Lt.is_epsilon l then
            Letter l (* Letter (Lt.epsilon) *)
          else
            Star (Letter l)
        | Plus r'
        | Option r'
        | Star r' -> Star r'
        | r' -> Star r'
      end
    | Plus r ->
      begin
        match simplify r with
        | Star r'
        | Option r' -> Star r'
        | Plus r' -> Plus r'
        | r' -> Plus r'
      end
    | Option r ->
      begin
        match simplify r with
        | Star r'
        | Plus r' -> Star r'
        | r' -> Option r'
      end

end