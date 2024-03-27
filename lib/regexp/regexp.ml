module type S = sig

  type lt

  type t_simp
  type t_ext

  val empty : t_simp
  val is_empty : t_simp -> bool

  val letter : lt -> t_simp
  val concat : t_simp -> t_simp -> t_simp
  val union : t_simp -> t_simp -> t_simp
  val star : t_simp -> t_simp

  val simp_to_ext : t_simp -> t_ext
  
  val to_string : t_ext -> string

  val simplify : t_ext -> t_ext

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t

  type t_simp =
    | S_Empty
    | S_Letter of lt
    | S_Concat of t_simp * t_simp
    | S_Union of t_simp * t_simp
    | S_Star of t_simp
  type t_ext =
    | Letter of lt
    | Concat of t_ext list
    | Union of t_ext list
    | Star of t_ext
    | Plus of t_ext
    | Option of t_ext



  let empty = S_Empty

  let is_empty (r : t_simp) : bool =
    r = empty



  let letter (l : lt) : t_simp =
    S_Letter l

  let concat (r1 : t_simp) 
             (r2 : t_simp) : t_simp =
    S_Concat (r1, r2)

  let union (r1 : t_simp) 
            (r2 : t_simp) : t_simp =
    S_Union (r1, r2)

  let star (r : t_simp) : t_simp =
    S_Star r
 


  let rec simp_to_ext (r : t_simp) : t_ext =
    match r with
    | S_Empty -> failwith "Can't convert the empty regex"
    | S_Letter l -> Letter l
    | S_Concat (r1, r2) -> Concat [simp_to_ext r1; simp_to_ext r2]
    | S_Union (r1, r2) -> Union [simp_to_ext r1; simp_to_ext r2]
    | S_Star r -> Star (simp_to_ext r)



  let rec flatten (r : t_ext) : t_ext =
    match r with
    | Letter _ -> r
    | Concat l ->
      let rec loop (l : t_ext list) : t_ext list =
        match l with
        | [] -> []
        | r :: l ->
          begin
            match r with
            | Concat ll ->
              List.append ll @@ loop l
            | _ -> r :: loop l
          end
      in
      Concat (loop @@ List.map flatten l)
    | Union l ->
      let rec loop (l : t_ext list) : t_ext list =
        match l with
        | [] -> []
        | r :: l ->
          begin
            match r with
            | Union ll ->
              List.append ll @@ loop l
            | _ -> r :: loop l
          end
      in
      Union (loop @@ List.map flatten l)
    | Star r -> Star (flatten r)
    | Plus r -> Plus (flatten r)
    | Option r -> Option (flatten r)

  let to_string (r : t_ext) : string =
    let rec loop (r : t_ext) : string =
      match r with
      | Letter l ->
        Lt.to_string l
      | Concat l ->
        String.concat "" @@ List.map loop l
      | Union l ->
        "(" ^ (String.concat "|" @@ List.map loop l) ^ ")"
      | Star r ->
        let s = loop r in
        if String.length s = 1 then
          s ^ "*"
        else
          "(" ^ s ^ ")*"
      | Plus r ->
        let s = loop r in
        if String.length s = 1 then
          s ^ "⁺"
        else
          "(" ^ s ^ ")⁺"
      | Option r ->
        let s = loop r in
        if String.length s = 1 then
          s ^ "?"
        else
          "(" ^ s ^ ")?"
    in
    loop @@ flatten r


  let simplify (r : t_ext) : t_ext =
    let rec simp (r : t_ext) : t_ext =
      match r with
      | Letter _ -> r
      | Concat l ->
        let l = List.map simp l in
        let rec loop (l : t_ext list) : t_ext list =
          match l with
          | []
          | _ :: [] -> l
          | r1 :: r2 :: l ->
            match r1, r2 with
            | Star rr1, Star rr2 ->
              if rr1 = rr2 then
                loop @@ (Star rr1) :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Star rr1, rr2
            | rr1, Star rr2 ->
              if rr1 = rr2 then
                loop @@ (simp @@ Plus rr1) :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Letter lt1, Letter lt2 ->
                if Lt.is_epsilon lt1 then
                  loop @@ r2 :: l
                else if Lt.is_epsilon lt2 then
                  loop @@ r1 :: l
                else
                  List.cons r1 @@ loop @@ r2 :: l
            | Letter lt, _ ->
              if Lt.is_epsilon lt then
                loop @@ r2 :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | _, Letter lt ->
              if Lt.is_epsilon lt then
                loop @@ r1 :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | _ ->
              List.cons r1 @@ loop @@ r2 :: l
        in
        Concat (loop l) 
      | Union l ->
        (* TODO : the hardest because need to play with commutativity and associativity..... *)
        let l = List.map simp l in
        (
          match l with
          | [] -> failwith "Empty union is impossible"
          | r :: [] -> r
          | _ -> (
            let (with_eps, without_eps) = List.partition (
              fun (r : t_ext) : bool ->
                match r with 
                | Letter lt -> Lt.is_epsilon lt 
                | _ -> false
            ) 
            l
            in
            match with_eps with 
            | [] -> Union l
            | _ -> 
              match without_eps with
              | [] -> Letter Lt.epsilon (* it was an union of espilon (why not) *)
              | r :: [] -> simp (Option r)
              | _ -> simp @@ Option (Union without_eps)
          )
        )
      | Star r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Star r
          | Star r
          | Plus r
          | Option r -> Star r
          | r -> Star r
        end
      | Plus r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Plus r
          | Star r
          | Option r -> Star r
          | Plus r -> Plus r
          | r -> Plus r
        end
      | Option r ->
        begin
          match simp r with
          | Star r
          | Plus r -> Star r
          | r -> Option r
        end
    in
    simp @@ flatten r

end