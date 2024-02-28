module type S = sig

  type lt
  type t_simp
  type t_ext

  val letter : lt -> t_simp
  val concat : t_simp -> t_simp -> t_simp
  val union : t_simp -> t_simp -> t_simp
  val star : t_simp -> t_simp

  val simp_to_ext : t_simp -> t_ext
  
  val simplify : t_ext -> t_ext

  val to_string : t_ext -> string

end

module Make (Lt : Letter.Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t
  type t_simp =
    | Letter of lt
    | Concat of t_simp * t_simp
    | Union of t_simp * t_simp
    | Star of t_simp
  type t_ext =
    | Letter of lt
    | Concat of t_ext list
    | Union of t_ext list
    | Star of t_ext
    | Plus of t_ext
    | Option of t_ext



  let letter (l : lt) : t_simp =
    Letter l

  let concat (r1 : t_simp) 
             (r2 : t_simp) : t_simp =
    Concat (r1, r2)

  let union (r1 : t_simp) 
            (r2 : t_simp) : t_simp =
    Union (r1, r2)

  let star (r : t_simp) : t_simp =
    Star r



  let rec simp_to_ext (r : t_simp) : t_ext =
    match r with
    | Letter l -> Letter l
    | Concat (r1, r2) -> Concat [simp_to_ext r1; simp_to_ext r2]
    | Union (r1, r2) -> Union [simp_to_ext r1; simp_to_ext r2]
    | Star r -> Star (simp_to_ext r)



  let simplify (r : t_ext) : t_ext =
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

    and simp (r : t_ext) : t_ext =
      match r with
      | Letter _ -> r
      | Concat l ->
        let l = List.map simp l in
        let rec loop (l : t_ext list) : t_ext list =
          match l with
          | [] -> []
          | r :: [] -> r :: []
          | r1 :: r2 :: l ->
            begin
              match r1, r2 with
              | Star rr1, Star rr2 ->
                if rr1 = rr2 then
                  Star rr1 :: loop l
                else
                  List.cons r1 @@ loop @@ r2 :: l
              | Star rr1, rr2 ->
                if rr1 = rr2 then
                  Plus rr1 :: loop l
                else
                  List.cons r1 @@ loop @@ r2 :: l
              | rr1, Star rr2 ->
                if rr1 = rr2 then
                  Plus rr1 :: loop l
                else
                  List.cons r1 @@ loop @@ r2 :: l
              | _ -> 
                List.cons r1 @@ loop @@ r2 :: l
            end
        in
        Concat (loop l)
      | _ -> r
    in
    (* simp @@ flatten r *)
    flatten r



  let rec to_string (r : t_ext) : string =
    match r with
    | Letter l ->
      Lt.to_string l
    | Concat l ->
      String.concat "" @@ List.map to_string l
    | Union l ->
      "(" ^ (String.concat "|" @@ List.map to_string l) ^ ")"
    | Star r ->
      let s = to_string r in
      if String.length s = 1 then
        s ^ "*"
      else
        "(" ^ s ^ ")*"
    | Plus r ->
      let s = to_string r in
      if String.length s = 1 then
        s ^ "⁺"
      else
        "(" ^ s ^ ")⁺"
    | Option r ->
      let s = to_string r in
      if String.length s = 1 then
        s ^ "?"
      else
        "(" ^ s ^ ")?"

end