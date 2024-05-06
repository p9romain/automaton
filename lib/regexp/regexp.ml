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
        String.concat ";" @@ List.map loop l
      | Union l ->
        "(" ^ (String.concat "|" @@ List.map loop l) ^ ")"
      | Star r -> (
          match r with
          | Letter _
          | Union _ -> loop r ^ "*"
          | _ -> "(" ^ loop r ^ ")*"
        )
      | Plus r -> (
          match r with
          | Letter _
          | Union _ -> loop r ^ "+"
          | _ -> "(" ^ loop r ^ ")+"
        )
      | Option r -> (
          match r with
          | Letter _
          | Union _ -> loop r ^ "?"
          | _ -> "(" ^ loop r ^ ")?"
        )
    in
    loop @@ flatten r



  let simplify (r : t_ext) : t_ext =
    let rec simp (r : t_ext) : t_ext =
      match r with
      | Letter _ -> r
      | Concat l ->
        let l = List.map simp l in
        (* TODO : because of flattening, Bool;Int(Bool;Int)* isn't factored (we only check neighbours) *)
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
        ( 
          match loop l with
          | [] -> failwith "Empty concat is impossible"
          | r :: [] -> r
          | l -> Concat l 
        )
      | Union l ->
        let get_rid_of_duplicate (f : 'a list -> 'a -> 'a list)
                                 (l : 'a list) =
          List.rev @@ List.fold_left f [] l
        in
        let unique_l = get_rid_of_duplicate (
            fun (acc : t_ext list)
                (r : t_ext) : t_ext list ->
              if List.mem r acc then
                acc
              else
                r :: acc
          )
          @@ List.map simp l
        in
        (
          match unique_l with
          | [] -> failwith "Empty union is impossible"
          | r :: [] -> r
          | _ -> (
            let (all_eps, without_eps) = List.partition (
              fun (r : t_ext) : bool ->
                match r with 
                | Letter lt -> Lt.is_epsilon lt 
                | _ -> false
            ) 
            unique_l
            in


            (* NEED TO MEMOIZE *)

            (* Returns the (prefixe of size len, what it is after) *)
            let rec calc_prefixe_for_regexp (r : t_ext)
                                            (len : int) : (t_ext * t_ext) option =
              if len = 0 then
                Some (Letter Lt.epsilon, Letter Lt.epsilon)
              else
                match r with
                | Concat l -> (
                  match l with
                  | [] -> None
                  | r :: next ->
                    if len = 1 then
                      Some (r, Concat next)
                    else (
                      match calc_prefixe_for_regexp (Concat next) @@ len - 1 with
                      | None -> None
                      | Some (r', next) -> 
                        Some(
                          Concat ([
                            r ;
                            r'
                          ]),
                          next
                        )
                    )
                )
                | Plus r ->
                  if len = 1 then
                    Some r
                  else (
                    match calc_prefixe_for_regexp (Star r) @@ len - 1 with
                    | None -> None
                    | Some (r', next) -> 
                      Some(
                        Concat ([
                          r ;
                          r'
                        ]),
                        next
                      )
                  )
                | _ ->
                  if len = 1 then
                    Some (r, Letter Lt.epsilon)
                  else
                    None
            in
            (* Calculates all the prefixe of size len for an union *)
            let calc_all_prefixes_for_union (l : t_ext list)
                                            (len : int) : (t_ext * t_ext) option list =
              List.fold_left (
                fun (acc : (t_ext * t_ext) option list)
                    (r: t_ext) : (t_ext * t_ext) option list ->
                  (calc_prefixe_for_regexp r len) :: acc 
              )
              [] l
            in
            (* Search the greatest prefixe for an union, and then factorize it
               (rr'|rr'') = r(r'|r'')
             *)
            (* let factorize (l : t_ext list) : t_ext =
              let max_not_found = ref true in
              let len = ref 0 in
              let cur = ref @@ calc_all_prefixes_for_union l !len in
              let max = ref @@ calc_all_prefixes_for_union l !len in
              let max_prefix = ref @@ Letter Lt.epsilon in
              let () =
                while !max_not_found do
                  let all_none, without_none = List.partition Option.is_none !max in
                  match without_none with
                  | [] -> max_not_found := false
                  | _ -> (
                    let () = len := !len + 1 in
                    match all_none with
                    | [] ->
                      let f l x = l in
                      match get_rid_of_duplicate f !max with
                      | [] -> assert false
                      | r :: [] ->



                      (* NEED TO CHECK IF ITS THE SAME PREFIX FOR ALL *)
                      max := calc_all_prefixes_for_union l !len
                    | _ -> ()
                  ) 
                done
              in
              Concat ( 
                List.cons !max_prefix 
                @@ List.map (
                    fun (cant_be_none : (t_ext * t_ext) option) : t_ext ->
                    snd @@ Option.get cant_be_none
                  ) 
                  !max 
              )
            in  *)
            match all_eps with 
            | [] -> Union without_eps (* no epsilon *)
            | _ -> (* at least one *)
              match without_eps with
              | [] -> Letter Lt.epsilon (* it was an union of espilon (why not) *)
              | r :: [] -> simp @@ Option r
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
              Star (Letter l)
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
              Plus (Letter l)
          | Star r
          | Option r -> Star r
          | Plus r -> Plus r
          | r -> Plus r
        end
      | Option r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Option (Letter l)
          | Star r
          | Plus r -> Star r
          | Option r -> Option r
          | r -> Option r
        end
    in
    simp @@ flatten r

end