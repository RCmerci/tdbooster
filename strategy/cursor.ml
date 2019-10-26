open Core

module type Data_with_timestamp = sig
  type t

  val date : t -> Date.t

  val to_string : t -> string
end

module Make (Data : Data_with_timestamp) = struct
  type t = {current: int; k_list: Data.t list; k_list_len: int}

  let to_string t = List.nth_exn t.k_list t.current |> Data.to_string

  let create k_list =
    if List.is_empty k_list then None
    else Some {current= 0; k_list; k_list_len= List.length k_list}

  let create_exn k_list =
    match create k_list with
    | None ->
        invalid_arg "Cursor.create_exn"
    | Some v ->
        v

  let move t n : t * int =
    let dst = t.current + n in
    if dst <= 0 then ({t with current= 0}, -t.current)
    else if dst >= t.k_list_len then
      ({t with current= t.k_list_len - 1}, t.k_list_len - 1 - t.current)
    else ({t with current= dst}, n)

  let left t n =
    if n < 0 then []
    else
      let left_index = if t.current - n >= 0 then t.current - n else 0 in
      List.slice t.k_list left_index t.current

  let right t n =
    if n < 0 then []
    else
      let n' =
        if t.current + n < t.k_list_len then n
        else t.k_list_len - 1 - t.current
      in
      List.sub t.k_list ~pos:(t.current + 1) ~len:n'

  let current t = List.nth_exn t.k_list t.current

  let date t = Data.date (current t)

  let datestring t = Date.to_string (date t)

  type hint = [`Day | `Week | `Month]
  (*
     dst >= date
     examples:
     goto_date [1;2;4] 3 = Some 4
     goto_date [1;2;3] 3 = Some 3
     goto_date [2;3] 1 = Some 2
     goto_date [1;2;3] 4 = None
 *)

  let goto_date ?(hint=`Day)t date =
    let diff = Date.diff date (Data.date (current t)) in
    let diff' = match hint with
      | `Day -> diff
      | `Week -> diff / 5
      | `Month -> diff / 20
    in
    let t', _ = move t diff' in
    let rec tuning t fasttime =
      match Date.compare (Data.date (current t)) date with
      | 0 ->
          Some t
      | a when a > 0 ->
          let left_one, n = move t (if fasttime>0 then (-10) else (-1)) in
          if n = 0 then Some t
          else if Data.date (current left_one) < date then Some t
          else tuning left_one (fasttime-1)
      | _ ->
        let right_one, n = move t (if fasttime>0 then 10 else 1) in
          if n = 0 then None
          else if Data.date (current right_one) > date then Some right_one
          else tuning right_one (fasttime-1)
    in
    tuning t' 10

  (* [t, end'] *)
  let fold ?end' t ~f ~init =
    let rec aux r t =
      match end' with
      | Some end_t when end_t.current < t.current ->
          r
      | _ -> (
          let r' = f r t in
          let t', n = move t 1 in
          match n with 0 -> r' | _ -> aux r' t' )
    in
    aux init t

  let find ?end' t ~f =
    let rec aux t =
      match end' with
      | Some end_t when end_t.current < t.current ->
          None
      | _ -> (
          if f t then Some t
          else
            let t', n = move t 1 in
            match n with 0 -> None | _ -> aux t' )
    in
    aux t

  let to_k_list t = t.k_list
end

let%test_unit "test-cursor" =
  let module D = struct
    type t = Time.t

    let date t = Date.of_time t ~zone:(Time.Zone.of_utc_offset ~hours:8)

    let to_string _ = ""
  end in
  let module C = Make (D) in
  let now = Time.now () in
  let now_1 = Time.add now (Time.Span.of_sec 1.) in
  let now_2 = Time.add now (Time.Span.of_sec 2.) in
  let now_3 = Time.add now (Time.Span.of_sec 3.) in
  let t = C.create_exn [now] in
  let t1, _ = C.move t 1 in
  let t2 = C.create_exn [now; now_1; now_2; now_3] in
  C.left t1 1 |> ignore ;
  C.left t1 0 |> ignore ;
  C.right t1 1 |> ignore ;
  C.right t1 0 |> ignore ;
  C.left t2 0 |> ignore ;
  C.left t2 1 |> ignore ;
  C.left t2 2 |> ignore ;
  C.left t2 3 |> ignore ;
  C.right t2 0 |> ignore ;
  C.right t2 1 |> ignore ;
  C.right t2 2 |> ignore ;
  C.right t2 3 |> ignore

let%test "test-cursor-goto_date" =
  let module D = struct
    type t = Date.t

    let date t = t

    let to_string _ = ""
  end in
  let module C = Make (D) in
  let days =
    List.map ~f:Date.of_string
      ["2012-11-01"; "2012-12-02"; "2012-12-04"; "2012-12-05"]
  in
  let data = C.create_exn days in
  let dst1 =
    Option.value_exn (C.goto_date data (Date.of_string "2012-12-03"))
  in
  let dst2 =
    Option.value_exn (C.goto_date data (Date.of_string "2011-11-11"))
  in
  let dst3 = C.goto_date data (Date.of_string "2012-12-06") in
  C.current dst1 = Date.of_string "2012-12-04"
  && C.current dst2 = Date.of_string "2012-11-01"
  && Option.is_none dst3

module Data_cursor = Make (Deriving.Type.Derived_data)
