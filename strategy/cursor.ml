open Core

type 'a t = {current: int; k_list: 'a list; k_list_len: int}

let create k_list =
  if List.is_empty k_list then None
  else Some {current= 0; k_list; k_list_len= List.length k_list}

let create_exn k_list =
  match create k_list with
  | None ->
      invalid_arg "Cursor.create_exn"
  | Some v ->
      v

let move (t : 'a t) n : 'a t * int =
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
      if t.current + n < t.k_list_len then n else t.k_list_len - 1 - t.current
    in
    List.sub t.k_list ~pos:(t.current + 1) ~len:n'

let current t = List.nth_exn t.k_list t.current

let%test_unit "test-cursor" =
  let t = create_exn [1] in
  let t1, _ = move t 1 in
  let t2 = create_exn [1; 2; 3] in
  left t1 1 |> ignore ;
  left t1 0 |> ignore ;
  right t1 1 |> ignore ;
  right t1 0 |> ignore ;
  left t2 0 |> ignore ;
  left t2 1 |> ignore ;
  left t2 2 |> ignore ;
  left t2 3 |> ignore ;
  right t2 0 |> ignore ;
  right t2 1 |> ignore ;
  right t2 2 |> ignore ;
  right t2 3 |> ignore
