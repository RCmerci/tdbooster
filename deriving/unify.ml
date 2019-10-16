open Core
open Option.Monad_infix

let unify (data_list : Loader.Type.raw_data list) :
    Type.derived_data list option =
  let timelist, ema5 = Ema.ema 5 data_list |> List.unzip in
  let _, ema10 = Ema.ema 10 data_list |> List.unzip in
  let _, ema12 = Ema.ema 12 data_list |> List.unzip in
  let _, ema14 = Ema.ema 14 data_list |> List.unzip in
  let _, ema26 = Ema.ema 26 data_list |> List.unzip in
  let _, ema60 = Ema.ema 60 data_list |> List.unzip in
  Macd.macd_dif_dea data_list
  >>= fun macd_dif_dea ->
  let rec aux timelist ema5 ema10 ema12 ema14 ema26 ema60 macd_dif_dea
      data_list (r : Type.derived_data list) =
    match
      ( timelist
      , ema5
      , ema10
      , ema12
      , ema14
      , ema26
      , ema60
      , macd_dif_dea
      , data_list )
    with
    | ( h1 :: t1
      , h2 :: t2
      , h3 :: t3
      , h4 :: t4
      , h5 :: t5
      , h6 :: t6
      , h7 :: t7
      , (_, macd, dif, dea) :: t8
      , h9 :: t9 ) ->
        aux t1 t2 t3 t4 t5 t6 t7 t8 t9
          ( { time= h1
            ; raw_data= h9
            ; ema5= h2
            ; ema10= h3
            ; ema12= h4
            ; ema14= h5
            ; ema26= h6
            ; ema60= h7
            ; dif
            ; dea
            ; macd }
          :: r )
    | _ ->
        List.rev r
  in
  Some
    (aux timelist ema5 ema10 ema12 ema14 ema26 ema60 macd_dif_dea data_list [])

let unify_day = unify

let unify_week (data_list : Loader.Type.raw_data list) =
  unify (Week_k.week_k data_list)

let unify_month (data_list : Loader.Type.raw_data list) =
  unify (Month_k.month_k data_list)

let%test "test-unify_day" =
  let datal =
    Loader.From_tonghuashun_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
  in
  let derived_day = Option.value_exn (unify_month datal) in
  let derived_day_1000 = List.sub (List.rev derived_day) ~pos:0 ~len:20 in
  Debug.eprint
    (List.to_string ~f:(fun e -> Type.show_derived_data e) derived_day_1000) ;
  false
