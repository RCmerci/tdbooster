open Core
open Option.Monad_infix

let unify (data_list : Loader.Type.raw_data list) :
  Type.Derived_data.t list option =
  let datelist, ema12 = Ema.ema 12 data_list |> List.unzip in
  let _, ema20 = Ema.ema 20 data_list |> List.unzip in
  let _, ema26 = Ema.ema 26 data_list |> List.unzip in
  let _, ema60 = Ema.ema 60 data_list |> List.unzip in
  let _, ema120 = Ema.ema 120 data_list |> List.unzip in
  let _, ma20 = Ma.ma 20 data_list |> List.unzip in
  let _, ma60 = Ma.ma 60 data_list |> List.unzip in
  let _, ma120 = Ma.ma 120 data_list |> List.unzip in
  Macd.macd_dif_dea data_list
  >>= fun macd_dif_dea ->
  let rec aux datelist  ema12 ema20 ema26 ema60 ema120 macd_dif_dea
      data_list ma20 ma60 ma120 (r : Type.Derived_data.t list) =
    match
      ( datelist
      , ema12
      , ema20
      , ema26
      , ema60
      , ema120
      , macd_dif_dea
      , data_list
      , ma20
      , ma60
      , ma120)
    with
    | ( h1 :: t1
      , h2 :: t2
      , h3 :: t3
      , h4 :: t4
      , h5 :: t5
      , h6 :: t6
      , (_, macd, dif, dea) :: t7
      , h8 :: t8
      , h9 :: t9
      , h10 :: t10
      , h11 :: t11
      ) ->
      aux t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11
        ( { date= h1
          ; raw_data= h8
          ; ema12= h2
          ; ema20=h3
          ; ema26= h4
          ; ema60= h5
          ; ema120=h6
          ; ma20=h9
          ; ma60=h10
          ; ma120=h11
          ; dif
          ; dea
          ; macd }
          :: r )
    | ([],[],[],[],[],[],[],[],[],[],[]) ->
      List.rev r
    | _ ->
      Debug.amf [%here] "unequal length of data";
      failwith "unequal length of data"
  in
  Some
    (aux datelist ema12 ema20 ema26 ema60 ema120 macd_dif_dea data_list ma20 ma60 ma120 [])

let unify_day = unify

let unify_week (data_list : Loader.Type.raw_data list) =
  unify (Week_k.week_k data_list)

let unify_month (data_list : Loader.Type.raw_data list) =
  unify (Month_k.month_k data_list)
