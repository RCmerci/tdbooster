open Core
open Option.Monad_infix

let unify (data_list : L1_loader.Type.raw_data)
    (zz800_data_list : L1_loader.Type.raw_data) :
    Type.Derived_data.t list option =
  let datelist, ema12 = Ema.ema 12 data_list |> List.unzip in
  let _, ema20 = Ema.ema 20 data_list |> List.unzip in
  let _, ema26 = Ema.ema 26 data_list |> List.unzip in
  let _, ema60 = Ema.ema 60 data_list |> List.unzip in
  let _, ema120 = Ema.ema 120 data_list |> List.unzip in
  let _, ma20 = Ma.ma 20 data_list |> List.unzip in
  let _, ma60 = Ma.ma 60 data_list |> List.unzip in
  let _, ma120 = Ma.ma 120 data_list |> List.unzip in
  let _, bias24 = Bias.bias 24 data_list |> List.unzip in
  let _, rsi6 = Rsi.rsi 6 data_list |> List.unzip in
  let _, rsi12 = Rsi.rsi 12 data_list |> List.unzip in
  let _, rsi24 = Rsi.rsi 24 data_list |> List.unzip in
  let _, kdj933 = Kdj.kdj 9 data_list |> List.unzip in
  let _, rel5 = Rel.rel 5 data_list zz800_data_list |> List.unzip in
  let _, rel20 = Rel.rel 20 data_list zz800_data_list |> List.unzip in
  let _, rel60 = Rel.rel 60 data_list zz800_data_list |> List.unzip in
  let _, rel120 = Rel.rel 120 data_list zz800_data_list |> List.unzip in
  Macd.macd_dif_dea data_list >>= fun macd_dif_dea ->
  let rec aux datelist ema12 ema20 ema26 ema60 ema120 macd_dif_dea data_list
      ma20 ma60 ma120 bias24 rsi6 rsi12 rsi24 kdj933 rel5 rel20 rel60 rel120
      (r : Type.Derived_data.t list) =
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
      , ma120
      , bias24
      , rsi6
      , rsi12
      , rsi24
      , kdj933
      , rel5
      , rel20
      , rel60
      , rel120 )
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
      , h12 :: t12
      , h13 :: t13
      , h14 :: t14
      , h15 :: t15
      , h16 :: t16
      , h17 :: t17
      , h18 :: t18
      , h19 :: t19
      , h20 :: t20 ) ->
      aux t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
        ( { date = h1
          ; raw_data = h8
          ; ema12 = h2
          ; ema20 = h3
          ; ema26 = h4
          ; ema60 = h5
          ; ema120 = h6
          ; ma20 = h9
          ; ma60 = h10
          ; ma120 = h11
          ; bias24 = h12
          ; rsi6 = h13
          ; rsi12 = h14
          ; rsi24 = h15
          ; kdj933 = h16
          ; dif
          ; dea
          ; macd
          ; rel5 = h17
          ; rel20 = h18
          ; rel60 = h19
          ; rel120 = h20
          }
        :: r )
    | ( []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , []
      , [] ) ->
      List.rev r
    | _ ->
      Debug.amf [%here] "unequal length of data";
      failwith "unequal length of data"
  in
  Some
    (aux datelist ema12 ema20 ema26 ema60 ema120 macd_dif_dea data_list ma20
       ma60 ma120 bias24 rsi6 rsi12 rsi24 kdj933 rel5 rel20 rel60 rel120 [])

let unify_day = unify

let unify_week (data_list : L1_loader.Type.raw_data) =
  unify (Week_k.week_k data_list)

let unify_month (data_list : L1_loader.Type.raw_data) =
  unify (Month_k.month_k data_list)
