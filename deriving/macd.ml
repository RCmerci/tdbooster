open Core

(* https://baike.baidu.com/item/MACD%E6%8C%87%E6%A0%87?fromtitle=MACD&fromid=3334786 *)

let dif ema12_list ema26_list : float list option =
  match
    List.map2 ema12_list ema26_list ~f:(fun ema12 ema26 -> ema12 -. ema26)
  with
  | List.Or_unequal_lengths.Ok v ->
      Some v
  | List.Or_unequal_lengths.Unequal_lengths ->
      Debug.amf [%here] "unequal length ema12_list ema26_list" ;
      None

(* let dea dif_list =  *)
