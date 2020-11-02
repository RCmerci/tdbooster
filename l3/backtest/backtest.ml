open Std
module Util = Util
module Strategy2 = Strategy2
open Util

let rec run_aux state finish_day custom_codes =
  let nextstate = Strategy2.eval state custom_codes in
  if nextstate.today > finish_day then
    nextstate
  else
    run_aux nextstate finish_day custom_codes

let run config_dir init_day finish_day custom_codes =
  let basedata_db = L2.Data.Query.BaseData.create config_dir in
  let industrytrend_db = L2.Data.Query.IndustryTrendData.create config_dir in
  let deriveddata_db = L2.Data.Query.DerivedData.create config_dir in
  let basedatamap =
    L2.Data.Query.BaseData.query basedata_db ~codes:[ L2.Data.Const.zz800 ]
      ~dwm:`DAY ~selector:L2.Data.Query.Selector.ALL
  in
  let trading_days =
    Map.find_exn basedatamap L2.Data.Const.zz800
    |> Array.map ~f:(fun (e : L2.Data.Type.base_data_elem) -> e.date)
    |> Set.of_array (module Date)
  in
  let state =
    { config_dir
    ; op_day = L2.Data.Op.create ~config_dir ~dwm:`DAY ~custom_codes
    ; op_week = L2.Data.Op.create ~config_dir ~dwm:`WEEK ~custom_codes
    ; op_month = L2.Data.Op.create ~config_dir ~dwm:`MONTH ~custom_codes
    ; db = industrytrend_db
    ; db2 = basedata_db
    ; db3 = deriveddata_db
    ; today = init_day
    ; txn_history = Hashtbl.create (module String)
    ; current_txn =
        { holding_code = None
        ; transactions = []
        ; phase = Phase0
        ; trend_ever_gt_90 = false
        ; trend_ever_gt_80 = false
        ; trend_ever_gt_50 = false
        ; ever_sold_highestpart = false
        }
    ; money = Money.create 10000.
    ; trading_days
    }
  in
  let next_trading_day = nextday state in
  run_aux { state with today = next_trading_day } finish_day custom_codes
