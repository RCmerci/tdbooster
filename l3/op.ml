open! Std

(** store data from downloaded formatted-txt to db *)
let store_data ~config_dir ~custom_codes =
  let open L2.Data.Store in
  db_delete ~config_dir;
  let db = db_open ~config_dir in
  let raw_data =
    ReadCodesBaseData.read_all_codes_base_data ~config_dir ~custom_codes
  in
  BaseData.store_day_data db raw_data;
  BaseData.store_week_data db raw_data;
  BaseData.store_month_data db raw_data;
  DerivedData.store_day_data db raw_data;
  DerivedData.store_week_data db raw_data;
  DerivedData.store_month_data db raw_data;
  IndustryTrendData.store_day_data db raw_data;
  IndustryTrendData.store_week_data db raw_data;
  IndustryTrendData.store_month_data db raw_data

let refresh_data ~config_dir ~custom_codes =
  let open L2.Data.Store in
  ReadCodesBaseData.fetch_all_codes_base_data ~config_dir ~custom_codes;
  store_data ~config_dir ~custom_codes

module type SeatchS = sig
  type t

  val create : config_dir:string -> t

  val search : string list -> Date.t -> string list
end

let search ?date ?codes (module BacktestStrategy : SeatchS) =
  let date' =
    Option.value date
      ~default:(Date.today ~zone:(Time.Zone.of_utc_offset ~hours:8))
  in
  let codes' = Option.value codes ~default:L2.Data.Const.hs300 in
  BacktestStrategy.search codes' date'
