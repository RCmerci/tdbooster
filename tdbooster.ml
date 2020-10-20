open Std

(* (string * float) list: (date, num) list *)
type marketinfo =
  { title : string
  ; data : (Date.t * float) list
  }
[@@deriving to_yojson]

type output =
  { data : L3.Marketinfo.Basedata_info.t
  ; marketinfo : marketinfo list
  ; (* code date data *)
    industry_trend : L3.Marketinfo.Industry_trend_info.t
  }
[@@deriving to_yojson]

type title = string [@@deriving to_yojson]

let marketinfo config_dir =
  let open L3.Marketinfo.Other_info in
  let basedata = get_data ~config_dir in
  let info = of_data basedata in
  [ { title = "GC"; data = info.gc }
  ; { title = "HG"; data = info.hg }
  ; { title = "CL"; data = info.cl }
  ; { title = "HG/GC"; data = info.hg_div_gc }
  ; { title = "CL/GC"; data = info.cl_div_gc }
  ]

let f codes output_dir refresh_data stats backtest =
  let () =
    let open L2.Data.Store in
    if refresh_data then (
      let () =
        ReadCodesBaseData.fetch_all_codes_base_data ~config_dir:output_dir
          ~custom_codes:codes
      in
      db_delete ~config_dir:output_dir;
      let db = db_open ~config_dir:output_dir in
      let raw_data =
        ReadCodesBaseData.read_all_codes_base_data ~config_dir:output_dir
          ~custom_codes:codes
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
    ) else
      let () = db_delete ~config_dir:output_dir in
      let db = db_open ~config_dir:output_dir in
      let raw_data =
        ReadCodesBaseData.read_all_codes_base_data ~config_dir:output_dir
          ~custom_codes:codes
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
  in
  let js =
    if List.length backtest > 0 then
      Yojson.Safe.from_string "{\"backtest\": true}"
    else if List.length stats = 0 then
      let basedata, derived_data =
        L3.Marketinfo.Basedata_info.get_data ~config_dir:output_dir
          ~custom_codes:codes
      in
      let basedatainfo =
        L3.Marketinfo.Basedata_info.of_data basedata derived_data
      in
      let marketinfo = marketinfo output_dir in
      let industry_trend =
        L3.Marketinfo.Industry_trend_info.(
          of_data (get_data ~config_dir:output_dir))
      in
      output_to_yojson { data = basedatainfo; marketinfo; industry_trend }
    else
      Yojson.Safe.from_string "{\"stat\": true}"
  in
  Yojson.Safe.to_string js |> Out_channel.print_string

let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open codes = flag "-f" (listed string) ~doc:""
      and refresh_data = flag "-r" no_arg ~doc:"refresh data then save to files"
      and output_dir =
        flag "-o" (required string) ~doc:"output path of datafiles"
      and stats = flag "-s" (listed string) ~doc:"statistics"
      and backtest = flag "-t" (listed string) ~doc:"back testing a strategy" in
      fun () -> f codes output_dir refresh_data stats backtest)

let () = Command.run command
