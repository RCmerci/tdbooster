open Core

type elem = {
  code: string;
  week_data: Filter.Type.Attributed_data.t list;
  day_data: Filter.Type.Attributed_data.t list;
}[@@deriving to_yojson]


(* 
   (string * float) list:  (date, num) list
 *)
type marketinfo = {
  title: string;
  data: (string * float) list;
}[@@deriving to_yojson]

type output = {
  data: elem list;
  marketinfo: marketinfo list;
  (*                code      date     data *)
  industry_trend: (string * (string * float) list) list;
} [@@deriving to_yojson]

type title = string [@@deriving to_yojson]

type stat_output = {
  data: (title * Statistics.Type.display_struct list) list;
} [@@deriving to_yojson]

let stat code day_k _week_k stats =
  List.map stats ~f:(fun stat ->
      match stat with
      |"oversold" -> Statistics.Oversold.f code day_k
      | _ -> failwith ""
    )

let filter code day_k week_k =
  let day_attr = Filter.Unify.unify day_k in
  let week_attr = Filter.Unify.unify week_k in
  {code;week_data=week_attr;day_data=day_attr}

let marketinfo m =
  let _, _, gc_day_k = Map.find_exn m "GC" in
  let _, _, hg_day_k = Map.find_exn m "HG" in
  let _, _, cl_day_k = Map.find_exn m "CL" in
  let info = Filter.Unify.marketinfo hg_day_k gc_day_k cl_day_k in
  [
    {
      title="GC";
      data=info.gc;
    };
    {
      title="HG";
      data=info.hg;
    };
    {
      title="CL";
      data=info.cl;
    };    
    {
      title="HG/GC";
      data=info.hg_div_gc;
    };
    {
      title="CL/GC";
      data=info.cl_div_gc;
    };
    
  ]
  

let backtest code strategy rawdata =
  let module S = Exec.Executor.Make(Strategy.Medium_term) in
  let exec_t = Option.value_exn (S.create rawdata) in
  S.exec_sequence strategy exec_t
  |> Exec.Executor.to_output code
  |> Exec.Output.to_yojson
  
let f codes output_dir refresh_data stats backtest =
  let all_codes = List.concat [codes;Loader.Industry.all_codes] |> List.stable_dedup in
  let k = List.map all_codes ~f:(fun code ->
      let get_data, read_from_file = match code with
        | "GC" | "HG" | "CL" -> (Loader.From_sina.Futures.get_futrues_data, Loader.From_sina.Futures.read_from_file)
        | _ -> ((fun ~output_dir ~code -> (Loader.From_baostock.run_py_script ~code ~output_dir;
                                           Loader.From_baostock_ttm.run_py_script ~code ~output_dir;)),
                (fun ~output_dir ~code ->
                   Loader.From_txt.read_from_file (Filename.concat output_dir code) (Filename.concat output_dir (code ^ ".ttm"))))
      in
      if refresh_data then get_data ~code ~output_dir;
      let rawdata = read_from_file ~output_dir ~code in
      let raw_day_k = Owl.Dataframe.to_rows rawdata in
      let week_k = Option.value_exn (Deriving.Unify.unify_week rawdata) in
      let day_k = Option.value_exn (Deriving.Unify.unify_day rawdata) in
      (code, (day_k, week_k, raw_day_k)))
  in
  let m = Map.of_alist_exn (module String) k in
  let selected_m = Map.filteri m ~f:(fun ~key ~data:_data -> List.exists codes ~f:(equal_string key)) in
  let js =
    if List.length backtest > 0
    then
      Yojson.Safe.from_string "{\"backtest\": true}"
    else if List.length stats = 0
    then
      let data = Map.mapi selected_m  ~f:(fun ~key:code ~data:(day_k, week_k, _) -> filter code day_k week_k) |> Map.data in
      let marketinfo = marketinfo m in
      let industry_trend = Filter.Unify.industry_trend m in
      output_to_yojson {data; marketinfo; industry_trend}
    else let data =
           Map.mapi m ~f:(fun ~key:code ~data:(day_k, week_k, _) -> stat code day_k week_k stats)
           |> Map.data
           |> List.join
           |> List.map ~f:(fun data -> (data.title, data))
           |> Map.of_alist_multi (module String)
           |> Map.to_alist
      in
      stat_output_to_yojson {data}
  in
  Yojson.Safe.to_string js |> Out_channel.print_string



let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open codes = flag "-f" (listed string) ~doc:""
      and refresh_data = flag "-r" (no_arg) ~doc:"refresh data then save to files"
      and output_dir = flag "-o" (required string) ~doc:"output path of datafiles"
      and stats = flag "-s" (listed string) ~doc:"statistics"
      and backtest = flag "-t" (listed string) ~doc:"back testing a strategy" in
      fun () -> f codes output_dir refresh_data stats backtest;)

let () = Command.run command
