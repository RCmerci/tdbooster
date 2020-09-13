open Core

type elem =
  { code : string
  ; week_data : L1_filter.Type.Attributed_data.t list
  ; day_data : L1_filter.Type.Attributed_data.t list
  }
[@@deriving to_yojson]

(* (string * float) list: (date, num) list *)
type marketinfo =
  { title : string
  ; data : (string * float) list
  }
[@@deriving to_yojson]

type output =
  { data : elem list
  ; marketinfo : marketinfo list
  ; (* code date data *)
    industry_trend : (string * (string * float) list) list
  }
[@@deriving to_yojson]

type title = string [@@deriving to_yojson]

type stat_output = { data : (title * Statistics.Type.display_struct list) list }
[@@deriving to_yojson]

let stat code day_k _week_k stats =
  List.map stats ~f:(fun stat ->
      match stat with
      | "oversold" -> Statistics.Oversold.f code day_k
      | _ -> failwith "")

let filter code day_k week_k zz800_day_k zz800_week_k =
  let day_attr = L1_filter.Unify.unify code zz800_day_k day_k in
  let week_attr = L1_filter.Unify.unify code zz800_week_k week_k in
  { code; week_data = week_attr; day_data = day_attr }

let marketinfo m =
  let _, _, gc_day_k = Map.find_exn m "GC" in
  let _, _, hg_day_k = Map.find_exn m "HG" in
  let _, _, cl_day_k = Map.find_exn m "CL" in
  let info = L1_filter.Unify.marketinfo hg_day_k gc_day_k cl_day_k in
  [ { title = "GC"; data = info.gc }
  ; { title = "HG"; data = info.hg }
  ; { title = "CL"; data = info.cl }
  ; { title = "HG/GC"; data = info.hg_div_gc }
  ; { title = "CL/GC"; data = info.cl_div_gc }
  ]

let backtest code strategy rawdata =
  let module S = Exec.Executor.Make (Strategy.Medium_term) in
  let exec_t = Option.value_exn (S.create rawdata) in
  S.exec_sequence strategy exec_t
  |> Exec.Executor.to_output code
  |> Exec.Output.to_yojson

let refresh_data_aux codes output_dir =
  let module T = Lwt_throttle.Make (Unit) in
  let throttler = T.create ~rate:4 ~max:99999 ~n:1 in
  let open Lwt.Infix in
  let (_ : unit list) =
    Lwt_list.map_p
      (fun code ->
        let get_data =
          match code with
          | "GC"
          | "HG"
          | "CL" ->
            L1_loader.From_sina.Futures.get_futrues_data
          | _ ->
            fun ~output_dir ~code ->
              L1_loader.From_baostock.run_py_script ~code ~output_dir
              >>= fun _ ->
              L1_loader.From_baostock_ttm.run_py_script ~code ~output_dir
              >>= fun _ -> Lwt.return_unit
        in
        T.wait throttler () >>= fun _ -> get_data ~code ~output_dir)
      codes
    |> Lwt_main.run
  in
  ()

let f codes output_dir refresh_data stats backtest =
  let all_codes =
    List.concat [ codes; L1_loader.Industry.all_codes; Data.Const.common_codes ]
    |> List.stable_dedup
  in
  if refresh_data then refresh_data_aux all_codes output_dir;
  let mapf code =
    let read_from_file =
      match code with
      | "GC"
      | "HG"
      | "CL" ->
        L1_loader.From_sina.Futures.read_from_file
      | _ ->
        fun ~output_dir ~code ->
          L1_loader.From_txt.read_from_file
            (Filename.concat output_dir code)
            (Filename.concat output_dir (code ^ ".ttm"))
    in
    let rawdata = read_from_file ~output_dir ~code in
    let raw_day_k = rawdata in
    let week_k =
      Option.value_exn
        ~message:(Printf.sprintf "code: %s" code)
        (L1_deriving.Unify.unify_week rawdata)
    in
    let day_k =
      Option.value_exn
        ~message:(Printf.sprintf "code: %s" code)
        (L1_deriving.Unify.unify_day rawdata)
    in
    (code, (day_k, week_k, raw_day_k))
  in
  let k =
    List.map all_codes ~f:(fun code ->
        try mapf code
        with _ ->
          refresh_data_aux [ code ] output_dir;
          mapf code)
  in
  let m = Map.of_alist_exn (module String) k in
  let selected_m =
    Map.filteri m ~f:(fun ~key ~data:_data ->
        List.exists codes ~f:(equal_string key))
  in
  let js =
    if List.length backtest > 0 then
      Yojson.Safe.from_string "{\"backtest\": true}"
    else if List.length stats = 0 then
      let zz800_day_k, zz800_week_k, _ = Map.find_exn m Data.Const.zz800 in
      let data =
        Map.mapi selected_m ~f:(fun ~key:code ~data:(day_k, week_k, _) ->
            try filter code day_k week_k zz800_day_k zz800_week_k
            with e ->
              failwith (Printf.sprintf "code: %s, %s" code (Exn.to_string e)))
        |> Map.data
      in
      let marketinfo = marketinfo m in
      let industry_trend = L1_filter.Unify.industry_trend m in
      output_to_yojson { data; marketinfo; industry_trend }
    else
      let data =
        Map.mapi m ~f:(fun ~key:code ~data:(day_k, week_k, _) ->
            stat code day_k week_k stats)
        |> Map.data |> List.join
        |> List.map ~f:(fun data -> (data.title, data))
        |> Map.of_alist_multi (module String)
        |> Map.to_alist
      in
      stat_output_to_yojson { data }
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
