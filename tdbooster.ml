open Core

type elem = {
  code: string;
  week_data: Filter.Type.Attributed_data.t list;
  day_data: Filter.Type.Attributed_data.t list;
  stats: Statistics.Type.display_struct list;
}[@@deriving to_yojson]

type output = {
  data: elem list
} [@@deriving to_yojson]


let stat code data stats =
  List.map stats ~f:(fun stat ->
      match stat with
      |"oversold" -> Statistics.Oversold.f code data
      | _ -> failwith ""
    )


let f codes output_dir refresh_data stats =
  let outputlist = List.map codes ~f:(fun code ->
      if refresh_data then
          Loader.From_baostock.run_py_script ~code ~output_dir;
          Loader.From_baostock_ttm.run_py_script ~code ~output_dir;
     let rawdata = Loader.From_txt.read_from_file (Filename.concat output_dir code)
         (Filename.concat output_dir (code ^ ".ttm")) in
     let week_k = Option.value_exn (Deriving.Unify.unify_week rawdata) in
     let day_k = Option.value_exn (Deriving.Unify.unify_day rawdata) in
     let week_attr = Filter.Unify.unify week_k in
     let day_attr = Filter.Unify.unify day_k in
     let stats' = stat code day_k stats in
     {code=code; week_data=week_attr; day_data=day_attr; stats=stats'})
  in
  Yojson.Safe.to_string (output_to_yojson {data=outputlist}) |> Out_channel.print_string
let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open codes = flag "-f" (listed string) ~doc:""
      and refresh_data = flag "-r" (no_arg) ~doc:"refresh data then save to files"
      and output_dir = flag "-o" (required string) ~doc:"output path of datafiles"
      and stats = flag "-s" (listed string) ~doc:"statistics" in
      fun () -> f codes output_dir refresh_data stats |> ignore)

let () = Command.run command
