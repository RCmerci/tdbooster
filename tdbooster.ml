open Core

type elem = {
  code: string;
  data: Filter.Type.Attributed_data.t list;
}[@@deriving to_yojson]

type output = {
  data: elem list
} [@@deriving to_yojson]

let f codes output_dir refresh_data =
  let outputlist = List.map codes ~f:(fun code ->
      if refresh_data then
          Loader.From_baostock.run_py_script ~code ~output_dir;
          Loader.From_baostock_ttm.run_py_script ~code ~output_dir;
     let rawdata = Loader.From_txt.read_from_file (Filename.concat output_dir code)
         (Filename.concat output_dir (code ^ ".ttm")) in
     let week_k = Option.value_exn (Deriving.Unify.unify_week rawdata) in
     let week_attr = Filter.Unify.unify week_k in
     {code=code; data=week_attr})
  in
  Yojson.Safe.to_string (output_to_yojson {data=outputlist}) |> Out_channel.print_string
let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open codes = flag "-f" (listed string) ~doc:""
      and refresh_data = flag "-r" (no_arg) ~doc:"refresh data then save to files"
      and output_dir = flag "-o" (required string) ~doc:"output path of datafiles" in
      fun () -> f codes output_dir refresh_data |> ignore)

let () = Command.run command
