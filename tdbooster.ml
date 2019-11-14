open Core
open Option.Monad_infix

let to_strategy s =
  match s with
  | "long_term" -> (module Strategy.Long_term: Strategy.Type.Strategy)
  | "medium_term" -> (module Strategy.Medium_term: Strategy.Type.Strategy)
  | _ -> failwith (Printf.sprintf "unknown strategy: %s" s)

let f codes strategy output_dir refresh_data =
  let outputlist = List.map codes ~f:(fun code ->
      if refresh_data then
          Loader.From_baostock.run_py_script ~code ~output_dir;
          Loader.From_baostock_ttm.run_py_script ~code ~output_dir;
     let rawdata = Loader.From_txt.read_from_file (Filename.concat output_dir code)
         (Filename.concat output_dir (code ^ ".ttm")) in
      let module S = (val to_strategy strategy:Strategy.Type.Strategy) in
      let module E = Exec.Executor.Make(S)  in
      E.create rawdata
      >>| fun t ->
      let r = E.exec_every_month code t in
      Exec.Executor.to_output code r) |> List.filter_opt |> List.join  in
  Exec.Output.to_string outputlist |> print_string
let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open codes = flag "-f" (listed string) ~doc:""
      and refresh_data = flag "-r" (no_arg) ~doc:"refresh data then save to files"
      and strategy = flag "-s" (required string) ~doc:"enum: long_term"
      and output_dir = flag "-o" (required string) ~doc:"output path of datafiles" in
      fun () -> f codes strategy output_dir refresh_data |> ignore)

let () = Command.run command
