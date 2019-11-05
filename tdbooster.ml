open Core
open Option.Monad_infix

let to_strategy s =
  match s with
  | "long_term" -> (module Strategy.Long_term: Strategy.Type.Strategy)
  | _ -> failwith (Printf.sprintf "unknown strategy: %s" s)

let f files strategy =
  let outputlist = List.map files ~f:(fun file ->
      let rawdata = Loader.From_tonghuashun_txt.read_from_file file in
      let module S = (val to_strategy strategy:Strategy.Type.Strategy) in
      let module E = Exec.Executor.Make(S)  in
      E.create rawdata
      >>| fun t ->
      let r = E.exec_sequence file t in
      Exec.Executor.to_output file r) |> List.filter_opt |> List.join  in
  Exec.Output.to_string outputlist |> print_string
let command =
  Command.basic ~summary:"Tdbooster"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = flag "-f" (listed string) ~doc:""
      and strategy = flag "-s" (required string) ~doc:"enum: long_term" in
      fun () -> f files strategy |> ignore)

let () = Command.run command
