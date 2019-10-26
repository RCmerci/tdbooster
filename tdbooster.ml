open Core
open Option.Monad_infix

let f files =
  let outputlist = List.map files ~f:(fun file ->
      let rawdata = Loader.From_tonghuashun_txt.read_from_file file in
      let module E = Exec.Executor.Make (Strategy.V1) in
      E.create rawdata
      >>| fun t ->
      let r = E.exec_sequence file t in
      Exec.Executor.to_output file r) |> List.filter_opt |> List.join  in
  Exec.Output.to_string outputlist |> print_string
let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = flag "-f" (listed string) ~doc:"" in
      fun () -> f files |> ignore)

let () = Command.run command
