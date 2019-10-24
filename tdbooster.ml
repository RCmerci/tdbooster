open Core
open Option.Monad_infix

let f files =
  List.map files ~f:(fun file ->
      Out_channel.printf "%s:\n" file ;
      Out_channel.flush Out_channel.stdout ;
      let rawdata = Loader.From_tonghuashun_txt.read_from_file file in
      let module E = Exec.Make (Strategy.V1) in
      E.create rawdata
      >>| fun t ->
      let r = E.exec_sequence t in
      let statstr =
        List.map r ~f:(fun er -> Exec.stat er |> Exec.show_statistics)
      in
      List.iter statstr ~f:Out_channel.print_endline )

let command =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open files = flag "-f" (listed string) ~doc:"" in
      fun () -> f files |> ignore)

let () = Command.run command
