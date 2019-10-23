open Core
open Option.Monad_infix

let _ =
  let rawdata = Loader.From_tonghuashun_txt.read_from_file "data.txt" in
  let module E = Exec.Make (Strategy.V1) in
  E.create rawdata
  >>| fun t ->
  let r = E.exec_sequence t in
  let statstr =
    List.map r ~f:(fun er -> Exec.stat er |> Exec.show_statistics)
  in
  List.iter statstr ~f:Out_channel.print_endline
