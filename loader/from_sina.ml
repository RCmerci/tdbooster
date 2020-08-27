open Core
open Lwt

module Futures = struct
  let get_futrues_data ~output_dir ~code =
    Cohttp_lwt_unix.Client.get
      (Uri.of_string
         (Printf.sprintf "https://stock2.finance.sina.com.cn/futures/api/jsonp.php/var%%20_XX=/GlobalFuturesService.getGlobalFuturesDailyKLine?symbol=%s" code)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun data ->
    Out_channel.write_all (Printf.sprintf "%s/%s" output_dir code) ~data

  let read_from_file ~output_dir ~code : Type.raw_data =
    let content  = In_channel.read_all (Filename.concat output_dir code) in
    let start = String.index_exn content '[' in
    let content' = String.drop_suffix content 2 in  
    let content'' = String.drop_prefix content' start in
    let json = Yojson.Safe.from_string content'' in
    let l = match json with
      | `List l -> l
      | _ -> []
    in
    List.map l ~f:(fun e ->
        match e with
        | `Assoc al ->
          (* {"date":"2010-07-23","open":"1195.100","high":"1203.900","low":"1183.400","close":"1187.800","volume":"19526"} *)
          ({
            date=(List.Assoc.find_exn al ~equal:(String.(=)) "date" |> Yojson.Safe.Util.to_string |> Date.of_string);
            opening=(List.Assoc.find_exn al ~equal:(String.(=)) "open" |> Yojson.Safe.Util.to_string |> Float.of_string);
            high=(List.Assoc.find_exn al ~equal:(String.(=)) "high" |> Yojson.Safe.Util.to_string |> Float.of_string);
            low=(List.Assoc.find_exn al ~equal:(String.(=)) "low" |> Yojson.Safe.Util.to_string |> Float.of_string);
            close=(List.Assoc.find_exn al ~equal:(String.(=)) "close" |> Yojson.Safe.Util.to_string |> Float.of_string);
            ttm=0.;
            days=1;
            percent_change=0.;
          }:Type.raw_data_elem)
        |  _ -> failwith "illegal data"
      ) 
end

