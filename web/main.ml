open! Js_of_ocaml
open! Js_of_ocaml_lwt
open Lwt.Infix
open! Core_kernel

let get_gist_updated_time =
  XmlHttpRequest.get
    "https://api.github.com/gists/10f9f945f225e4340e3cd31c5c051088"
  >>= fun r ->
  Lwt.return r.content >>= fun c ->
  let json = Yojson.Safe.from_string c in
  Lwt.return Yojson.Safe.Util.(member "updated_at" json |> to_string)

let get_tdbooster_output_gist =
  XmlHttpRequest.get
    "https://gist.githubusercontent.com/RCmerci/10f9f945f225e4340e3cd31c5c051088/raw"
  >>= fun r ->
  Lwt.return r.content >>= fun c ->
  let r = Yojson.Safe.from_string c |> Public_type.output_of_yojson in
  match r with
  | Ok output -> Lwt.return output
  | Error e -> Lwt.fail_with e

let industry_trend_cols
    (industry_trend : Public_type.Marketinfo_industry_trend_info.t) =
  let cols =
    List.init
      (List.length (snd (List.nth_exn industry_trend 0)))
      ~f:(fun i ->
        [ Date.to_string
            (fst (List.nth_exn (snd (List.nth_exn industry_trend 0)) i))
        ])
  in
  let cols' =
    List.fold industry_trend ~init:cols ~f:(fun r (_, ll) ->
        List.map2_exn ll r ~f:(fun (_, ll') cols' ->
            Int.to_string (Float.to_int ll') :: cols'))
  in
  List.map cols' ~f:List.rev

let start _ =
  let d = Dom_html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "body")) (fun () -> assert false)
  in
  get_tdbooster_output_gist >>= fun output ->
  let title =
    "" :: List.init (List.length output.industry_trend) ~f:Int.to_string
  in
  let cols = industry_trend_cols output.industry_trend in
  let table = Table.createTable d title cols in
  get_gist_updated_time >>= fun updated_at ->
  let updatetime = Dom_html.createH6 d in
  Dom.appendChild updatetime (d##createTextNode (Js.string updated_at));
  Dom.appendChild body updatetime;
  Lwt.return (Dom.appendChild body table)

let _ =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        ignore (start ());
        Js._false)
