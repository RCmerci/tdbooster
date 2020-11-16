open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Infix
open! Core_kernel

let createTable d (title : string list) (content : string list list) =
  let module H = Dom_html in
  let table = H.createTable d in
  let head = H.createThead d in
  let headtr = H.createTr d in
  let body = H.createTbody d in
  table##.rules := Js.string "all";
  table##.frame := Js.string "box";
  Dom.appendChild head headtr;
  Dom.appendChild table head;
  Dom.appendChild table body;
  List.iter title ~f:(fun s ->
      let th = H.createTh d in
      Dom.appendChild headtr th;
      Dom.appendChild th (d##createTextNode (Js.string s)));
  List.iter content ~f:(fun l ->
      let tr = H.createTr d in
      Dom.appendChild body tr;
      List.iter l ~f:(fun c ->
          let td = H.createTd d in
          Dom.appendChild tr td;
          Dom.appendChild td (d##createTextNode (Js.string c))));
  table
