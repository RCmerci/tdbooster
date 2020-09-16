open Core

let industry_cat_list =
  List.map L1.Loader.Industry.get_industry_list ~f:(fun e -> e.category)

let industry_code_list = L1.Loader.Industry.all_codes

let industry_list = L1.Loader.Industry.get_industry_list

let zz800 = "sh.000906"

let gc = "GC"

let cl = "CL"

let hg = "HG"

let common_codes = [ zz800; cl; gc; hg ]

(* utils *)

let get_industry_opt code =
  List.find industry_list ~f:(fun e ->
      List.mem e.codes code ~equal:String.equal)
  |> Option.map ~f:(fun (e : L1_loader.Type.IndustryList.one) -> e.category)
