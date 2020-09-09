open Core

let industry_cat_list =
  List.map Loader.Industry.get_industry_list ~f:(fun e -> e.category)

let industry_code_list = Loader.Industry.all_codes

let industry_list = Loader.Industry.get_industry_list

let zz800 = "sh.000906"

let gc = "GC"

let cl = "CL"

let hg = "HG"

let common_codes = [ zz800; cl; gc; hg ]
