open Core

let get_industry code =
  let open Option.Monad_infix in
  List.find L1_loader.Industry.get_industry_list ~f:(fun e ->
      List.mem e.codes code ~equal:String.equal)
  >>= fun r -> Some r.category
