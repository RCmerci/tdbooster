open Core

let f code (data:Deriving.Type.Derived_data.t list): Type.display_struct =
  let open Option.Monad_infix in
  let six_month_ago = Date.add_months (Date.today ~zone:(Time.Zone.of_utc_offset ~hours:8)) (-6) in
  let sub_data = Deriving.Op.sub_by_startdate data six_month_ago in
  let low_rsi6_list = List.filter_mapi sub_data ~f:(fun i e -> if e.rsi6 < 20. then Some i else None) in
  let down_lt_5 = List.filter_map low_rsi6_list
      ~f:(fun i ->
          let n = List.nth_exn sub_data i in
          List.nth sub_data (i+1) >>= fun n_1 ->
          let ratio = (n_1.raw_data.closing -. n.raw_data.closing) /. n.raw_data.closing in
          if ratio < (-0.05) then Some (i+1) else None)
  in
  let updown = List.filter_map down_lt_5
    ~f:(fun i ->
        let n = List.nth_exn sub_data i in
        List.nth sub_data (i+1) >>= fun n_1 ->
        if n.raw_data.closing < n_1.raw_data.closing then
          Some `UP
        else Some `DOWN)
  in
  let up_col, down_col = List.fold updown ~init:(0, 0)
      ~f:(fun (up,down) e -> match e with `UP -> (up+1, down) | `DOWN -> (up, down+1))
  in
  let up_ratio = float_of_int up_col /. float_of_int (up_col + down_col) in
  {code;
   title="oversold";
   column_message=[
     {title="up"; value=string_of_int up_col};
     {title="down"; value=string_of_int down_col};
     {title="up ratio";value=string_of_float up_ratio};]}
