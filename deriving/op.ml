open Core



let sub_by_date:
  Type.Derived_data.t list ->
  Date.t ->
  Date.t ->
  Type.Derived_data.t list=
  fun deriving_data start_date end_date ->
  let l = List.drop_while deriving_data
      ~f:(fun e -> Date.(e.date < start_date))
  in
  List.take_while l
    ~f:(fun e -> Date.(e.date <= end_date))


let sub_by_startdate:
  Type.Derived_data.t list ->
  Date.t ->
  Type.Derived_data.t list=
  fun deriving_data start_date ->
  sub_by_date deriving_data start_date Time.(to_date (now()) ~zone:(Zone.of_utc_offset ~hours:8))
