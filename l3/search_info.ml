let search_info date config_dir : Public_type.search_info =
  let module M = Backtest.Strategy2 in
  let codes = Op.search ~date ~config_dir (module M) in
  let cond = M.condition date in
  { case = L2.Data.Op.Condition.to_string cond; codes }
