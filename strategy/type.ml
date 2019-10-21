open Cursor

type 'a buy_action =
  | Buy of Data_cursor.t
  | Buy_skip_to of ('a option * Data_cursor.t)
  | Buy_continue of 'a option

type 'a sell_action = Sell of Data_cursor.t | Sell_keep

module type Strategy = sig
  val month_k_buy : Data_cursor.t -> 'a option -> 'a buy_action

  val week_k_buy : Data_cursor.t -> 'a option -> 'a buy_action

  val day_k_buy : Data_cursor.t -> 'a option -> 'a buy_action

  val sell : buy_c:Data_cursor.t -> 'a sell_action
end
