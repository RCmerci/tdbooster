open Cursor

type 'a buy_action =
  | Buy of (Data_cursor.t * float option)
  | Buy_skip_to of ('a option * Data_cursor.t)
  | Buy_continue of 'a option
  | Buy_quit of Data_cursor.t

module type Strategy = sig
  type ctx

  val month_k_buy : Data_cursor.t -> ctx option -> ctx buy_action

  val week_k_buy : Data_cursor.t -> ctx option -> ctx buy_action

  val day_k_buy : Data_cursor.t -> ctx option -> ctx buy_action

  val sell :
       buy_c:Data_cursor.t
    -> buy_price:float
    -> Deriving.Type.Derived_data.t list (* day_k *)
    -> Deriving.Type.Derived_data.t list (* week_k *)
    -> Deriving.Type.Derived_data.t list (* month_k *)
    -> (* sell_c, sell_price *) (Data_cursor.t * float) option
end
