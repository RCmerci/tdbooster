open Cursor

type ('a, 'ctx_to_next_level) buy_action =
  | Buy of (Data_cursor.t * float option * 'ctx_to_next_level)
  | Buy_skip_to of ('a option * Data_cursor.t)
  | Buy_continue of 'a option
  | Buy_quit of Data_cursor.t

module type Strategy = sig
  type ctx

  type month_to_week_ctx

  type week_to_day_ctx

  type day_to_sell_ctx

  val month_k_buy :
       Data_cursor.t
    -> ctx option
    -> unit (* to keep 3 buy func similar type *)
    -> (ctx, month_to_week_ctx) buy_action Log_warning.LogAndWarnWriter.m

  val week_k_buy :
       Data_cursor.t
    -> ctx option
    -> month_to_week_ctx
    -> (ctx, week_to_day_ctx) buy_action Log_warning.LogAndWarnWriter.m

  val day_k_buy :
       Data_cursor.t
    -> ctx option
    -> week_to_day_ctx
    -> (ctx, day_to_sell_ctx) buy_action Log_warning.LogAndWarnWriter.m

  val sell :
       buy_c:Data_cursor.t
    -> buy_price:float
    -> day_to_sell_ctx
    -> L1_deriving.Type.Derived_data.t list (* day_k *)
    -> L1_deriving.Type.Derived_data.t list (* week_k *)
    -> L1_deriving.Type.Derived_data.t list (* month_k *)
    -> (* sell_c, sell_price *)
       (Data_cursor.t * float) option Log_warning.LogAndWarnWriter.m
end
