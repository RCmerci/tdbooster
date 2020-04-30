(* open Core
 * open Poly
 * open Owl
 * open Option.Monad_infix
 * 
 * (\* volume of `end'` great than `percent` volumes in [`start`, `end'`] *\)
 * let gt (start:Date.t) (end':Date.t) cursor (percent:float) =
 *   Cursor.Data_cursor.goto_date cursor end' >>= fun end_c ->
 *   let sub = Cursor.Data_cursor.sub ~end_date:end' start cursor in
 *   None *)
