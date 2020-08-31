open Core

type size =
  { min : float
  ; max : float
  }

type t =
  { size : size
  ; value : float
  }

let create_size ~min ~max =
  assert (Float.( < ) min max);
  { min; max }

let create ~min ~max value =
  assert (Float.( <= ) min value && Float.( >= ) max value);
  { size = create_size ~min ~max; value }

let max t = t.max

let min t = t.min

let of_float size f = { size; value = f }

let normalize v = (v.value -. v.size.min) /. (v.size.max -. v.size.min) *. 100.

let sum vl =
  { size = { min = 0.; max = Float.of_int (List.length vl * 100) }
  ; value = List.sum (module Float) vl ~f:normalize
  }

let%test "test-score_normalize" =
  let s = create_size ~min:(-1000.) ~max:1000. in
  let v = of_float s 50. in
  let v2 = create ~min:0. ~max:1. 0.3 in
  Float.(
    round_decimal ~decimal_digits:1 (normalize v) = 52.5
    && round_decimal ~decimal_digits:1 (normalize v2) = 30.0)

let%test "test-score_sum" =
  let l =
    List.map
      [ (-10., 20., 5.); (-100., 200., 50.); (-20., 0., -5.); (-10., 20., -5.) ]
      ~f:(fun (min, max, v) -> create ~min ~max v)
  in
  Float.(round_decimal ~decimal_digits:1 (normalize (sum l)) = 47.9)
