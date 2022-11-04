let zfill width s =
  let to_fill = width - (String.length s) in
  if to_fill <= 0 then s
  else (String.make to_fill '0') ^ s

let zfill' n s =
  let length = Bytes.length s in
  if n <= length then
    s
  else
    let result = Bytes.make n '0' in
    Bytes.blit s 0 result (n-length) length;
    result

(* https://stackoverflow.com/a/74274510/202168
   NOTE: log2 8 = 3 but 8 is 0b1000 *)
let log2 x = (Float.of_int x) |> Float.log2 |> Float.ceil |> Int.of_float
(*
  fun facts:
    log base 8 is the same as (log2 x) / 3
    and 2. ** 60. = 8. ** 20.
*)
