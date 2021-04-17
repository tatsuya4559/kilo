module Repeat = struct
  let times n f =
    for _ = 0 to n do
      f ()
    done
end

(* TODO: smart format *)
module StringFormat = struct
  let fit ?(pad=' ') len ~left ~right =
    let left_len = String.length left in
    let right_len = String.length right in
    if (left_len + right_len) <= len then
      let padding = String.make (len - left_len - right_len) pad in
      left ^ padding ^ right
    else
      StringLabels.sub left ~pos:0 ~len:(len - right_len) ^ right
end
