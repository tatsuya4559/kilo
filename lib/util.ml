module Repeat = struct
  let times n f =
    for _ = 0 to n do
      f ()
    done
end
