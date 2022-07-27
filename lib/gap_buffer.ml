(* row = char gapbuf 
   whole = gapbuf gapbuf
   *)

module Array = BatArray

(* いったん char gapbufとして実装してからファンクタにする *)
type t = {
  mutable buf: char option array;
  mutable gap_size: int;
  mutable gap_offset: int;
}

(** debug print for t *)
let debug t =
  let open Printf in
  let string_of_array = BatIO.to_string (Array.print (BatOption.print BatChar.print)) in
  sprintf "GapBuffer { buf = %s; gap_size = %d; gap_offset = %d; }"
    (string_of_array t.buf) t.gap_size t.gap_offset

let create size =
  {
    buf = Array.make size None;
    gap_size = size;
    gap_offset = 0;
  }

let content_length t =
  Array.length t.buf - t.gap_size

let at t offset =
  if content_length t <= offset then
    None
  else begin
    assert (offset < (Array.length t.buf - t.gap_size));
    if offset < t.gap_offset then
      match Array.get t.buf offset with
      | Some ch -> Some ch
      | None -> assert false
    else
      Array.get t.buf (offset + t.gap_size)
  end

let insert t _ elem =
  t.buf.(0) <- Some elem;
  t.gap_size <- t.gap_size - 1;
  t.gap_offset <- t.gap_offset + 1

(* let grow t =
  let size = Array.length t.buf in
  let new_size = size * 2 in
  let new_gap_size = t.gap_size + size in
  let new_gap_end = t.gap_start + new_gap_size - 1 in
  let new_buf = Array.make new_size None in
  Array.blit t.buf 0 new_buf 0 t.gap_start;
  Array.blit t.buf t.gap_end new_buf new_gap_end (size - t.gap_end);
  t.gap_size <- new_gap_size;
  t.gap_end <- new_gap_end;
  t.buf <- new_buf

let at t i =
  if i < t.gap_start then
    Option.get t.buf.(i)
  else
    Option.get t.buf.(i+t.gap_end)

let set_at t i c =
  if i < t.gap_start then
    t.buf.(i) <- Some c
  else
    t.buf.(i + t.gap_size - 1) <- Some c

let length t =
  Array.length t.buf - t.gap_size

let insert_at t i c =
  if i = t.gap_start then begin
    set_at t i c;
    t.gap_size <- t.gap_size - 1;
    t.gap_start <- t.gap_start + 1
  end else if i > t.gap_start then begin
    if i >= length t then begin
      (* e.g.) Insert F at #
         before: [A, B, C, x, x, D, E] #
                           ^  ^
                           s  e
         after:  [A, B, C, x, D, E, F]
                           ^
                           s
                           e
      *)
      for j = t.gap_end to Array.length t.buf - 2 do
        t.buf.(j) <- t.buf.(j+1)
      done;
      t.buf.(Array.length t.buf - 1) <- Some c;
      t.gap_size <- t.gap_size - 1;
      t.gap_end <- t.gap_end - 1
    end else begin
      (* e.g.) Insert G at #
         before: [A, B, C, x, x, x, D, E, #, F]
                           ^     ^
                           s     e
         after:  [A, B, C, D, E, x, x, G, F]
                                 ^  ^
                                 s  e
      *)
      let n = i + t.gap_size - t.gap_end - 1 in (* num of gaps to move *)
      for j = 0 to n - 1 do
        t.buf.(t.gap_start + j) <- t.buf.(t.gap_start + j + t.gap_size);
        t.buf.(t.gap_start + j + t.gap_size) <- None
      done;
      set_at t i c;
      t.gap_size <- t.gap_size - 1;
      t.gap_start <- t.gap_start + n;
      t.gap_end <- t.gap_end + n - 1
    end
  end else begin
    (* e.g.) Insert G at #
       before: [A, B, #, C, x, x, x, D, E, F]
                            ^     ^
                            s     e
       after:  [A, B, G, x, x, C, D, E, F]
                         ^  ^
                         s  e
    *)
    let n = t.gap_start - i in
    for j = 0 to n - 1 do
      t.buf.(i + j + t.gap_size) <- t.buf.(i + j);
      t.buf.(i + j) <- None
    done;
    set_at t i c;
    t.gap_size <- t.gap_size - 1;
    t.gap_start <- t.gap_start - n + 1;
    t.gap_end <- t.gap_end - n
  end;
  if t.gap_start >= t.gap_end then grow t

module type Stringer = sig
  type t
  val to_string : t -> string
end *)

let%test_module "gap_buffer test" = (module struct
  open Printf

  let assert_buf_equal got want =
    let is_same_buf = got.buf = want.buf in
    let is_same_gap_size = got.gap_size = want.gap_size in
    let is_same_gap_offset = got.gap_offset = want.gap_offset in
    match (is_same_buf, is_same_gap_size, is_same_gap_offset) with
    | (true, true, true) -> true
    | _ -> failwith (sprintf "gap_buffer: want %s, got %s" (debug want) (debug got))


  let%test "create" =
    let gapbuf = create 3 in
    assert_buf_equal gapbuf {
      buf = [|None; None; None|];
      gap_size = 3;
      gap_offset = 0;
    }

  let%test "at None" = 
    let gapbuf = create 3 in
    at gapbuf 1 = None

  let%test "insert at gap" =
    let gapbuf = create 5 in
    insert gapbuf 2 'a';
    assert_buf_equal gapbuf {
      buf = [|Some 'a'; None; None; None; None|];
      gap_size = 4;
      gap_offset = 1;
    }

end)
