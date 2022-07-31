(* row = char gapbuf 
   whole = gapbuf gapbuf
   *)
module Array = BatArray
module Char = BatChar
module Option = BatOption

(*
offset means index in t.buf array = internal use
index means index of content elements = interface use
*)

module type ElementType = sig
  type t

  val sep : string

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module type S = sig
  type elem

  type t

  val equal : t -> t -> bool

  val make : int -> t

  val at : t -> int -> elem option

  val insert : t -> int -> elem -> unit

  val erase : t -> int -> unit

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Make (M : ElementType) : S with type elem := M.t = struct
  type t = {
    mutable buf : M.t option array;
    mutable gap_size : int;
    mutable gap_offset : int;
  }
  [@@deriving show]

  let buf_equal = Array.equal (Option.eq ~eq:M.equal)

  let equal t1 t2 =
    t1.gap_size = t2.gap_size
    && t1.gap_offset = t2.gap_offset
    && buf_equal t1.buf t2.buf

  let make size =
    { buf = Array.make size None; gap_size = size; gap_offset = 0 }

  let index_to_offset t index =
    if index <= t.gap_offset then
      index
    else
      index + t.gap_size

  let at t index =
    if index < t.gap_offset then
      t.buf.(index)
    else
      t.buf.(index + t.gap_size)

  let content_length t = Array.length t.buf - t.gap_size

  let glow_buf_size t =
    let size = Array.length t.buf in
    assert (t.gap_size = 0);
    t.buf <- Array.append t.buf (Array.make size None);
    t.gap_offset <- size;
    t.gap_size <- t.gap_size + size

  let reserve_buf_size t size =
    if Array.length t.buf >= size then
      ()
    else
      glow_buf_size t

  let move_gap t index =
    let offset = index_to_offset t index in
    if index > content_length t then
      failwith "move_gap offset out of range"
    else if offset = t.gap_offset then
      (* when gap is at the cursor *)
      ()
    else if offset < t.gap_offset then (
      (* when gap is after cursor *)
      t.buf <-
        Array.concat
          [
            Array.sub t.buf 0 offset;
            Array.sub t.buf t.gap_offset t.gap_size;
            Array.sub t.buf offset (t.gap_offset - offset);
            Array.sub t.buf
              (t.gap_offset + t.gap_size)
              (Array.length t.buf - (t.gap_offset + t.gap_size));
          ];
      t.gap_offset <- index
    ) else
      (* when gap is before cursor *)
      t.buf <-
        Array.concat
          [
            Array.sub t.buf 0 t.gap_offset;
            Array.sub t.buf
              (t.gap_offset + t.gap_size)
              (offset - (t.gap_offset + t.gap_size));
            Array.sub t.buf t.gap_offset t.gap_size;
            Array.sub t.buf offset (Array.length t.buf - offset);
          ];
    t.gap_offset <- index

  let insert t index elem =
    if index < 0 || content_length t < index then
      failwith "out of range"
    else (
      reserve_buf_size t (content_length t + 1);
      move_gap t index;
      t.buf.(index) <- Some elem;
      t.gap_size <- t.gap_size - 1;
      t.gap_offset <- t.gap_offset + 1
    )

  let erase t index =
    if index < 0 || content_length t <= index then
      failwith "out of range"
    else (
      move_gap t index;
      t.buf.(index) <- None;
      t.gap_size <- t.gap_size + 1
    )

  let to_string t =
    Array.filter_map
      (function Some e -> Some (M.to_string e) | None -> None)
      t.buf
    |> Array.to_list |> String.concat M.sep
end

module RowBuffer = Make (struct
  include Char

  type t = char [@@deriving show]

  let sep = ""

  let to_string ch = String.make 1 ch
end)

module EditorBuffer = Make (struct
  include RowBuffer

  let sep = "\n"
end)

let%test_module "RowBuffer test" =
  (module struct
    open RowBuffer

    let%test "create" =
      let gapbuf = make 3 in
      to_string gapbuf = ""

    let%test "get element before gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 'a';
      at gapbuf 0 = Some 'a'

    let%test "get element after gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 'a';
      insert gapbuf 0 'b';
      at gapbuf 1 = Some 'a'

    let%test_unit "insert at 0, 1" =
      let gapbuf = make 5 in
      insert gapbuf 0 'a';
      assert (to_string gapbuf = "a");
      insert gapbuf 1 'b';
      assert (to_string gapbuf = "ab");
      insert gapbuf 0 'c';
      assert (to_string gapbuf = "cab");
      insert gapbuf 3 'd';
      assert (to_string gapbuf = "cabd");
      insert gapbuf 4 'e';
      assert (to_string gapbuf = "cabde");
      erase gapbuf 4;
      assert (to_string gapbuf = "cabd")
  end)

let%test_module "EditorBuffer test" =
  (module struct
    open EditorBuffer

    let row = RowBuffer.make 10

    let _ =
      RowBuffer.insert row 0 'a';
      RowBuffer.insert row 1 'b'

    let%test "create" =
      let gapbuf = make 3 in
      to_string gapbuf = ""

    let%test "get element before gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 row;
      at gapbuf 0 = Some row

    let%test "get element after gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 row;
      insert gapbuf 0 row;
      at gapbuf 1 = Some row

    let%test_unit "insert at 0, 1" =
      let gapbuf = make 5 in
      insert gapbuf 0 row;
      assert (to_string gapbuf = "ab");
      insert gapbuf 1 row;
      assert (to_string gapbuf = "ab\nab");
      insert gapbuf 0 row;
      assert (to_string gapbuf = "ab\nab\nab")
  end)
