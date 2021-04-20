type t

(** create buffer with initial value *)
val create : string -> t

(** rows of buffer *)
val rows : t -> int

(** cols of y row *)
val cols : t -> int -> int

(** append row after y *)
val append_row : t -> int -> string -> unit

(** delete row at y *)
val delete_row : t -> int -> unit

(** append string to row y *)
val append_string : t -> int -> string -> unit

(** join row y and y+1 *)
val join_row : t -> int -> unit

(** insert newline at x, y *)
val insert_newline : t -> y:int -> x:int -> unit

(** insert_char at x, y *)
val insert_char : t -> char -> y:int -> x:int -> unit

(** delete_char at x, y *)
val delete_char : t -> y:int -> x:int -> unit

(** get contents of buffer that starts at (`x`, `y`) and has `len` length at most.
 *  x and y are indexes from 0. *)
val get : t -> y:int -> x:int -> len:int -> string

(** single string representation of buffer *)
val to_string : t -> string
