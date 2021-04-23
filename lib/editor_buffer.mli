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

(** get row at y *)
val get_row : t -> int -> string

(** get contents of buffer that starts at (`x`, `y`) and has `len` length at most.
 *  x and y are indexes from 0. *)
val get : t -> y:int -> x:int -> len:int -> string

(** get x position of query in row y
 * return -1 if not found *)
val get_position_in_row : t -> int -> string -> int

(** wheather char at x, y is a hard tab *)
val is_tab : t -> y:int -> x:int -> bool

(** fix x coordinate if it is in the middle of hard tab *)
val fix_pos : t -> y:int -> x:int -> int

(** single string representation of buffer *)
val to_string : t -> string
