type t

(** create makes empty gap buffer of given size *)
val make : int -> t

(** at returns non-gap element at given offset
    returns None if gap buffer is empty *)
val at : t -> int -> char option

val insert : t -> int -> char -> unit