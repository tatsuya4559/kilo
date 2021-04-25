(** global state of editor *)
type t

(** create new editor state *)
val create : unit -> t option

(** read content from file *)
val open_file : t -> string -> t

(** set status message *)
val set_statusmsg : t -> string -> unit

(** wait and process keypress *)
val process_keypress : t -> unit
