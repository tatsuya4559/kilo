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
  (** create makes empty gap buffer of given size *)

  val at : t -> int -> elem option

  val insert : t -> int -> elem -> unit

  val erase : t -> int -> unit

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Make (M : ElementType) : S with type elem := M.t

module RowBuffer : S with type elem := char

module EditorBuffer : S with type elem := RowBuffer.t
