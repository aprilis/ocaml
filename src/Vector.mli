type 'a t

val create: 'a -> 'a t
val length: 'a t -> int
val append: 'a t -> 'a -> unit
val get: 'a t -> int -> 'a
val set: 'a t -> int -> 'a -> unit