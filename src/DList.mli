type 'a t = { mutable prev: 'a t option; mutable next: 'a t option; mutable item: 'a option; _id: int }

val equal: 'a t -> 'a t -> bool
val get: 'a option -> 'a
val create: unit -> 'a t
val remove: 'a t -> 'a t
val insert_after: 'a -> 'a t -> 'a t
val of_list: 'a list -> 'a t
val _begin: 'a t -> 'a t
val _end: 'a t -> 'a t
val to_list: 'a t -> 'a t -> 'a list