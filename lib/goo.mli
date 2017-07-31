type -'a goo
type goo_object = [`goo_object]

type -'a events
type 'a handler = [`Handler of 'a events]

type ('self,'events) delegate = 'self goo -> 'events -> bool
  constraint 'self = [> 'events handler]

val get_handle : [> goo_object] goo -> int
(*val get_tag : [> `Tag of 'a ] goo -> 'a option
val set_tag : [> `Tag of 'a ] goo -> 'a option -> unit*)
val set_handler : 'self goo -> ('self, 'events) delegate -> unit

val retain : _ goo -> unit
val release : _ goo -> unit

val compare : _ goo -> _ goo -> int
val equal : _ goo -> _ goo -> bool
val hash : _ goo -> int

type 'a witness

val cast : _ goo -> 'a witness -> 'a goo option
