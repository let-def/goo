type -'a goo
type goo_object = [`goo_object]

type -'a events
type 'a handler = [`Handler of 'a events]

type ('self,'events) delegate = 'self goo -> 'events -> bool
  constraint 'self = [> 'events handler]

val set_handler : 'self goo -> ('self, 'events) delegate -> unit

val retain : _ goo -> unit
val release : _ goo -> unit

val compare : _ goo -> _ goo -> int
val equal : _ goo -> _ goo -> bool
val hash : _ goo -> int

type 'a witness

val cast : _ goo -> 'a witness -> 'a goo option

type -'a return
val give : 'a return -> 'a -> unit
