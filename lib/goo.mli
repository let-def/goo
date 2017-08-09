type -'a goo
type goo_object = [`goo_object]

type (+'self, 'a) event
val set_event : 'self goo -> ('self, 'a) event -> ('self goo -> 'a) -> unit
val unset_event : 'self goo -> ('self, 'a) event -> unit

type 'a witness
val cast : _ goo -> 'a witness -> 'a goo option

val retain : _ goo -> unit
val release : _ goo -> unit

val compare : _ goo -> _ goo -> int
val equal : _ goo -> _ goo -> bool
val hash : _ goo -> int
