type -'a goo
type goo_object = [`goo_object]
type goo_object_events = [`Null]

external set_handle : 'a goo -> int -> unit = "ml_goo_set_handle" [@@noalloc]
external get_handle : [>goo_object] goo -> int = "ml_goo_get_handle" [@@noalloc]

type -'a events
type 'a handler = [`Handler of 'a events]

type ('self,'events) delegate = 'self goo -> 'events -> bool
  constraint 'self = [> 'events handler]

let set_handler (goo : 'self goo) (handler : ('self, _) delegate) =
  Obj.set_field (Obj.repr goo) 1 (Obj.repr handler)

let table = Goo_ref.create ~compact:set_handle 64
let alloc x = Goo_ref.wref table x
let deref x = Goo_ref.wderef table x

let () =
  Callback.register "ml_goo_alloc" alloc;
  Callback.register "ml_goo_deref" deref;
  Callback.register "ml_goo_string" "";

external compare : _ goo -> _ goo -> int = "ml_goo_compare" [@@noalloc]
external hash : _ goo -> int = "ml_goo_hash" [@@noalloc]

let equal a b = compare a b = 0

module Tbl = Hashtbl.Make(struct
    type t = goo_object goo
    let equal = equal
    let hash = hash
  end)

let roots = Tbl.create 7

let retain (goo : _ goo) =
  let goo = (Obj.magic goo : goo_object goo) in
  Tbl.add roots goo ()

let release (goo : _ goo) =
  let goo = (Obj.magic goo : goo_object goo) in
  Tbl.remove roots goo

type 'a witness

external cast : _ goo -> 'a witness -> 'a goo option = "ml_goo_cast"
