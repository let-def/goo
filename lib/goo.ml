(* Encode goo object hierarchy with polymorphic variants *)
type -'a goo
type goo_object = [`goo_object]

type (+'self, 'a) event = int
let set_event (obj : 'self goo) (event : ('self, 'a) event) (fn : 'self goo -> 'a) =
  Obj.set_field (Obj.repr obj) (event + 2) (Obj.repr fn)
let unset_event (obj : 'self goo) (event : ('self, 'a) event) =
  Obj.set_field (Obj.repr obj) (event + 2) (Obj.repr ())

(* Weak table for referencing OCaml objects from C *)
external set_handle : 'a goo -> int -> unit = "ml_goo_set_handle" [@@noalloc]

let () =
  (* Dereferencing could be done more efficiently without leaving C-side,
     but that's ok for now. *)
  let table = Goo_ref.create ~compact:set_handle 64 in
  let alloc x = Goo_ref.wref table x in
  let deref x = Goo_ref.wderef table x in
  Callback.register "ml_goo_alloc" alloc;
  Callback.register "ml_goo_deref" deref;
  Callback.register "ml_goo_string" ""

(* Primitive comparison, hashing and equality of Goo objects.

   TODO: maybe use object tag with the C-address as unique integer.
   It costs one more word, but it has one less indirection and behaves well
   with non-parametric polymorphic operators. *)
external compare : _ goo -> _ goo -> int = "ml_goo_compare" [@@noalloc]
external hash : _ goo -> int = "ml_goo_hash" [@@noalloc]
let equal a b = compare a b = 0

(* Root hashtables.

   Put object that must be indefinitely retained in the hashtable.
   For instance, windows of a GUI generally manage their lifetime themselves:
   the window should be kept alive as long as it is displayed.

   But being "displayed" is a notion foreign to the GC. When a C objects have
   custom lifetimes, it is useful to make them roots.
   (Otherwise it won't cause memory unsafety, but the window risk disappearing
   when the GC reclaims memory for application need.)
*)
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

(* Type casting. Welcome to OOP world!
   Goo object system can recover some type information at runtime.
   This casting routine won't break safety and runs in O(1). *)
type 'a witness
external cast : _ goo -> 'a witness -> 'a goo option = "ml_goo_cast"
