(* Some tools for making abstract names *)
type name = string

(* Abuse object ids to get well behaved physical identity (comparison, hashing,
   etc). *)
external oo_id : unit -> int = "caml_fresh_oo_id"

type 'a t = { value : 'a; id : int; name : name }

let inj name value =
  let result = { value; id = oo_id (); name } in
  Obj.set_tag (Obj.repr result) Obj.object_tag;
  result

let prj x = x.value

let name x = x.name

type void
let forget : _ t -> void t = Obj.magic
