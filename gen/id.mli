(* Some tools for abstracting names *)
type name = string

type 'a t
val inj : name -> 'a -> 'a t
val prj : 'a t -> 'a
val name : 'a t -> name

type void
val forget : _ t -> void t
