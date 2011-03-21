type rgb = (float * float * float)

val rainbow_array: rgb array
val rainbow_color: int -> rgb

(* use emacs color names *)
val rgb_of_string: string -> rgb
