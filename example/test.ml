(* Defining the factorial function *)
let factorial n = 
  let rec f n a =
    if n < 2 then a else f (n-1) (n*a)
  in f n 1

(* Defining the sum function *)
let sum a b = a + b

(* Registering the callback for external use *)
let _ = Callback.register "factorial" factorial
let _ = Callback.register "sum" sum

(* Defining an algebraic datatype representing a tree *)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let count =
  let count = ref (-1) in
  fun () -> incr count; !count
let _ = Callback.register "count" count

(* Left rotation of the tree *)
let rotate_left t = match t with
  | Node (a, Node (b, c)) -> Node (Node (a, b), c)

(* Right rotation of the tree *)
let rotate_right t = match t with
  | Node (Node (a, b), c) -> Node (a, Node (b, c))

(* Registering the callbacks *)
let _ = Callback.register "rotate_left" rotate_left
let _ = Callback.register "rotate_right" rotate_right
