type +'a node =
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let hd stm =
  match stm () with
  | Cons (a, _) -> a

let tl stm =
  match stm () with
  | Cons (_, stm) -> stm

let rec repeat x () =
  Cons (x, repeat x)

let rec forever f () =
  Cons (f (), forever f)

let rec append l stm () =
  match l with
  | [] ->
    stm ()
  | x::xs ->
    Cons (x, append xs stm)

let rec cycle l () =
  append l (cycle l) ()

let follow x y () =
  match x () with
  | Cons (x, _) ->
    Cons (x, y)

let rec map f stm () =
  match stm () with
  | Cons (x, next) ->
    Cons (f x, map f next)

let rec filter f stm () =
  match stm () with
  | Cons (x, next) ->
    if f x
    then Cons (x, filter f next)
    else filter f next ()

let rec filter_opt f stm () =
  match stm () with
  | Cons (x, next) ->
    if f x
    then Cons (Some x, filter_opt f next)
    else Cons (None, filter_opt f next)

let rec iter f stm =
  match stm () with
  | Cons (x, next) ->
    f x;
    iter f next

let rec peek stm n =
  if n < 0
  then invalid_arg "Stream.peek"
  else if n = 0
  then []
  else match stm () with
    | Cons (x, next) ->
      x :: peek next (n - 1)

let rec take stm n ~def () =
  if n < 0
  then invalid_arg "Stream.take"
  else if n = 0
  then repeat def ()
  else match stm () with
    | Cons (x, next) ->
      Cons (x, take next (n - 1) ~def:def)

let rec take_opt stm n () =
  if n < 0
  then invalid_arg "Stream.take_opt"
  else if n = 0
  then repeat None ()
  else match stm () with
    | Cons (x, next) ->
      Cons (Some x, take_opt next (n - 1))

let rec drop stm n () =
  if n < 0
  then invalid_arg "Stream.drop"
  else if n = 0
  then stm ()
  else match stm () with
    | Cons (x, next) ->
      Cons (x, drop next (n - 1))

let rec merge cs xs ys () =
  match (cs (), xs (), ys ()) with
  | Cons (c, cs),
    Cons (x, xs),
    Cons (y, ys) ->
    if c
    then Cons (y, merge cs xs ys)
    else Cons (x, merge cs xs ys)

