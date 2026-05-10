type 'a t = 
  | Single of 'a
  | Cons of 'a * 'a t

let rec map f = function
  | Single x -> Single (f x)
  | Cons (x, xs) -> Cons (f x, map f xs)

let rec to_list = function
  | Single x -> [x]
  | Cons (x, xs) -> x :: to_list xs

let rec of_list = function
  | [] -> failwith "Empty list can not be converted to nonEmpty."
  | [x] -> Single x
  | x::xs -> Cons (x, of_list xs)

let rec of_list_opt = function
  | [] -> None
  | [x] -> Some (Single x)
  | x::xs -> 
    match of_list_opt xs with
    | None -> Some (Single x)
    | Some list_xs -> Some (Cons (x, list_xs))

let rec length = function
  | Single _ -> 1
  | Cons (_, xs) -> 1 + length xs

let rec fold_left f accu l =
  match l with
  | Single x -> f accu x
  | Cons (x, xs) -> fold_left f (f accu x) xs

let rec fold_right f l accu =
  match l with
  | Single x -> f accu x
  | Cons (x, xs) -> f x (fold_right f xs accu)
