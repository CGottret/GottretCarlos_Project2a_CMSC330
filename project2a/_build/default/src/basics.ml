(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a,b,c) = tup in (c,b,a)

let is_odd x = 
    if (x mod 2) <> 0 then true
    else false;;

let area_helper x y=
    match x with (a,b) -> match y with (a1, b1) -> (abs(a-a1), abs(b-b1));;

let area x y =
    match (area_helper x y) with (a,b) -> a*b;;

let volume_helper x y =
    match x with (a,b,c) -> match y with (a1,b1,c1) -> (abs(a-a1), abs(b-b1), abs(c-c1));;

let volume x y = 
    match (volume_helper x y) with (a,b,c) -> a*b*c;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
    if n <= 0 then 0
    else if n = 1 then 1
    else (fibonacci(n - 1) +fibonacci(n - 2));;

let rec pow x y =
    if y = 0 then 1
    else x * pow x (y - 1);;

let rec log x y = 
    if x > y then 0
    else (log x (y/x)) + 1

let rec gcf x y = 
    if y = 0 then x
    else gcf y(x mod y);;

let rec is_prime_helper x y =
    y * y > x || (x mod y != 0 && (is_prime_helper x (y + 1)));;

let rec is_prime x = 
    x >= 2 && is_prime_helper x 2;;
(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
	match lst with
	| [] -> failwith "Out of bounds"
	| h::t -> if idx = 0 then h else get (idx - 1) t;;

let rec combine lst1 lst2 = 
    match lst1 with
        |[] -> lst2
        |x::xs -> x::(combine xs lst2);;

let rec reverse lst =
    match lst with
    |[]->[]
    |x::xs -> combine(reverse xs) [x];;

let rec length l = match l with
 | [] -> 0
 | (_::t) -> 1 + (length t);;

 let larger lst1 lst2 =
    if length lst1 == length lst2 then []
    else if length lst1 < length lst2 then lst2
    else lst1;;

let rec sorting_algorithm lst =
    let sorted = match lst with
    | x::x1::xs ->
        if x > x1 then x1::sorting_algorithm (x::xs)
        else x::sorting_algorithm (x1::xs)
    | xs -> xs in
    if lst = sorted then lst
    else sorting_algorithm sorted;;

let rec merge lst1 lst2 = sorting_algorithm (combine lst1 lst2);;

let rotation lst =
  match lst with
  |[]->[] 
  |[x]->[x]
  |x::xs->(combine xs [x]);;

let rec rotate shift lst = 
  if shift == 0 then lst
  else rotate (shift-1) (rotation lst);;

let rec is_palindrome lst = 
    if lst = reverse (lst) then true
    else false;;