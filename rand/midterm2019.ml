(* Problem 1 *)
let f a = Some (a + a) ;;
let f (a, b) = if b then a else a ;;
let f a = Some [] ;;
UNSOLVED

(* Problem 2 *)
'a
int * int * int
float list
'a list -> 'a list

(* Problem 3 *)
bool -> string

(* Problem 4 *)
let string_of_bool (input : bool) : string =
  if input then "true" else "false" ;;

(* Problem 5 *)
List.length is an accumulator function and so it iterates through the list of size n and
returns information after n time. Even though there are two lists that are iterated through,
we get that the runtime is O(n) for each list and so O(n1) + O(n2). Since they are of the
same magnitude, we get O(n) where n is the larger list and then + 1 for the compare
evaluation. Thus O(n+1) = O(n). 

(* Problem 6 *)
let rec compare_lengths xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | hd1 :: tl1, hd2 :: tl2 -> compare_lengths tl1 tl2 ;;

(* Problem 7 *)
NO SUBSTITUION

(* Problem 8 *)
type 'a bintree =
| Empty
| Node of 'a bintree * 'a * 'a bintree ;;

let insert (item : 'a) (tree : bintree) : bintree =
  match bintree with
  | Empty -> Node (Empty, item, Empty) 
  | Node (left, old, right) ->
  if old < item then
  Node (insert item left, right)
  else
  Node (left, insert item right) ;;

1. Should be match tree with not match bintree with
2. Need rec keyword
3. We need to compare the value from the left and right subtrees with the item.
A way to do that is to change old < item to 
(match left with | (_,b,_) -> b) < item.
4. If the old one is less, then we should insert to the right. Also, Node is
type 'a bintree * 'a * 'a bintree and so instead of  Node (insert item left, right)
we should write Node (left, old, insert item right)
5. The else statement should be similarly adjusted. Now we can see that if the 
old one is less than the item, then we go to the right. Otherwise, we go to the left
if it would otherwise be greater or equal.
6. bintree signature should be 'a bintree
9. tree matching is non-exhaustive for partially empty nodes since the logic below 
assumes that the branches have items.

Fixed
let rec insert (item : 'a) (tree : 'a bintree) : 'a bintree =
  match tree with
  | Empty -> Node (Empty, item, Empty)
  | Node (Empty, old, right) ->
  if (match right with | Node (_, b, _) -> b) > item then 
  Node (Empty, old ,insert item right) else Node (Node (Empty, item, Empty), old, right)
  | Node (left, old, Empty) -> 
  if (match left with | Node (_, b, _) -> b) < item then 
  Node (insert item left, old , Empty) else Node (left, old , Node (Empty, item, Empty))
  | Node (left, old, right) ->
  if (match left with | Node (_, b, _) -> b) < item then
  Node (left, old ,insert item right) else
  Node (insert item left, old, right) ;;

(* Problem 9 *)
type direction = Left | Right ;;

let rec gorn (tofind : 'a)(tree : 'a bintree) : direction list =
  match tree with
    | Empty -> raise (Failure "gorn: item not found")
    | Node (Empty, cur, rem) ->
    if cur = tofind then [] else Right :: gorn tofind rem
    | Node (rem, cur, Empty) ->
    if cur = tofind then [] else Left :: gorn tofind rem
    | Node (left, cur, right) ->
    if cur = tofind then [] else if
    (match left with | Node (_, b, _) -> b) >= tofind then Left :: gorn tofind left else
    Right :: gorn tofind right ;; 

let find (tofind : 'a)(tree : 'a bintree) : bool =
  try List.length (gorn tofind tree) >= 0 with
  | Failure "gorn: item not found" -> false ;;