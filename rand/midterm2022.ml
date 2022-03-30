(* Provide a succinct definition of a function named reciprocal that returns the 
reciprocal of its argument of type float. That is, reciprocal 2.0 should return 
0.5 and reciprocal 4.0 should return 0.25. Your implementation may do whatever 
you want for an input of 0. *)

let reciprocal (f: float) : float = 
    if f = 1.0 then 1.0 else 1.0 /. f ;;

(* Consider a definition of a person type as follows.

type person = {first_name: string; last_name: string}

Define an uncurried function named full_name that accepts a string * string 
input representing a person’s first and last name. The function should 
return a person with that name. *)

let full_name (first, last : string * string) : person = 
    {first_name = first; last_name = last} ;;

(* Define a function named swap_arguments that accepts as input f, a curried 
function that takes two arguments. swap_arguments should return a function that 
behaves identically to f but swaps the order in which it takes its two arguments.

For example, (swap_arguments (/)) 2 8 should return 4, and (swap_arguments (<)) 2 8 should return false. *)

let swap_arguments f x y = f y x ;;


let setup = (0, (fun x -> 42)) in
let a, b = setup in
b a ;;

let setup = _______________ in
let x = try 
          let _ = setup 0 
          in 0
        with _ -> 21 in
x * (setup 1) ;;

let setup = ([], [21; 0]) in
match setup with
| [], [] -> 1
| [], hd :: _tl -> hd * 2
| hd :: _tl, [] -> hd * 3
| _ -> 4 ;;

let setup = (fun x -> int_of_float ((float_of_int x) *. 8.4)) in
1 + setup 2 + setup 3 ;;

let setup = (fun x -> (fun x -> (fun x -> 42))) in
let x = setup 1 in
let y = x 2 in
y 3 ;;

y = (fun x -> x * 3)

(* An acrostic poem is a poem where taking the nth letter of each line 
(typically the first) produces a word. Define a function acrostic that,
 when given a list of strings and an integer i, returns the string that 
 results from concatenating the character at index i of each string.

For example:

# acrostic [ "Always"; "Be"; "Closing" ] 0 ;;
- : string = "ABC"
# acrostic [ "Accumulating"; "answers"; "of5ends"; "no1" ] 2 ;;
- : string = "cs51"

Implement acrostic using map/fold/filter as specified above.
 You may assume the existence of a 
 function get : string -> int -> string such that get s i returns a string c
 ontaining only the character at index i of string s 
 (and raises an exception if s does not have at least i + 1 characters).

Your implementation may do whatever you want if a line is not long enough.
Recall that the binary operator ^ concatenates two strings. *)

let acrostic (lst : string list) i : string = 
    if length lst < i then raise (Invalid_argument "Line too short")
    else fold_left (^) "" (map ((swap_arguments get) i) lst) ;;

type clade =
    | Species of string
    | Branch of clade * clade 
;;

let exa = Branch (Branch ((Species "A"), (Species "B")), (Species "B")) ;;

let rec count_species (c : clade) : int = 
    match c with
    | Species s -> 1
    | Branch (x, y) -> count_species x + count_species y ;;

let rec clade_contains_species (s : string) (c : clade) : bool = 
    match c with
    | Species s' -> s = s'
    | Branch (x, y) -> clade_contains_species s x || clade_contains_species s y ;;

let rec clade_fold (combine : 'a -> 'a -> 'a) (init : string -> 'a) (clade : clade) : 'a =
    match clade with 
    | Species s -> init s
    | Branch (c1, c2) -> combine (clade_fold combine init c1) (clade_fold combine init c2) 
;;

let count_species_new (c : clade) : int = 
    clade_fold (+) (fun s -> 1) c ;;

(* Define a function clade_contains_species_new : string -> clade -> bool that accepts
 the name of a species and a clade, returning true if the clade contains the species and false otherwise. *)

let clade_contains_species (s : string) (c : clade) : bool = 
    clade_fold (||) (fun x -> x = s) c ;;


(* In the following signature, METRIC_TYPE, the function metric computes the distance between elements (or "points") of the abstract type t.

module type METRIC_TYPE = 
sig
  type t
  val metric : t -> t -> float
end *)

(* Define a module called FloatMetric where the type t is equal to float and the 
metric is the absolute difference between two floats. You may use the function
 Float.abs to compute the absolute value of a float. *)

 module FloatMetric : METRIC_TYPE with type t = float =
    struct 
        type t = float
        let metric (x: float) (y: float) = Float.abs (x -. y)
    end ;;

module RoutePlanner (MT : METRIC TYPE) =
  struct
    type t = MT.t
    let rec remove (ts : t list) (a : t) : t list =
      match ts with 
       | [] -> []
       | hd :: tl -> if a = hd then tl else hd :: (remove tl a)
    let rec min_distance (a : t) (ts : t list) : (t * float) option =
        match ts with 
         | [] -> None
         | hd :: tl -> 
             match min_distance a tl with 
            | None -> Some (hd, Float.max_float)
            | Some (b, d) -> if (MT.metric hd a) < d then Some (hd, MT.metric hd a) else Some (b, d)
    let rec route (start : t) (waypoints : t list) : t list =
        let (last, d) = min_distance start waypoints in
        if d = Float.max_float then [] else last :: (route last (remove waypoints end))
  end ;;
                                                  

(* Define a function find_divergence_point : clade -> string -> string -> clade option. The function accepts a clade and two species names, returning the smallest clade that contains both species (or None if no such clade exists).

For example, if we were to represent the example cladogram at the top of this problem as a value c:

# find_divergence_point c "Cow" "Giraffe" ;;
- : clade option = Some (Branch (Species "Giraffe", Species "Cow"))
# find_divergence_point c "Cow" "Llama" ;;
- : clade option = None
You may make use of functions defined previously in this question. You may assume that the two species names will be distinct, and you may assume that any species is included at most once in any clade. Your solution need not be the most efficient possible: any correct implementation will receive full credit. *)

let find_divergence_point (c : clade) (s1 : string) (s2 : string) : clade option = 
    match c with
    | Species s -> None
    | Branch (x, y) -> 
        let x_opt = find_divergence_point x s1 s2 in
        let y_opt = find_divergence_point y s1 s2 in
        match x_opt with
        | None -> match y_opt with
            | None -> None
            | Some y' -> Some y'
        | Some x' -> match y_opt with
            | None -> Some x'
            | Some y' -> if (count_species_new x') < (count_species_new y') then Some x' else Some y'
    ;;

(* Define a function find_divergence_point_new : clade -> string -> string -> clade option. The function accepts a clade and two species names, returning the smallest clade that contains both species (or None if no such clade exists).

type 'a merge =
     | First of ('a -> 'a)
    | Second of ('a * 'a)
; ;
let rec f wx у z
           “ with match |w
      | [] -> y
     | First a | b -> :: b
                 ах in
               c:: y) z
        let c
        f b c
     | Second a
        let c = f b x°in
           d
                 a
               :: b <>
        c (Z'a :: y° ) z le C
; ;
                    |