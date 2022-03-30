module MakeStack (Arg: sig type t end) : (STACK with type elt = Arg.t) =
    struct
        type elt = Arg.t
        type stack = elt list
        exception Empty of string
        let empty () = []
        let push (s,x) = x::s
        let pop s = match s with
        | x :: t -> (x, t)
        | [] -> raise (Empty "empty")
        let isEmpty = fun x -> x = []
    end ;;

module type STACK =
    sig
        type elt
        type stack
        exception Empty of string
        val empty : unit -> stack
        val push : stack * elt -> stack
        val pop : stack -> elt * stack
        val isEmpty : stack -> bool
    end ;;

(* Define a curried function named power that returns its first
argument (an integer) raised to the power of its second argument
(also an integer, which you can assume is nonnegative), returning
an integer result. *)

let power x y = int_of_float ((float_of_int x) ** (float_of_int y)) ;;
let power (x, y) = int_of_float ((float_of_int x) ** (float_of_int y)) ;;

(* The tower function takes a list of integers
[a1; a2; a3; ...; an] and returns their nested exponentiation 

# tower [2; 2] ;;
- : int = 4
# tower [2; 3] ;;
- : int = 8
# tower [2; 3; 2] ;;
- : int = 512

*)

let tower (lst: int list) : int = 
    let lst = List.rev lst in 
    List.fold_left (fun x  -> power (x)) 1 lst ;;

let find f lst =
  let all = List.filter f lst in
  match all with
  | [] -> raise Not_found
  | h :: tl -> h ;;


(* Problem 6 a *)
type connect = | Series | Parallel ;;

type circuit = 
    | Single of float
    | Pair of circuit * circuit * connect ;;

(* Problem 6 b *)
let circ_a = Single 3.0 ;;

(* Problem 6 c *)
let circ_c = Pair (Single 2.0, Single 4.0, Parallel) ;;

(* Problem 6 d *)
let circ_d = Pair (Single 1.0, Pair (Pair (Single 2.0, Single 4.0, Parallel), Single 4.0, Parallel), Series) ;;

(* problem 6 e *)
let rec resistance (c: circuit) : float = 
    match c with
    | Single r -> r
    | Pair (c1, c2, Series) -> resistance c1 +. resistance c2
    | Pair (c1, c2, Parallel) -> 1.0 /. ((1.0 /. resistance c1) (1.0 /. resistance c2)) ;;

(* Problem 7 a *)
(* SEQUENCE with type t = Int *)

(* 7b *)
(* int list *)

(* 7c *)
(* sequence_from 0 length *)

(* 7d *)
(* SEQUENCE with type t = Float *)

(* 7e *)
(* float list *)

(* 7f *)
(* (from /. 2.0) *)

(* 7g *)
(* sequence_from 1.0 length *)

(* 7 next *)
module type ELEMENT =
    sig 
        type element
        val init: element 
        val next: element -> element
    end ;;

module type SEQUENCE =
    sig
        type t
        val sequence : int -> t list
    end ;;

module Sequence (Element: ELEMENT) : (SEQUENCE with type t = Element.element) =
    struct
        type t = Element.element
        let rec sequence_from
    end ;;

module IntSequence = Sequence 
    ( 
        struct 
            type element  = int
            val init: 0
            val next: (fun x -> x + 1)
        end
    ) ;;

(* Problem 8 *)