(* 2 *)
(* int *)

3
bool

4
'a option option

5
'a ->'b -> 'c

6
('a -> 'b) -> ('b -> 'c) -> 'd

10
if (if true then false else true) then false else true ;;

(* 2 *)
let rec copies (times: int) (str: string) : string = 
    if times < 0 then "" else
        match times with
        | 0 -> ""
        | _ -> str ^ copies (times - 1) str ;;


(* 3 *)

module type COMPARABLE =
    sig
        type t
        type order = Less | Equal | Greater
        val compare : t -> t -> order
    end ;;

module IntComparable : COMPARABLE with type t = int  =
    struct
        type t = int
        type order = Less | Equal | Greater
        let compare (t1: t) (t2: t) =
            if t1 < t2 then Less else if t1 = t2 then Equal else Greater
    end ;;

(* Multisets *)
(* 4 *)
module type MULTISET =
    sig
        type element (* the type of elements of the multiset *)
        type t (* the type of the multiset itself *)
        (* an empty multiset *)
        val empty_set : t
        (* empty_p mset -- Returns `true` if and only if `mset`
        is empty *)
        val empty_p : t -> bool
        (* add elt mset -- Returns a multiset like `mset` with
        one more `elt` *)
        val add : element -> t -> t
        (* drop elt mset -- Returns a multiset with one `elt`
        removed from `mset` *)
        val drop : element -> t -> t
        (* count elt mset -- Returns the number of `elt`s in
        `mset` *)
    end ;;

module MakeMultiset (Element : COMPARABLE) : (MULTISET with type element = Element.t) =
    struct
        type element = Element.t
        type t = (element * int) list
        let empty_set = []
        let empty_p mset = mset = empty_set
        let rec adjust fn elt mset =
            match mset with
                | [] -> let newcount = fn 0 in
                    if newcount = 0 then mset
                    else (elt, newcount) :: mset
                | (current, curcount) :: rest ->
            match Element.compare elt current with
                | Less -> let newcount = fn 0 in
                    if newcount = 0 then mset
                    else (elt, newcount) :: mset
                | Equal -> let newcount = fn curcount in
                    if newcount = 0 then rest
                    else (elt, newcount) :: rest
                | Greater -> (current, curcount) :: adjust fn elt rest
        let rec add elt mset = adjust succ elt mset
        let rec drop elt mset =
        adjust (fun count -> if count = 0 then 0 else pred count) elt mset
        (* ...the rest of the implementation would go here... *)
    end ;;

(* 5 *)
module IntMultiset = MakeMultiset (
    struct
        type t = int
        type order = Less | Equal | Greater
        let compare (t1: t) (t2: t) =
            if t1 < t2 then Less else if t1 = t2 then Equal else Greater
    end ) ;;

(* 7 *)
let m_set = add 1 (add 5 (add 5 empty_set)) ;;

(* 8 *)
(count 5 mset) > (count 1 mset)

(* Royals *)
type royal = {name : string; age : int; children : royal list} ;;

let compare_age (r1: royal) (r2: royal) =
    if r1.age < r2.age then -1 else if r1.age = r2.age then 0 else 1 ;;

let rec count_royals (lst: royal list) : int =
    match lst.children with
    | [] -> 1
    | (hd :: tl) -> List.fold_left (+) 0 (List.map count_royals hd.children) ;;