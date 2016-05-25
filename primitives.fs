module Primitives

let random = new System.Random()

/// get sign of an integer
let sign x =
    match x with
    | _ when x < 0 -> -1
    | _ when x > 0 -> 1
    | _ -> 0

/// take first n elements of list
let rec take n l = 
    match n, l with
     | 0, _ -> []
     | _, [] -> []
     | _,  (head :: tail) -> head :: take (n - 1) tail

let listtail l =
    if List.length l > 0 then
        l.Tail
    else
        []    
    
// limit
let doNotExceedMax x m = 
    if x > m then m else x


