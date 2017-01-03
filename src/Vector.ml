type 'a t = { mutable arr: 'a array; mutable len: int; default: 'a }

let create default = { arr=Array.make 1 default; len=0; default=default }

let reserve vec n =
    let new_arr = Array.make n vec.default in
    Array.blit vec.arr 0 new_arr 0 vec.len;
    vec.arr <- new_arr

let length vec = vec.len

let append vec a =
    if vec.len = Array.length vec.arr then reserve vec (vec.len * 2);
    Array.set vec.arr vec.len a;
    vec.len <- vec.len + 1

let pop vec =
    vec.len <- vec.len - 1;
    Array.set vec.arr vec.len vec.default

let get vec n = 
    if n >= vec.len 
    then raise (Invalid_argument "index out of bounds")
    else Array.get vec.arr n
let set vec n a =
    if n >= vec.len
    then raise (Invalid_argument "index out of bounds")
    else Array.set vec.arr n a