type 'a t = { mutable prev: 'a t option; mutable next: 'a t option; mutable item: 'a option; _id: int }
let cur_id = ref 0

let equal a b =
    a._id = b._id

let get (Some x) = x

let create () =
    let rec a = { prev = None; next = Some b; item = None; _id = !cur_id }
        and b = { prev = Some a; next = None; item = None; _id = !cur_id + 1 }
    in cur_id := !cur_id + 2; a
let remove x =
    (get x.prev).next <- x.next;
    (get x.next).prev <- x.prev;
    get x.next

let insert_after a l =
    let it = { prev = Some l; next = l.next; item = Some a; _id = !cur_id } in
    cur_id := !cur_id + 1;
    (get l.next).prev <- Some it;
    l.next <- Some it;
    l

let rec of_list l =
    match l with
        [] -> create ()
        | h::t -> insert_after h (of_list t)

let rec _begin l =
    match l.prev with
        None -> l
        | Some p -> _begin p

let rec _end l =
    match l.next with
        None -> l
        | Some n -> _end n

let rec to_list a b =
    if equal a b || a.next = None then []
    else let nx = get a.next in
        if equal nx b then []
        else get nx.item :: to_list nx b