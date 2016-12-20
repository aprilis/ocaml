module MutList =
struct
    type 'a t = Empty | Item of ('a * 'a t) ref

    let create () = Empty
    let remove_next (Item x) =
        let (h, t) = !x in
        let Item y = t in
        let (_, nt) = !y in
        x := (h, nt)
    
    let next (Item x) =
        let (_, t) = !x in t

    let prepend h t = Item (ref (h, t))

end;;

let (++) = MutList.prepend;;
