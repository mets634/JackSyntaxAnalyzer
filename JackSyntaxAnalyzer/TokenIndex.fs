module TokenIndex

let mutable i = []

let append = 
    i <- i @ [1]
    i

let currIndex = 
    i.Length