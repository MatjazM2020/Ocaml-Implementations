let selectionSort lst =

  let rec findMin lst =  match lst with 
    | [x] -> x
    | a::b::c when a > b ->  findMin (b::c)
    | a::b::c -> findMin (a::c)                          
  in

  let rec delete lst elt newLst = match lst with
    | [] -> []
    | a::b when a = elt -> newLst @ b
    | a::b -> delete b elt (newLst@[a])
  in

  let rec sort  lst = match lst with
    | [] -> []
    | [x] -> [x]
    |  a::b  ->  [findMin(a::b)] @ sort (delete (a::b) (findMin(a::b)) []) 
  in sort lst
;;