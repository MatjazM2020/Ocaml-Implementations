module Graph = struct

  type node = int
  type graph = ( node * node list ) list
  let prazna = ( [] : graph )
  let order (x : graph)  = List.length x

  let degree (x : graph) (y : node) = List.length (snd (List.nth x (y-1)))


  let rec max_degree (x : graph) = match x with 
    | [] -> 0
    | hd::tl when tl = [] -> List.length( snd hd)
    | hd::tl when List.length (snd hd) <= List.length (snd (List.hd (tl))) -> max_degree(List.tl x) 
    | hd::tl -> max_degree (hd::(List.tl(tl)))


  let rec min_degree (x : graph) = match x with 
    | [] -> 0
    | hd::tl when tl = [] -> List.length( snd hd)
    | hd::tl when List.length (snd hd) >= List.length (snd (List.hd (tl))) -> min_degree(List.tl x) 
    | hd::tl -> min_degree (hd::(List.tl(tl)))
                  
                  
  let matrix_of_graph (x : graph) = 
    let matrica  = Array.make_matrix (List.length x) (List.length x) 0  in
    for i = 0 to List.length x-1 do
      for j = 0 to List.length x-1 do 
      
        if i != j then (
          if ((List.filter (fun k -> k = fst(List.nth x i)) (snd(List.nth x j))) 
              = [fst(List.nth x i)]) then matrica.(i).(j) <- 1 
        )
      done;
    done;
    matrica
   
   
  let add_edge (x : graph) ( (a,b) :  node * node) = 
    let newGraph = ref [] in 
        
    for i = 0 to (List.length x)-1 do 
      if i< a-1 then      newGraph := !newGraph @ [List.nth x (i)]
      else if i=a-1 then   newGraph := !newGraph @ [(a, b::snd(List.nth x (a-1)))]
      else if i<b-1 then   newGraph := !newGraph @ [List.nth x (i)]
      else if i=b-1 then   newGraph := !newGraph @ [(b, a::snd(List.nth x (b-1)))]
      else  newGraph := !newGraph @ [List.nth x (i)]
    done;
    ((!newGraph):graph)
        
  let delete_last (x : graph) = 
    let newGraph = ref [] in 
        
    for i = 0 to (List.length x)-1 do 
      if i<(List.length x)-1 then newGraph := !newGraph @ [List.nth x (i)]

    done;
    (!newGraph : graph) 
             
    
    
  let add (x : graph) (y : node list) = 
    let newGraph = ref [] in 
    for i = 0 to (List.length x)-1 do   
      if ((List.filter (fun k -> k = fst(List.nth x i)) (y))) 
         = [fst(List.nth x i)] then 
        newGraph := !newGraph @ [(i+1, List.length x+1::snd(List.nth x i))]
      else 
        newGraph := !newGraph @ [List.nth x i]
    done;
    ( (( List.length x+1, y) :: !newGraph): graph)
end 

