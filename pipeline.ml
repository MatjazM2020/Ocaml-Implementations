(* .a__________________________.a_____________________________.a*)
let print_pipeline pipeArray boolArray = 
  for i = 0 to Array.length pipeArray-1 do 
    for j = 0 to Array.length pipeArray.(0)-1 do 
      if boolArray.(i).(j) = false then (print_string " ")
      else (
        if pipeArray.(i).(j) = Plus then (print_string "+")
        else if pipeArray.(i).(j) = Minus then (print_string "-")
        else if pipeArray.(i).(j) =Block then (print_string "x")
        else if pipeArray.(i).(j) = Midbar then (print_string "|"))
    done;
    print_string "\n"
  done;;

(*.b___________________________.b_____________________________.b*)
let rotate_pipeline pipeArray = 
  let rotatedArray =  Array.make_matrix (Array.length pipeArray) (Array.length pipeArray.(0)) pipeArray.(0).(0) in 
  for i = 0 to Array.length pipeArray-1 do 
    for j = 0 to Array.length pipeArray.(0)-1 do 
      if pipeArray.(i).(j) = Minus then (rotatedArray.(i).(j) <- Midbar)
      else if pipeArray.(i).(j) = Midbar then (rotatedArray.(i).(j) <- Minus)
      else if pipeArray.(i).(j) = Plus then (rotatedArray.(i).(j) <- Plus)
      else if pipeArray.(i).(j) = Block then (rotatedArray.(i).(j) <- Block)
    done
  done;
  rotatedArray
;;
(*.c___________________________.c______________________________.c*)

let explore_pipeline pipeArray = 
  let boolArray = Array.make_matrix (Array.length pipeArray) (Array.length pipeArray.(0)) false in 
  boolArray.(0).(0) <- true; 

  for i = 0 to Array.length pipeArray-1 do
    for j = 0 to Array.length pipeArray.(0)-1 do 
       
      if boolArray.(i).(j) = true then (
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then ( 
          if i>0 then (
            if pipeArray.(i-1).(j) = Midbar || pipeArray.(i-1).(j) = Plus
            then (boolArray.(i-1).(j) <- true)));
            
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then(
          if i<Array.length pipeArray-1 then(
            if pipeArray.(i+1).(j) = Midbar || pipeArray.(i+1).(j) = Plus
            then (boolArray.(i+1).(j) <- true)));
        
        if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
          if j>0 then (
            if pipeArray.(i).(j-1) = Minus || pipeArray.(i).(j-1) = Plus 
            then (boolArray.(i).(j-1) <- true)));
             
        if j<Array.length pipeArray.(0)-1 then (
          if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
            if  pipeArray.(i).(j+1) = Minus || pipeArray.(i).(j+1) = Plus
            then (boolArray.(i).(j+1) <- true))); )
    done 
  done;
   
         
  for i = 0 to Array.length pipeArray-1 do
    for j = Array.length pipeArray.(0)-1 downto 0 do 
       
      if boolArray.(i).(j) = true then (
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then (
          if i>0 then (
            if pipeArray.(i-1).(j) = Midbar || pipeArray.(i-1).(j) = Plus
            then (boolArray.(i-1).(j) <- true)));
           
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then(
          if i<Array.length pipeArray-1 then (
            if pipeArray.(i+1).(j) = Midbar || pipeArray.(i+1).(j) = Plus
            then( boolArray.(i+1).(j) <- true)));
         
        if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
          if j>0 then (
            if pipeArray.(i).(j-1) = Minus || pipeArray.(i).(j-1) = Plus 
            then (boolArray.(i).(j-1) <- true)));
              
        if j<Array.length pipeArray.(0)-1 then (
          if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
            if  pipeArray.(i).(j+1) = Minus || pipeArray.(i).(j+1) = Plus
            then (boolArray.(i).(j+1) <- true)));)
    done 
  done;
                    
  for i = Array.length pipeArray-1 downto 0 do 
    for j = Array.length pipeArray.(0)-1 downto 0 do 
       
      if boolArray.(i).(j) = true then (
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then (
          if i>0 then (
            if pipeArray.(i-1).(j) = Midbar || pipeArray.(i-1).(j) = Plus
            then ( boolArray.(i-1).(j) <- true)));
      
            
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then (
          if i<Array.length pipeArray-1 then (
            if pipeArray.(i+1).(j) = Midbar || pipeArray.(i+1).(j) = Plus
            then ( boolArray.(i+1).(j) <- true)));
        
               
        if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
          if j>0 then (
            if pipeArray.(i).(j-1) = Minus || pipeArray.(i).(j-1) = Plus 
            then (boolArray.(i).(j-1) <- true)));
              
                 
        if j<Array.length pipeArray.(0)-1 then (
          if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
            if  pipeArray.(i).(j+1) = Minus || pipeArray.(i).(j+1) = Plus
            then (boolArray.(i).(j+1) <- true))); )
                     
    done
  done;
                
  for i = Array.length pipeArray-1 downto 0 do 
    for j = 0 to Array.length pipeArray.(0)-1 do 
       
      if boolArray.(i).(j) = true then (
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then (
          if i>0 then (
            if pipeArray.(i-1).(j) = Midbar || pipeArray.(i-1).(j) = Plus
            then (boolArray.(i-1).(j) <- true)));
      
        if pipeArray.(i).(j) = Midbar || pipeArray.(i).(j) = Plus then(
          if i<Array.length pipeArray-1 then(
            if pipeArray.(i+1).(j) = Midbar || pipeArray.(i+1).(j) = Plus
            then (boolArray.(i+1).(j) <- true)));
        
               
        if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
          if j>0 then (
            if pipeArray.(i).(j-1) = Minus || pipeArray.(i).(j-1) = Plus 
            then (boolArray.(i).(j-1) <- true)));
              
        if j<Array.length pipeArray.(0)-1 then (
          if pipeArray.(i).(j) = Minus || pipeArray.(i).(j) = Plus then (
            if  pipeArray.(i).(j+1) = Minus || pipeArray.(i).(j+1) = Plus
            then (boolArray.(i).(j+1) <- true)));)
                     
    done
  done;

  boolArray
;;