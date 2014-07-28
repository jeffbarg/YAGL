let main () = 
  let cin = 
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin 
  in 
  let lexbuf = Lexing.from_channel cin in  
  Scanner.main_entry lexbuf
(* since there are no printfs in the actions, 
   this won't give anything meaningful right now *)
let _ = Printexc.print main () 
