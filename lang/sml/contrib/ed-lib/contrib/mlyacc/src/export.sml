(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

structure ExportParseGen =
   struct
      fun parseGen (name :: file :: nil, _) = ParseGen.parseGen file
        | parseGen (name :: _,_ ) = output(std_out,"Usage: sml-yacc " ^ "f1\n")
        | parseGen _ = output(std_out,"smlyacc: internal error!\n")
   end;
   
exportFn("sml-yacc",ExportParseGen.parseGen);
   
              
               
           
          
