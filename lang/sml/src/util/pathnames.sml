(* Copyright 1989 by AT&T Bell Laboratories *)

structure Pathnames = struct

  fun findChr (ch :string) ((i,s) :int * string) :int =
    let val len = String.length s
        fun find j =
          if j=len
          then 0
          else if ch = substring(s,j,1)
                 then j+1
                 else find (j+1)
    in if (size ch) = 0 then 0 else find i end;
  
  fun explodePath (path:string) :string list =
    let val slash = findChr "/" (0,path)
        val len = size path
    in
      if slash = 0
        then [path]
        else ((substring (path, 0, slash-1)) ::
              (explodePath (substring (path, slash, len - slash))))
    end;

  fun implodePath (pathlist :string list) :string =
    let fun merge (x,y) = if y = "" then x else (x ^ "/" ^ y) in
      fold merge pathlist ""
    end;

  fun trim (path:string) :string =
    let val parts = explodePath path
        val len = length parts
        val strip' = len - (!System.Print.pathnames) - 1
        val strip = if strip'<=1 then 0 else if strip'>len then len else strip'
        val showParts' = nthtail (parts, strip)
        val showParts = if strip>0 then ("..."::showParts') else showParts'
    in
      implodePath showParts
    end
end
