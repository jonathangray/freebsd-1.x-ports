(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Pathname :PATHNAME = struct

structure SysIO = System.Unsafe.SysIO

val maxsymlinks = 32   (* from /usr/include/sys/param.h *)

fun getwd () = Execute.firstLine ("/bin/pwd",[])

fun findChr (ch :string) ((i,s) :int * string) :int =
  let val len = String.length s
      fun find j =
        if j=len
	  then 0
	  else if ch = chr(ordof(s,j))
	         then j+1
		 else find (j+1)
  in if (size ch) = 0 then 0 else find i end

fun findChrFromRight (ch :string) ((i,s) :int * string) :int =
  let val len = String.length s
      fun find j =
        if j = ~1
	  then len
	  else if ch = chr(ordof(s,j))
	         then j
		 else find (j-1)
  in if (size ch) = 0 then len else find (i-1) end

fun extension (name :string) =
  let val len = size name
      val dot = findChrFromRight "." (len, name)
  in if dot = len then ""
       else (substring (name, dot+1, len-dot-1))
  end

fun stripExtension (name :string) =
  let val len = size name
      val dot = findChrFromRight "." (len, name)
  in
    substring (name, 0, dot)
  end

fun splitFileExtension (name :string) =
let val len = size name
      val dot = findChrFromRight "." (len, name)
  in if dot = len then (name, "")
       else (substring (name,0, dot), substring (name, dot+1, len-dot-1))
  end

val slash = ord "/"

fun mergeDirFile dirname filename =
  if dirname = "" then filename
    else if filename = "" then dirname
    else if (ord filename = slash) orelse
            (ordof (dirname,(size dirname)-1) = slash)
           then (dirname ^ filename)
           else (dirname ^ "/" ^ filename)

fun mergePathnames arg =
  case arg of
    [] => ""
  | (head::tail) =>
      let val rest = mergePathnames tail in
        if head = "" then rest else if rest = "" then head else
          let val size'head = size head
              val head' = if (ordof (head,size'head-1) = slash)
                            then substring (head, 0, size'head-1)
                            else head
              val rest' = if (ord rest = slash)
                            then substring (rest, 1, (size rest)-1)
                            else rest
          in (head' ^ "/" ^ rest') end
      end

fun directoryPart name =
  let val name'len = size name
      val pos = findChrFromRight "/" (name'len, name)
  in
    if pos = name'len then "" 
      else if pos = 0 then "/"
        else substring (name, 0, pos)
  end

fun stripDirectory name =
  let val name'len = size name
      val pos = findChrFromRight "/" (name'len, name)
      val dirname = substring (name, 0, pos)
  in
    if pos = name'len
      then dirname
      else substring (name, pos+1, name'len-(pos+1))
  end

fun splitDirFile name =
  let val name'len = size name
      val pos = findChrFromRight "/" (name'len, name)
      val dirname = substring (name, 0, pos)
  in
    if pos = name'len then ("", dirname)
      else if pos = 0 then ("/", substring (name, 1, name'len-1))
        else (dirname, substring (name, pos+1, name'len-(pos+1)))
  end

fun splitDirFile' name =
  let val (first, second) = splitDirFile name in
    if first = "" then (second, first) else (first, second) end

fun explodePath (path:string) :string list =
  let val slash = findChr "/" (0,path)
      val len = size path
  in
    if slash = 0 then [path] else 
      let val head = if slash = 1 then "/" else substring (path, 0, slash-1) in
        head::(explodePath (substring (path, slash, len - slash))) end
  end;

fun implodePath (pathlist :string list) :string =
  let fun merge (x,y) =
        if y = "" then x 
	  else if x = "/" then ("/" ^ y)
	         else (x ^ "/" ^ y)
  in
    fold merge pathlist ""
  end

fun clearPath' (path'list :string list) :string list =
  let fun processDots (prefix:string list) (suffix:string list) =
        case suffix of
           [] => rev prefix
         | (s0::s'tail) =>
             case prefix of
                [] => processDots [s0] s'tail
              | (p0::p'tail) =>
                  if s0 = ".."
                    then if p0 = "" (* rmDot guarantees p'tail=[] *)
                           then p0::s0::(processDots [] s'tail)
                           else processDots [] ((rev p'tail) @ s'tail)
                    else processDots (s0::prefix) s'tail
      fun rmDot (pathlist:string list) =
        let fun rm'dot ([]:string list) = []
              | rm'dot (name::tail) =
                  if (name = ".") orelse (name = " ")
                    then rm'dot tail else name::(rm'dot tail)
        in
          case pathlist of
             [] => []
           | (first::rest) =>
               if first = " " then first::(rm'dot rest)
                 else if (first = ".") andalso (rest = []) then pathlist
                   else rm'dot pathlist
        end
  in
    processDots [] (rmDot path'list)
  end;

fun clearPath (path :string) :string =
  implodePath (clearPath' (explodePath path))

fun relativeName (directory:string) (filename:string) =
  let val dir = explodePath directory
      val file = explodePath filename
      fun rm'common'prefix (args :string list * string list) =
        case args of
           (head1::tail1,head2::tail2) =>
             if (head1=head2)
               then rm'common'prefix (tail1,tail2)
               else args
         | (_,_) => args
     val (dir2,file2) = rm'common'prefix (dir, file)
  in
    if (dir2 = []) orelse (dir2 = [""])
      then implodePath file2
      else filename
  end

fun absoluteName (directory:string) (filename:string) = 
  if substring(filename,0,1) = "/"
    then clearPath filename
    else clearPath (mergeDirFile directory filename)

fun absoluteName' (directory:string list) (filename:string list) =
  case filename of
     [] => directory
   | [""] => directory
   | (head::tail) =>
       if substring(head,0,1) = "/" then clearPath' filename
         else clearPath' (directory @ filename)

val longSymLinkChain = (System.Unsafe.CInterface.SystemCall
      ("Chain of symbolic links longer than "^(makestring maxsymlinks)))
val emptySymLink = (System.Unsafe.CInterface.SystemCall
                    "Symbolic link is empty")

fun resolve'symlinks count (dirname, fname) :string =
  let val pathname = mergePathnames [dirname, fname] in
    if count < maxsymlinks then () else raise longSymLinkChain;
    case SysIO.ftype (SysIO.PATH pathname) of
       SysIO.F_SYMLINK =>
          let val targetname = SysIO.readlink pathname in
            if ((ord targetname) = (ord "/")) handle Ord => raise emptySymLink
              then resolve'symlinks (count+1)(splitDirFile targetname)
              else resolve'symlinks (count+1)(dirname,targetname)
          end
     | _ => pathname
  end

val resolveSymLinks' = resolve'symlinks 0

fun resolveSymLinks (pathname:string) :string =
  resolveSymLinks' (splitDirFile pathname)

fun resolveAllSymLinks' (pathnameComponents:string list) :string =
  let fun res (x,y) = resolveSymLinks' (y,x) in
    revfold res pathnameComponents ""
  end

fun resolveAllSymLinks pathname :string =
  resolveAllSymLinks' (explodePath pathname)

fun absoluteRealPathname' (name :string list) =
  let val absolute'name =
        case name of
           [] => explodePath (getwd())
         | [""] => explodePath (getwd())
         | (head::tail) =>
             if substring (head,0,1) = "/" then name
               else (explodePath (getwd())) @ name
  in
    resolveAllSymLinks' (clearPath' absolute'name)
  end

fun absoluteRealPathname (name:string) =
  absoluteRealPathname' (explodePath name)

end
