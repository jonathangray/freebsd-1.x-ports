(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure FileList :FILELIST = struct

fun inDir (recursive:bool, dirs :string list) :string list =
  let fun pathlists dirname filename filetype (dirlst, filelst) =
        if (ordof(filename,0) = (ord ".")) then (dirlst, filelst)
          else
            let val pathname = Pathname.mergePathnames [dirname, filename] in
              case filetype of
                 DirFile.FILE => (dirlst, pathname::filelst)
               | DirFile.SYMLINK => (dirlst, pathname::filelst)
               | DirFile.DIR => (pathname::dirlst, filelst)
            end
      fun dirpathlists (dirname :string, result) =
        DirFile.fold pathlists [DirFile.FOLLOWDIRS] dirname result

      val (dirlst, filelst) = fold dirpathlists dirs ([],[])
      val morefiles =
             if (not recursive) orelse (dirlst = []) then []
               else inDir (recursive, dirlst)
  in
    filelst @ morefiles
  end

fun readFilenames cwd (filename, accum) =
  let val absoluteFilename = Pathname.absoluteName cwd filename
      val dir = Pathname.directoryPart absoluteFilename
      fun fixPath x = Pathname.relativeName cwd (Pathname.absoluteName dir x)
      val stream = open_in absoluteFilename
      fun pathlist (line:string) =
        map fixPath (StringXtra.breakAtBlanks line)
      fun work (instream:instream)(init:string list) :string list =
        let fun action acc =
              if end_of_stream instream then acc
                else action (acc @ (pathlist (input_line instream)))
        in action init end
  in
    IO_Stream.withInStream stream work accum
  end handle (Io msg) => (print ("\n% "^msg^"\n  File ignored.\n\n"); accum)

fun inFile (pointerFiles:string list) :string list =
  let val cwd = Pathname.getwd () in
    fold (readFilenames cwd) pointerFiles []
  end


fun cshExpansion (pattern:string) =
  StringXtra.breakAtBlanks
    (Execute.firstLine ("/bin/csh", ["-F", "-c", "echo " ^ pattern]))


fun isEqual name (name2 :string, result :bool) = result orelse (name = name2)

fun isInList lst name = fold (isEqual name) lst false

fun filter'skips lst (name:string, result:string list) =
  if isInList lst name then result else name::result

fun skipFiles (skipNames:string list) (names:string list) :string list =
  fold (filter'skips skipNames) names []


fun check'ext name (ext :string, result :bool) =
  result orelse ((Pathname.extension name) = ext)

fun check'ext'list (ext'list:string list) (name:string, result:string list) =
  if (ext'list = []) orelse (fold (check'ext name) ext'list false)
    then name::result else result

fun extensionsOnly (exts:string list) (names:string list) :string list =
  fold (check'ext'list exts) names []

end
