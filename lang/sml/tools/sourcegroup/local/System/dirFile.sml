(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure DirFile :DIRFILE = struct

structure SysIO = System.Unsafe.SysIO

val mergePathnames = Pathname.mergePathnames
val splitDirFile = Pathname.splitDirFile
val splitDirFile' = Pathname.splitDirFile'
val resolveSymLinks = Pathname.resolveSymLinks
val resolveSymLinks' = Pathname.resolveSymLinks'

datatype mapOptions = FOLLOWDIRS | FOLLOWFILES | RECURSIVE | ALPHA
datatype fileType = FILE | DIR | SYMLINK

fun get'options (oplist :mapOptions list) =
  let val followdirs = ref false
      val followfiles = ref false
      val recursive = ref false
      val alpha = ref false
      fun mark [] = () 
        | mark (h::t) =
           (case h of
	       FOLLOWDIRS => followdirs := true
	     | FOLLOWFILES => followfiles := true
	     | RECURSIVE => recursive := true
	     | ALPHA => alpha := true;
	    mark t)
  in mark oplist; (!followdirs, !followfiles, !recursive, !alpha) end        

fun checkFile dirname fname followdirs followfiles =
  let val pathname :string = mergePathnames [dirname, fname] in
    (case SysIO.ftype (SysIO.PATH pathname) of
        SysIO.F_SYMLINK =>
          if followdirs orelse followfiles then
              let val newpathname = resolveSymLinks' (dirname, fname) in
                if System.Directory.isDir newpathname
                  then if followdirs
		         then ((splitDirFile newpathname), DIR, true)
                         else ((dirname, fname), SYMLINK, true)
                  else if followfiles
		         then ((splitDirFile newpathname), FILE, true)
                         else ((dirname, fname), SYMLINK, true)
              end
            else ((dirname, fname), SYMLINK, true)
      | _ =>
        if System.Directory.isDir pathname
          then ((dirname, fname), DIR, true)
          else ((dirname, fname), FILE, true))
     handle (System.Unsafe.CInterface.SystemCall _)
              => ((dirname, fname), FILE, false)
   end

val string'sort = ListSort.sort String.<

fun scan (operate :string -> string -> fileType -> unit)
	 (options :mapOptions list) (dirname :string) :unit =
 let val (followdirs, followfiles, recursive, alpha) = get'options options
     fun dmap dir =
      let val dirlist = System.Directory.listDir dir
          val dir' = if dir = "." then "" else dir
          val files = if alpha then string'sort dirlist else dirlist
          fun operate'object (name) =
            if (name = ".") orelse (name = "..") then () else
              let val ((newdir,newfile),fileType,exists)
                    = checkFile dir' name followdirs followfiles in
                if not exists then () else
                 (operate newdir newfile fileType;
                  case fileType of
                     DIR => if recursive
                              then dmap (mergePathnames [newdir, newfile])
                              else ()
                   | _ => ())
              end
      in map operate'object files; () end
 in dmap dirname end

fun fold (operate :string -> string -> fileType -> 'a -> 'a)
	 (options :mapOptions list) (dirname :string) (acc :'a) :'a =
 let val (followdirs, followfiles, recursive, alpha) = get'options options
     fun dfold dir (a :'a) :'a =
      let val dirlist = System.Directory.listDir dir
          val dir' = if dir = "." then "" else dir
          val files = if alpha then string'sort dirlist else dirlist
          fun operate'object (name, (result :'a)) :'a =
            if (name = ".") orelse (name = "..") then result else
              let val ((newdir,newfile),fileType,exists)
                    = checkFile dir' name followdirs followfiles in
                if not exists then result else
                  let val r = operate newdir newfile fileType result in
                    case fileType of
                       DIR =>
                         if recursive
                           then dfold (mergePathnames [newdir, newfile]) r
                           else r
                     | _ => r
                  end
              end
      in List.fold operate'object files a end
 in dfold dirname acc end

val zeroTime :System.Timer.time = System.Timer.TIME{sec=0,usec=0}
fun isZeroTime (System.Timer.TIME {sec,usec}) = (sec=0) andalso (usec=0)

fun timeModified (pathname:string) :System.Timer.time =
  if pathname = "" then zeroTime
    else (SysIO.mtime (SysIO.PATH (resolveSymLinks pathname)))
            handle _ => zeroTime

fun timeModifiedInSeconds filename :int =
  let val System.Timer.TIME{sec, ...} = timeModified filename
  in sec end

fun fileExists filename =
  let val _ = SysIO.mtime(SysIO.PATH filename) in true end
    handle _ => false

fun print'name (dirname :string) (filename :string) (filetype :fileType) =
  (case filetype of
     DIR =>     print " DIR: " |
     FILE =>    print "FILE: " |
     SYMLINK => print "SYML: ";
  print (mergePathnames [dirname, filename]); print "\n")

fun listFiles dirname map'options = scan print'name map'options dirname
end
