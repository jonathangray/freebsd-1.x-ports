(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure CShellDir :CSHELLDIR = struct

exception CShell

fun currentTime () =
  let val getTimeOfDay :unit -> System.Timer.time =
            System.Unsafe.CInterface.c_function "timeofday"
      val System.Timer.TIME {sec,...} = getTimeOfDay ()
  in
    sec
  end

fun getWD () = Execute.firstLine ("/bin/pwd", [])

fun print'sep [] sep = ()
  | print'sep (a::(rest as (b::c))) sep =
      (String.print a; String.print sep; print'sep rest sep)
  | print'sep (a::[]) sep = String.print a

fun homeDir () =
  let val env = System.Unsafe.CInterface.environ ()
      fun find'home [] = ""
        | find'home (head::tail) =
            if substring(head,0,4) = "HOME" then substring(head,5,size head-5)
              else find'home tail
  in
    find'home env
  end;

val dirlist :string list ref = ref []

fun getwd () =
  let val wd = getWD () in
    case !dirlist of
       [] => dirlist := [wd]
     | (head::tail) => dirlist := (wd::tail);
    wd
  end

fun pwd () = (print (getwd()); print "\n")

fun get'dirlist () =
  (if (!dirlist) = [] then (getwd(); ()) else ();
   !dirlist)

fun dirs () = (print'sep (get'dirlist()) " "; print "\n")

fun new'cwd dirname =
  let val (pathlist as (first::tail)) = Pathname.explodePath dirname
      val pathlist' =
            if first = "" then pathlist
              else if first = "~" then (Pathname.explodePath(homeDir())) @ tail
              else (Pathname.explodePath(hd(get'dirlist()))) @ pathlist
  in
    Pathname.implodePath (Pathname.clearPath' pathlist')
  end

fun cd dirname = 
  let val newdir = new'cwd dirname
  in
    System.Directory.cd newdir;
    case (!dirlist) of
       [] => dirlist := [newdir]
     | (head::tail) => dirlist := (newdir::tail)
  end

fun pushd dirname = 
  let val newdir = new'cwd dirname in
    System.Directory.cd newdir;
    dirlist := newdir::(get'dirlist());
    dirs()
  end

fun popd () =
 (case (!dirlist) of
     [] => ()
   | (head::[]) => ()
   | (first::second::tail) =>
       (System.Directory.cd second;
        dirlist := (second::tail));
  dirs())

fun swapd () =
 (case (!dirlist) of
     [] => ()
   | (head::[]) => ()
   | (first::second::tail) =>
       (System.Directory.cd second;
        dirlist := (second::first::tail));
  dirs())

end
