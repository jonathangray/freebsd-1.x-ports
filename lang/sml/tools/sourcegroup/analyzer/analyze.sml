(* Copyright (c) 1992 by Carnegie Mellon University *)

functor AnalyzeFun
 (structure Traverse :TRAVERSE
  structure Parse    :PARSE
  structure Process  :PROCESS
  sharing Process.SC.MD = Traverse.MD
 ) :ANALYZE = struct

structure SC = Process.SC

fun connections filename 
      :(string list * string list * string list * string list) *
       (string list * string list * string list * string list) =
  let val ast = Parse.parseSource filename
      val decl = Traverse.traverse ast
      val shared'table = SC.sharedTable ()
      val scopes = Process.processDecl shared'table decl
  in
    SC.connections scopes
  end

fun report filename =
  let val ast = Parse.parseSource filename
      val decl = Traverse.traverse ast
      val shared'table = SC.sharedTable ()
      val scopes = Process.processDecl shared'table decl
      fun reporter strm () =
        (output (strm, "source sml \"" ^ filename ^ "\"");
         SC.printAll strm scopes)
  in
    reporter std_out ()
  end

fun testFile runDiff  (shared'table:SC.modtable) filename =
  let val ast = Parse.parseSource filename
      val decl = Traverse.traverse ast
      val scopes = Process.processDecl shared'table decl
      fun reporter strm () =
        (output (strm, "source sml \"" ^ filename ^ "\"");
         SC.printAll strm scopes)
      val outfile = filename^".out"
      val (out'dir,out'file) = Pathname.splitDirFile outfile
      val expected'file = Pathname.mergePathnames [out'dir,"expected",out'file]
  in
    IO_Stream.withOutStream (open_out outfile) reporter ();
    if not runDiff then () else
      (System.system ("diff "^expected'file^" "^outfile); ())
  end

fun scan directory =
  let val shared'table = SC.sharedTable ()
      fun scanner d f t =
        let val e = Pathname.extension f in
          if not ((e="sml") orelse (e="sig")) then ()
            else testFile true shared'table (Pathname.mergePathnames[d,f])
        end
  in
    print ("["^directory^"]\n");
    DirFile.scan scanner [DirFile.ALPHA] directory
  end

val testDir =
  "/afs/cs.cmu.edu/misc/sml/build/sourcegroup/src/3.0/analyzer/test/"

fun test () =
  let val dir = Pathname.getwd() in
    System.Directory.cd testDir;
    app scan ["a","b","c","d","e"];
    System.Directory.cd dir
  end

fun interface (commandLine:string list, _:string list) =
  case commandLine of
     [] => ()
   | (first::[]) => print ("? usage: "^first^" filename\n")
   | (first::second::[]) => report second
   | (first::second::third::rest) => print ("? usage: "^first^" filename\n")

end

