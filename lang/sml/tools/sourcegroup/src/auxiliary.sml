(* Copyright (c) 1992 by Carnegie Mellon University *)

functor AuxiliaryFun
  (structure ListSort :LISTSORT
   structure IO_Stream :IO_STREAM
   structure Pathname :PATHNAME
   structure DirFile :DIRFILE
   structure Group :GROUP
  ) :AUXILIARY = struct

 structure Util = Group.Util
 structure Pathname = Pathname
 structure Data = Group.Data
 structure Hash = Data.Hash
 structure Hasher = Data.Hasher
 structure NameRefTable = Group.NameRefTable

open Data

exception SourceGroupInternalError
exception Skip
exception Quit

val say = System.Print.say
val hasher = Hasher.hasher
val smlH = hasher "sml"
val signatureH =  hasher "signature"
val functorH = hasher "functor"
val structureH = hasher "structure"

val modtime = DirFile.timeModified

val stringSort = ListSort.sort String.<

fun common ([]:string list, _:string list) :string list = []
  | common (_,[]) = []
  | common (list1 as (head1::tail1), list2 as (head2::tail2)) =
      if head1 = head2 then head1::(common(tail1,tail2))
        else if head1 < head2
               then common (tail1, list2)
               else common (list1, tail2)

fun printCommon exportTable (spaceH as (space,_)) importList =
  let val exportList = NameRefTable.getNameList exportTable spaceH
      val commonList = common (stringSort importList, stringSort exportList)
  in
    case commonList of
       [] => ()
     | (head::tail) =>
         (say ("  "^space^" "); Util.printSep " " commonList; say "\n")
  end
      
fun printConnections name1 name2 (first:fileInfo) (second:fileInfo) =
  let val (F {imports,...}) = first
      val (F {exports,...}) = second in
    say ("File "^name1^" imports the following from "^name2^":\n");
    NameRefTable.scan (!imports) (printCommon (!exports))
  end

fun circle (group:groupInfo) (trail:string list) =
  let val cwd = Pathname.getwd()
      val relative = Pathname.relativeName cwd
      val _ = say "? Circular dependency detected:\n";
      val infoList = map (Group.lookupFile' o hasher) (rev trail)
      fun iter (info'list :fileInfo list) =
        case info'list of
           [] => ()
         | (name::[]) => ()
         | (first::(rest as (second::tail))) =>
             let val (F {nameH=(name1,_),...}) = first
                 val (F {nameH=(name2,_),...}) = second
             in
               printConnections (relative name1) (relative name2) first second;
               iter rest
             end
  in
    iter infoList
  end

fun connections' (out:outstream) (g:group) =
  let val group = Group.findGroup g
      val cwd = Pathname.getwd()
      val rel = Pathname.relativeName cwd
      val pr = (outputc out)
      fun prs x = (pr x; pr " ")
      fun prq x = (pr "\""; pr x; pr "\"")
      fun printNameRefList (direction:string) (space,_) nameList =
        case nameList of
           [] => ()
         | (head::tail) =>
             (pr "\n  "; pr direction; prs space; map prs nameList; ()) 
      fun conn (filename,_)(F{toolH as (tool,_),imports,exports,...}:fileInfo)=
        (pr "source "; prs tool; prq (rel filename);
         NameRefTable.scan (!imports) (printNameRefList "import ");
         NameRefTable.scan (!exports) (printNameRefList "export ");
         pr ";\n\n")
      fun do'group (group:groupInfo)(isLibrary)(acc:unit):unit =
        if isLibrary then () else Group.scanFiles group conn
  in
    Group.groupsFold group false do'group ()
  end

fun connections (name:string) (g:group) =
  IO_Stream.withOutStream (open_out name) connections' g

fun newer (x,y) = System.Timer.earlier (modtime y, modtime x)

fun doLexFile (sourceName:string)
              ((grm,lex,sml):string list * string list * string list) =
 let val callString = ("sml-lex "^sourceName)
     val lexOutputFile = sourceName ^ ".sml"
     val status = if newer (sourceName, lexOutputFile)
                    then (say ("[" ^ callString ^ "]\n");
                          System.system callString)
                    else 0
 in (grm, sourceName::lex, lexOutputFile::sml) end

fun doYaccFile (sourceName:string)
               ((grm,lex,sml):string list * string list * string list) =
 let val callString = ("sml-yacc "^sourceName)
     val sigFile = sourceName ^ ".sig"
     val smlFile = sourceName ^ ".sml"
     val status = if (newer (sourceName, sigFile)) orelse
                     (newer (sourceName, smlFile))
                    then (say ("[" ^ callString ^ "]\n");
                          System.system callString)
                    else 0
 in (sourceName::grm, lex, sigFile::smlFile::sml) end

fun doSMLFile (sourceName:string)
              ((grm,lex,sml):string list * string list * string list) =
  (grm, lex, sourceName::sml)

fun arrangeFiles (filelist:string list) =
 case filelist of
    [] => ([],[],[])
  | (filename::tail) =>
      let val (name1, ext1) = Pathname.splitFileExtension filename
          val (name2, ext2) = Pathname.splitFileExtension name1
      in
        case (ext2, ext1) of
           ("grm","sig") => arrangeFiles tail
         | ("grm","sml") => arrangeFiles tail
         | ("lex","sml") => arrangeFiles tail
         | (_,"grm") => doYaccFile filename (arrangeFiles tail)
         | (_,"lex") =>  doLexFile filename (arrangeFiles tail)
         | (_)       =>  doSMLFile filename (arrangeFiles tail)
     end

end
