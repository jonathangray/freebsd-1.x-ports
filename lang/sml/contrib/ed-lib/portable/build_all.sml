(* BUILD FILE - ENTIRE LIBRARY VERSION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author


DESCRIPTION

   This file builds the entire library.


NOTES

   Any system specific load file should be loading before using this
   file to build the library itself.  System specific files define
   loadEntry, loadSig, etc., and resolve any name clashes with existing
   structures.


SEE ALSO

   build_make.skel, build_core.skel.


RCS LOG

$Log: build_all.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.14  1991/10/22  19:08:05  db
Added call to load "../CHANGES", to get version string.

Revision 1.13  1991/10/22  18:55:13  db
Added calls to load FULL_ORD and FULL_SEQ_ORD.

Revision 1.12  91/09/13  16:44:19  16:44:19  db (Dave Berry)
'Added call to load Hash structure.

Revision 1.11  91/03/08  17:01:42  17:01:42  db (Dave Berry)
Added call to load EqFinMap.

Revision 1.10  91/02/22  18:54:37  18:54:37  db (Dave Berry)
Removed calls to load SequenceToEqPrint and it's derivatives, as
these have been deleted.

Revision 1.9  91/02/11  21:00:23  21:00:23  db (Dave Berry)
Changed this file to include the new signatures and structures, and
the changes of names, created by the major reorganisation of the library.

Revision 1.8  91/02/05  11:45:31  11:45:31  db (Dave Berry)
Added build_core.skel to SEE ALSO section.

Revision 1.7  91/02/04  15:25:38  15:25:38  db (Dave Berry)
Replaced code for opening structures that match the INSTREAM_TYPE,
OUTSTREAM_TYPE and GENERAL_TYPES signatures with loadEntry "InstreamType",
loadEntry "OutstreamType" and loadEntry "GeneralTypes".   These files
were originally added for the Make system, but they can simplify this
file too.
Also now load OutstreamType before loading the core files, to avoid
a type clash.

Revision 1.6  91/01/30  19:07:15  19:07:15  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.
Defined loadLibrary to do nothing.

Revision 1.5  91/01/30  17:50:14  17:50:14  db (Dave Berry)
Now load String structure earlier because it's used to parse other
types of values.

Revision 1.4  91/01/25  20:04:42  20:04:42  db (Dave Berry)
Changed signature names to all upper case.

Revision 1.3  91/01/24  17:36:33  17:36:33  db (Dave Berry)
Removed command to load "Load.sml", which no longer exists.

Revision 1.2  91/01/24  16:35:53  16:35:53  db (Dave Berry)
Moved loadFun etc. here for system specific files.
Added references holding current dirctory names.
Added commands to load Make system and Core entries.

Revision 1.1  90/12/20  14:50:29  14:50:29  db (Dave Berry)
Initial revision


*)

local
  fun subst _ _ [] = []
  |   subst p a (h::t) =
    if p h then a :: subst p a t
    else h :: subst p a t

  val loadPrefix = ref "";
  val loadSigPrefix = ref "../signatures/";
in
  fun loadEntry s =
    let val l = subst (fn x => x = "'") "_" (explode s)
    in NonStandard.use (!loadPrefix ^ implode l ^ ".sml")
    end

  fun loadSig s =
    let val l = subst (fn x => x = "'") "_" (explode s)
    in NonStandard.use (!loadSigPrefix ^ implode l ^ ".sml")
    end

  fun loadLocalSig s =
    let val l = subst (fn x => x = "'") "_" (explode s)
    in NonStandard.use (!loadPrefix ^ implode l ^ ".sml")
    end

  fun setLoadPrefix s = (loadPrefix := s)

  fun setLoadSigPrefix s = (loadSigPrefix := s)
end;

NonStandard.use "../CHANGES";

(* If OutstreamType is loaded before Make then it should also be loaded
   before the Core files.  This is because Make uses NonStandard.flush_out,
   which must be defined on the outstream type in scope in the body of Make. *)

loadEntry "GeneralTypes";
loadEntry "InstreamType";
loadEntry "OutstreamType";

setLoadPrefix "Core/";
loadEntry "Vector";
loadEntry "Array";
loadEntry "Utils";
setLoadPrefix "";

loadSig "EQUALITY";
loadSig "ORDERING";
loadSig "EQ_ORD";
loadSig "PRINT";
loadSig "OBJECT";
loadSig "PARSE";
loadSig "SEQ_PARSE";
loadSig "EQ_PRINT";
loadSig "SEQUENCE";
loadSig "SEQ_ORD";
loadSig "MONO_SEQ_PARSE";
loadSig "ORD_PRINT";
loadSig "EQTYPE_ORD";
loadSig "EQTYPE_PRINT";
loadSig "FULL_SEQ_ORD";
loadSig "FULL_ORD";

loadEntry "System";
loadEntry "ListPair";

loadEntry "List";
loadEntry "ListSort";
loadEntry "LexOrdList";
loadEntry "Vector";
loadEntry "StringType";
loadEntry "Ascii";
loadEntry "String";
loadEntry "StringParse";
loadEntry "StringListOps";
loadEntry "AsciiOrdString";
loadEntry "LexOrdString";
loadEntry "Bool";
loadEntry "BoolParse";
loadEntry "Int";
loadEntry "IntParse";
loadEntry "Real";

loadEntry "ListParse";
loadEntry "VectorParse";
loadEntry "Array";
loadEntry "ArrayParse";

loadEntry "MonoVector";
loadEntry "MonoArray";
loadEntry "MonoVectorParse";
loadEntry "MonoArrayParse";

loadEntry "Pair";
loadEntry "PairParse";
loadEntry "StreamPair";
loadEntry "User";
loadEntry "Byte";
loadEntry "ByteParse";
loadEntry "Const";
loadEntry "Ref";

loadEntry "BoolVector";
loadEntry "ByteVector";
loadEntry "ByteArray";
loadEntry "BoolArray";

loadEntry "Combinator";
loadEntry "Memo";
loadEntry "EqFinMap";
loadEntry "Hash";

loadEntry "Set";
loadEntry "EqSet";
loadEntry "MonoSet";

setLoadPrefix "Make/";
loadEntry "Make";
setLoadPrefix "";

fun loadLibrary _ = "All library entries have been loaded";
