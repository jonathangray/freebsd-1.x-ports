(* Copyright (c) 1992 by Carnegie Mellon University *)


functor ConnLrValsFun
  (structure Token :TOKEN
   structure AbSyn :ABSYN
   structure ConnOperators :CONN_OPERATORS
     sharing type AbSyn.operator = ConnOperators.operator) :Conn_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
open AbSyn
open ConnOperators
fun stringTerm (str:string) = terminal (STRING_ str)
fun idTerm (str:string) = terminal (ID_ str)
fun idListTerm (lst:string list) = terminal (IDLIST_ lst)

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\000\000\
\\001\000\001\000\010\000\000\000\
\\001\000\001\000\020\000\000\000\
\\001\000\001\000\021\000\000\000\
\\001\000\002\000\013\000\000\000\
\\001\000\003\000\011\000\000\000\
\\001\000\003\000\019\000\000\000\
\\001\000\004\000\000\000\000\000\
\\001\000\005\000\017\000\006\000\016\000\000\000\
\\001\000\007\000\006\000\008\000\005\000\000\000\
\\025\000\000\000\
\\026\000\007\000\006\000\008\000\005\000\000\000\
\\027\000\000\000\
\\028\000\000\000\
\\029\000\000\000\
\\030\000\005\000\017\000\006\000\016\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\001\000\009\000\000\000\
\\035\000\000\000\
\"
val actionRowNumbers =
"\009\000\011\000\010\000\000\000\
\\001\000\012\000\005\000\019\000\
\\004\000\013\000\020\000\008\000\
\\015\000\006\000\002\000\003\000\
\\016\000\014\000\000\000\000\000\
\\017\000\018\000\007\000"
val gotoT =
"\
\\001\000\022\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\005\000\003\000\001\000\000\000\
\\000\000\
\\006\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\013\000\005\000\012\000\000\000\
\\004\000\016\000\005\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\020\000\000\000\
\\006\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 23
val numrules = 11
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; ordof(!s,i) + ordof(!s,i+1) * 256
end
val string_to_list = fn s' =>
    let val len = String.length s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.length s'
 	 fun f ()=
	    if !index < len then convert_row() :: f()
	    else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
	fun f i =
	     if i=numstates then g i
	     else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
	   in f 0 handle Subscript => ()
	   end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.arrayoflist(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.arrayoflist(actionRows) in fn i=>Array.sub(a,i) end
in Array.arrayoflist(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | ID of unit ->  (string)
 | idList of unit ->  (string list) | clause of unit ->  (ast)
 | clauseList of unit ->  (ast list) | section of unit ->  (ast)
 | sectionList of unit ->  (ast list) | start of unit ->  (ast)
end
type svalue = MlyValue.svalue
type result = ast
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 6) => true | (T 5) => true | (T 4) => true | (T 2) => true | _ => false
val preferred_insert =
fn _ => false
val preferred_subst =
fn  _ => nil
val noShift = 
fn (T 3) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "STRING"
  | (T 2) => "SEMI"
  | (T 3) => "EOF"
  | (T 4) => "EXPORT"
  | (T 5) => "IMPORT"
  | (T 6) => "SOURCE"
  | (T 7) => "NAMESPACE"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn (T 0) => MlyValue.ID(fn () => ("bogus")) | 
(T 1) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms = (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: nil
end
structure Actions =
struct 
exception mlyAction of int
val actions = 
let open Header
in
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.sectionList sectionList1,sectionList1left,
sectionList1right))::rest671) => let val result=MlyValue.start(fn _
 => let val sectionList as sectionList1=sectionList1 ()
 in (ast CONN_ sectionList) end
)
 in (LrTable.NT 0,(result,sectionList1left,sectionList1right),rest671)
 end
| (1,(_,(MlyValue.section section1,section1left,section1right))::
rest671) => let val result=MlyValue.sectionList(fn _ => let val 
section as section1=section1 ()
 in ([section]) end
)
 in (LrTable.NT 1,(result,section1left,section1right),rest671) end
| (2,(_,(MlyValue.sectionList sectionList1,_,sectionList1right))::(_,(
MlyValue.section section1,section1left,_))::rest671) => let val result
=MlyValue.sectionList(fn _ => let val section as section1=section1 ()
val sectionList as sectionList1=sectionList1 ()
 in (section :: sectionList) end
)
 in (LrTable.NT 1,(result,section1left,sectionList1right),rest671) end
| (3,(_,(_,_,SEMI1right))::(_,(MlyValue.idList idList1,_,_))::(_,(_,
NAMESPACE1left,_))::rest671) => let val result=MlyValue.section(fn _
 => let val idList as idList1=idList1 ()
 in (ast NAMESPACE_ [idListTerm idList]) end
)
 in (LrTable.NT 2,(result,NAMESPACE1left,SEMI1right),rest671) end
| (4,(_,(_,_,SEMI1right))::(_,(MlyValue.clauseList clauseList1,_,_))::
(_,(MlyValue.STRING STRING1,_,_))::(_,(MlyValue.ID ID1,_,_))::(_,(_,
SOURCE1left,_))::rest671) => let val result=MlyValue.section(fn _ => 
let val ID as ID1=ID1 ()
val STRING as STRING1=STRING1 ()
val clauseList as clauseList1=clauseList1 ()
 in (ast SOURCE_ ((idTerm ID)::((stringTerm STRING)::clauseList))) end
)
 in (LrTable.NT 2,(result,SOURCE1left,SEMI1right),rest671) end
| (5,(_,(MlyValue.clause clause1,clause1left,clause1right))::rest671)
 => let val result=MlyValue.clauseList(fn _ => let val clause as 
clause1=clause1 ()
 in ([clause]) end
)
 in (LrTable.NT 3,(result,clause1left,clause1right),rest671) end
| (6,(_,(MlyValue.clauseList clauseList1,_,clauseList1right))::(_,(
MlyValue.clause clause1,clause1left,_))::rest671) => let val result=
MlyValue.clauseList(fn _ => let val clause as clause1=clause1 ()
val clauseList as clauseList1=clauseList1 ()
 in (clause::clauseList) end
)
 in (LrTable.NT 3,(result,clause1left,clauseList1right),rest671) end
| (7,(_,(MlyValue.idList idList1,_,idList1right))::(_,(MlyValue.ID ID1
,_,_))::(_,(_,IMPORT1left,_))::rest671) => let val result=
MlyValue.clause(fn _ => let val ID as ID1=ID1 ()
val idList as idList1=idList1 ()
 in (ast IMPORT_ [idListTerm (ID::idList)]) end
)
 in (LrTable.NT 4,(result,IMPORT1left,idList1right),rest671) end
| (8,(_,(MlyValue.idList idList1,_,idList1right))::(_,(MlyValue.ID ID1
,_,_))::(_,(_,EXPORT1left,_))::rest671) => let val result=
MlyValue.clause(fn _ => let val ID as ID1=ID1 ()
val idList as idList1=idList1 ()
 in (ast EXPORT_ [idListTerm (ID::idList)]) end
)
 in (LrTable.NT 4,(result,EXPORT1left,idList1right),rest671) end
| (9,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.idList(fn _ => let val ID as ID1=ID1 ()
 in ([ID]) end
)
 in (LrTable.NT 5,(result,ID1left,ID1right),rest671) end
| (10,(_,(MlyValue.idList idList1,_,idList1right))::(_,(MlyValue.ID 
ID1,ID1left,_))::rest671) => let val result=MlyValue.idList(fn _ => 
let val ID as ID1=ID1 ()
val idList as idList1=idList1 ()
 in (ID::idList) end
)
 in (LrTable.NT 5,(result,ID1left,idList1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Conn_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EXPORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SOURCE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NAMESPACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
end
end
