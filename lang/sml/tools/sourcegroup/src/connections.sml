(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor ConnectionsFun
  (structure ListSort :LISTSORT
   structure Hash :HASH
   structure Hasher :HASHER
   structure AbSyn :ABSYN
   structure NameRefTable :NAMEREFTABLE
   structure ConnOperators :CONN_OPERATORS
     sharing type ConnOperators.operator = AbSyn.operator
   structure Conn :CONN
     sharing Conn.AbSyn = AbSyn
) :CONNECTIONS = struct

structure NameRefTable = NameRefTable

exception ConnectionsError
val spaceSize = 37

val say = System.Print.say

fun err (isInternal:bool) (msg:string) :'a =
  (say "? SourceGroup Connections: ";
   if isInternal then say "(internal error) " else ();
   say msg; say "\n";
   raise ConnectionsError)

fun getToolName (decl:AbSyn.ast) :string =
  case AbSyn.oper (AbSyn.subterm decl 1) of
     (ConnOperators.ID_ name) => name
   | _ => err true "getToolName"

fun getSourceName (decl:AbSyn.ast) :string =
  case AbSyn.oper (AbSyn.subterm decl 2) of
     (ConnOperators.STRING_ name) => name
   | _ => err true "getSourceName"

fun getConnList (decl:AbSyn.ast) :AbSyn.ast list =
  let val (_::_::conn'list) = AbSyn.subterms decl in
    conn'list
  end

fun getIdList (term:AbSyn.ast) :string list =
  case AbSyn.oper term of
     (ConnOperators.IDLIST_ lst) => lst
   | _ => err true "getIdList"

fun string'equal ((x, y):string*string) :bool = (x = y)
val normalize = ListSort.unique'sort string'equal String.<

fun enterConn namespace'table
              (imports:NameRefTable.t)(exports:NameRefTable.t)(connDecl:AbSyn.ast) =
  let val table = case AbSyn.oper connDecl of
                     ConnOperators.IMPORT_ => imports
                   | ConnOperators.EXPORT_ => exports
      val (spaceName, nameList) =
            case AbSyn.oper (AbSyn.subterm connDecl 1) of
               ConnOperators.IDLIST_ (space::names) => (space, normalize names)
             | _ => err true "enterConn"
      val spaceNameH = Hasher.hasher spaceName
      val _ = (Hash.lookup' namespace'table spaceNameH; ())
                handle Hash.NotFound =>
                  (say "% SourceGroup Connections: Warning namespace ";
                   say spaceName; say " not declared\n")
  in
    case NameRefTable.lookup table spaceNameH of
       NONE => NameRefTable.enter table spaceNameH nameList
     | (SOME existingList) =>
         let val newList = normalize (nameList @ existingList) in
           NameRefTable.enter table spaceNameH newList
         end
  end

fun do'source'decl
      namespace'table (decl:AbSyn.ast) :string*string*NameRefTable.t*NameRefTable.t =
  let val importTable = NameRefTable.create spaceSize
      val exportTable = NameRefTable.create spaceSize
      val toolName = getToolName decl
      val sourceName = getSourceName decl
      val connList = getConnList decl
  in
    map (enterConn namespace'table importTable exportTable) connList;
    (toolName, sourceName, importTable, exportTable)
  end

fun new'namespace namespace'table (id:string) =
  Hash.enter namespace'table (Hasher.hasher id) true

fun do'section namespace'table ((section:AbSyn.ast), accum) =
  case AbSyn.oper section of
     (ConnOperators.NAMESPACE_) =>
       (map (new'namespace namespace'table)
            (getIdList (AbSyn.subterm section 1));
        accum)
   | (ConnOperators.SOURCE_) =>
       ((do'source'decl namespace'table section)::accum)
   | _ => err true "do'section"

fun get (filename:string) :(string*string*NameRefTable.t*NameRefTable.t) list =
  let val section'list = (AbSyn.subterms (Conn.parse false filename))
                        handle _ => err false "Syntax Error"
      val namespace'table = Hash.createDefault ([]:bool list)
      val _ = map (new'namespace namespace'table)
                  ["structure", "functor", "signature", "funsig"]
      val result = fold (do'section namespace'table) (rev section'list) []
  in result
  end

end
