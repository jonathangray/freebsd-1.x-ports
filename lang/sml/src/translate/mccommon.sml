structure MCCommon = 
  struct

    open Types Variables Lambda Absyn ErrorMsg

    datatype simp = VARsimp of var | RECORDsimp of simp list

    fun mkRECORDpat (RECORDpat{fields, flex, typ, ...}) pats =
          RECORDpat {pats=ref pats, fields=fields, flex=flex, typ=typ}
      | mkRECORDpat _ pats = impossible "non record passed to mkRECORDpat"

    datatype path
      = RECORDPATH of path list
      | PIPATH of int * path
      | VPIPATH of int * ty * path
      | VLENPATH of path
      | DELTAPATH of con * ty option * path
      | ROOTPATH
      
    type dconinfo = datacon * ty option

    datatype dectree
      = CASETEST of 
          path * Access.conrep list * (con * ty option * dectree) list 
               * dectree option
      | ABSTEST0 of path * dconinfo * dectree * dectree
      | ABSTEST1 of path * dconinfo * dectree * dectree
      | RHS of int
      | BIND of path * dectree
      
    exception Lookup

    fun lookup (a, (b,c)::d) = 
           if a = b then c else lookup(a, d) 
      | lookup _ = raise Lookup


    fun conEq(DATACON{rep=r1,...},DATACON{rep=r2,...}) = r1 = r2
    fun conEq'((DATACON{rep=r1,...},_), (DATACON{rep=r2,...},_)) = r1 = r2

    fun constantEq (INTcon n, INTcon n') = n = n'
      | constantEq (REALcon r, REALcon r') = r = r'
      | constantEq (STRINGcon s, STRINGcon s') = s = s'
      | constantEq (VLENcon n, VLENcon n') = n = n'
      | constantEq (DATAcon(_,krep,_), DATAcon(_,krep',_)) = krep = krep'
      | constantEq _ = false

    val pathEq  = (op =) : path * path -> bool

    fun abstract x = false
    fun template x = false
    fun isAnException x = false
    fun signOfCon (DATACON{sign,...}) = sign
    fun unary (Types.DATACON{const,...},_) = const

end

