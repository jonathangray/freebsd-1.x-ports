(* Copyright 1989 by AT&T Bell Laboratories *)
(* boot/system.sig *)

signature HOOKS =
sig
  structure Assembly : ASSEMBLY

  (* System.Symbol components *)
  val valSymbol_ref : (unit -> unit) ref
  val tycSymbol_ref : (unit -> unit) ref
  val sigSymbol_ref : (unit -> unit) ref
  val strSymbol_ref : (unit -> unit) ref
  val fctSymbol_ref : (unit -> unit) ref
  val fixSymbol_ref : (unit -> unit) ref
  val labSymbol_ref : (unit -> unit) ref
  val tyvSymbol_ref : (unit -> unit) ref
  val fsigSymbol_ref : (unit -> unit) ref
  val name_ref      : (unit -> unit) ref
  val makestring_ref: (unit -> unit) ref
  val kind_ref      : (unit -> unit) ref
  val nameSpace_ref : (unit -> unit) ref

  (* System.Env components *)
  val emptyEnv_ref     : (unit -> unit) ref
  val concatEnv_ref    : (unit -> unit) ref
  val layerEnv_ref     : (unit -> unit) ref
  val staticPart_ref   : (unit -> unit) ref
  val layerStatic_ref  : (unit -> unit) ref
  val filterEnv_ref    : (unit -> unit) ref
  val filterStaticEnv_ref    : (unit -> unit) ref
  val catalogEnv_ref   : (unit -> unit) ref
  val describe_ref     : (unit -> unit) ref

  (* System.Compile components *)
  val makeSource_ref  : (unit -> unit) ref
  val closeSource_ref : (unit -> unit) ref
  val changeLvars_ref : (unit -> unit) ref
  val elaborate_ref   : (unit -> unit) ref
  val parse_ref       : (unit -> unit) ref
  val compile_ref     : (unit -> unit) ref
  val compileAst_ref  : (unit -> unit) ref
  val execute_ref     : (unit -> unit) ref
  val eval_stream_ref : (unit -> unit) ref
  val use_file_ref    : (unit -> unit) ref
  val use_stream_ref  : (unit -> unit) ref

  (* System.PrettyPrint components *)
  val mk_ppstream_ref : (unit -> unit) ref
  val dest_ppstream_ref   : (unit -> unit) ref
  val begin_block_ref : (unit -> unit) ref
  val end_block_ref   : (unit -> unit) ref
  val add_break_ref   : (unit -> unit) ref
  val add_string_ref  : (unit -> unit) ref
  val add_newline_ref : (unit -> unit) ref
  val clear_ppstream_ref : (unit -> unit) ref 
  val flush_ppstream_ref : (unit -> unit) ref 
  val with_pp_ref     : (unit -> unit) ref 
  val install_pp_ref  : (unit -> unit) ref

  val allocProfReset_ref : (unit -> unit) ref 
  val allocProfPrint_ref : (unit -> unit) ref 
  val prLambda        : (unit -> unit) ref
  val debugInterface  : (int -> unit) ref
  val lookup_r        : (int -> Assembly.object) ref
  val profile_register : (string -> int * int array * int ref) ref
  val toplevelcont    : unit cont ref

  val clear : unit -> unit
  val profile_sregister : (Assembly.object * string -> Assembly.object) ref
  val pervasiveEnvRef : Assembly.object ref
  val topLevelEnvRef : Assembly.object ref
end

signature RUNTIMECONTROL =
sig
  val collected : int ref
  val collectedfrom : int ref
  val gcmessages : int ref
  val majorcollections : int ref
  val minorcollections : int ref
  val ratio : int ref
  val softmax : int ref
  val lastratio : int ref
end

signature MCCONTROL =
sig
  val printArgs : bool ref
  val printRet : bool ref
  val bindContainsVar : bool ref
  val bindExhaustive : bool ref
  val matchExhaustive : bool ref
  val matchRedundant : bool ref
  val expandResult : bool ref
end

signature CGCONTROL =
sig
  structure M68 : sig val trapv : bool ref end
  val tailrecur : bool ref
  val recordopt : bool ref
  val tail : bool ref
  val allocprof : bool ref
  val closureprint : bool ref
  val closureStrategy : int ref
  val lambdaopt : bool ref
  val cpsopt : bool ref
  val rounds : int ref
  val path : bool ref
  val betacontract : bool ref
  val eta : bool ref
  val selectopt : bool ref
  val dropargs : bool ref
  val deadvars : bool ref
  val flattenargs : bool ref
  val switchopt : bool ref
  val handlerfold : bool ref
  val branchfold : bool ref
  val arithopt : bool ref
  val betaexpand : bool ref
  val unroll : bool ref
  val knownfiddle : bool ref
  val invariant: bool ref
  val targeting: int ref
  val lambdaprop: bool ref
  val newconreps : bool ref
  val unroll_recur : bool ref
  val hoistup : bool ref
  val hoistdown : bool ref
  val maxregs : int ref
  val recordcopy : bool ref
  val tagopt : bool ref
  val recordpath : bool ref
  val machdep : bool ref
  val misc1 : bool ref
  val misc2 : bool ref
  val misc3 : int ref
  val misc4 : int ref
  val hoist : bool ref
  val argrep : bool ref
  val reduce : bool ref
  val bodysize : int ref
  val reducemore : int ref
  val alphac : bool ref
  val comment : bool ref
  val knownGen : int ref
  val knownClGen : int ref
  val escapeGen : int ref
  val calleeGen : int ref
  val spillGen : int ref
  val foldconst : bool ref
  val etasplit : bool ref
  val printLambda : bool ref
  val printit : bool ref
  val printsize : bool ref
  val scheduling : bool ref
  val cse : bool ref
  val optafterclosure : bool ref
  val calleesaves : int ref
  val extraflatten : bool ref
  val uncurry : bool ref
  val ifidiom : bool ref
  val comparefold : bool ref
  val csehoist : bool ref
  val rangeopt : bool ref
  val floatargs : int ref
  val floatvars : int ref
  val floatreg_params : bool ref
  val icount : bool ref
  val representations : bool ref
end

signature PRINTCONTROL =
sig
  type outstream
  val printDepth : int ref
  val printLength : int ref
  val stringDepth : int ref
  val printLoop : bool ref
  val signatures : int ref
  val pathnames : int ref
  val out : outstream ref
  val linewidth : int ref
  val say : string -> unit 
  val flush: unit -> unit
end

signature DEBUG =
sig
  val debugging : bool ref
  val getDebugf : (int -> 'a) ref
  val interface : (int -> ('a -> 'b)) ref
end

signature CONTROL =
sig
  structure Runtime : RUNTIMECONTROL
  structure MC : MCCONTROL
  structure CG : CGCONTROL
  structure Print : PRINTCONTROL
  structure Debug : DEBUG
  val allocProfReset : unit -> unit
  val allocProfPrint : unit -> unit
  val prLambda : (unit -> unit) ref
  val debugging : bool ref
  val primaryPrompt : string ref
  val secondaryPrompt : string ref
  val internals : bool ref
  val weakUnderscore : bool ref
  val interp : bool ref
  val debugLook : bool ref
  val debugCollect : bool ref
  val debugBind : bool ref
  val saveLambda : bool ref
  val saveLvarNames : bool ref
  val timings : bool ref
  val reopen : bool ref
  val markabsyn : bool ref
  val indexing : bool ref
  val instSigs : bool ref
  val quotation : bool ref
end

signature TIMER =
sig
  datatype time = TIME of {sec : int, usec : int}
  type timer
  val start_timer : unit -> timer
  val check_timer : timer -> time
  val check_timer_sys: timer -> time
  val check_timer_gc: timer -> time
  val makestring : time -> string
  val add_time : time * time -> time
  val sub_time : time * time -> time
  val earlier : time * time -> bool
end

signature TAGS =
  sig
    type tag
    val width_tags : int (* number of bits to hold a tag *)
    val power_tags : int (* 2 ^ width_tags *)
    val max_length : int (* one greater than max length value *)
  (* tag values *)
    val tag_record : tag
    val tag_pair : tag
    val tag_array : tag
    val tag_string : tag
    val tag_embedded_string : tag
    val tag_bytearray : tag
    val tag_reald : tag
    val tag_embedded_reald : tag
    val tag_realdarray : tag
    val tag_variant : tag (* currently not used *)
    val tag_special : tag
    val tag_backptr : tag
  (* build a descriptor from a tag and length *)
    val make_desc : (int * tag) -> int
  (* fixed descriptors *)
    val desc_pair : int
    val desc_reald : int
    val desc_embedded_reald : int
  (* special descriptors *)
    val desc_special : int
    val special_evaled_susp : int
    val special_unevaled_susp : int
    val special_weak : int
    val special_nulled_weak : int
  end

signature STATS =
sig
  structure Timer : TIMER
  val lines : int ref
  val parse : Timer.time ref
  val debuginstrum: Timer.time ref
  val translate : Timer.time ref
  val codeopt : Timer.time ref
  val convert : Timer.time ref
  val hoist : Timer.time ref
  val cpsopt : Timer.time ref
  val closure : Timer.time ref
  val globalfix : Timer.time ref
  val spill : Timer.time ref
  val codegen : Timer.time ref
  val schedule : Timer.time ref
  val freemap : Timer.time ref
  val execution : Timer.time ref
  val codesize : int ref
  val update : Timer.time ref * Timer.time -> unit
  val summary : unit -> unit
  val reset : unit -> unit
end

signature CINTERFACE =
sig
  exception CFunNotFound of string
  exception SysError of (int * string)
  exception SystemCall of string

  type time

  val c_function : string -> ('a -> 'b)
  val c_string : string -> string (* insure that a string is safe to pass to C *)
  val wrap_sysfn : string -> ('a -> 'b) -> 'a -> 'b

(* C functions *)
  val argv	    : unit -> string list
  val environ	    : unit -> string list
  val gethostname : unit -> string
  val exec	    : (string * string list * string list) -> (int * int)
  val system      : string -> int
  val export      : int -> bool
  val blas	    : (int * 'a) -> int
  val salb	    : string -> 'a
  val gettime     : unit -> {usr : time, sys : time, gc : time}
  val setitimer   : (int * time * time) -> unit
  val gc          : int -> unit
  val syscall	    : (int * string list) -> int
(* System calls *)
  val exit	    : int -> 'a
  val getpid	    : unit -> int
  val getuid	    : unit -> int
  val getgid	    : unit -> int
  val chdir	    : string -> unit
  val gethostid   : unit -> string
  val gettimeofday   : unit -> time
end (* CINTERFACE *)

signature SYSIO =
sig
  type bytearray
  type time

  type fd
  eqtype fileid
  datatype fname	= DESC of fd | PATH of string
  datatype mode	= O_READ | O_WRITE | O_APPEND
  datatype whence	= L_SET | L_INCR | L_XTND
  datatype access	= A_READ | A_WRITE | A_EXEC
  datatype file_type	= F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK

  val dtablesize	: int
  val openf		: (string * mode) -> fd
  val closef		: fd -> unit
  val unlink		: string -> unit
  val pipe		: unit -> (fd * fd)
  val connect_unix	: string -> fd
  val connect_inet	: (string * string) -> fd
  val link		: (string * string) -> unit
  val symlink		: (string * string) -> unit
  val mkdir		: (string * int) -> unit
  val dup		: fd -> fd

  val read		: (fd * bytearray * int) -> int
  val readi		: (fd * bytearray * int * int) -> int
  val write		: (fd * bytearray * int) -> unit
  val writei		: (fd * bytearray * int * int) -> unit
  val writev		: (fd * (bytearray * int) list) -> unit
  val send_obd	: (fd * bytearray * int) -> unit
  val getdirent	: fd -> string list
  val readlink	: string -> string
  val truncate	: (fname * int) -> unit
  val lseek		: (fd * int * whence) -> int

  val getmod		: fname -> int
  val chmod		: (fname * int) -> unit
  val umask		: int -> int

  val access		: (string * access list) -> bool
  val isatty		: fd -> bool
  val fionread	: fd -> int
  val getfid		: fname -> fileid
  val ftype		: fname -> file_type
  val getownid	: fname -> (int * int)
  val fsize		: fname -> int
  val atime		: fname -> time
  val ctime		: fname -> time
  val mtime		: fname -> time
  val select		: (fd list * fd list * fd list * time option)
			  -> (fd list * fd list * fd list)
end (* SYSIO *)

signature SIGNALS =
sig
  datatype signal
    = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGVTALRM | SIGTERM | SIGURG
    | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2 | SIGPROF
    | SIGTSTP | SIGCONT (* not yet supported *)
    | SIGGC
  val setHandler : (signal * ((int * unit cont) -> unit cont) option) -> unit
  val inqHandler : signal -> ((int * unit cont) -> unit cont) option
  val maskSignals : bool -> unit
  val masked : unit -> bool
  val pause : unit -> unit (* sleep until the next signal *)
end

signature DIRECTORY =
sig
  val isDir : string -> bool
      (* return true, if path is a directory *)
  exception NotDirectory
  val listDir : string -> string list
      (* return a list of the files in the specified directory,
	 raises NotDirectory *)
  val cd : string -> unit
      (* change directory, raises NotDirectory*)
  val getWD : unit -> string
      (* return the current working directory *)
end (* DIRECTORY *)

signature SYMBOL =
sig
  type symbol
  datatype namespace =
     VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
     LABspace | TYVspace | FSIGspace
  val valSymbol : string -> symbol
  val tycSymbol : string -> symbol
  val sigSymbol : string -> symbol
  val strSymbol : string -> symbol
  val fctSymbol : string -> symbol
  val fixSymbol : string -> symbol
  val labSymbol : string -> symbol
  val tyvSymbol : string -> symbol
  val fsigSymbol : string -> symbol
  val name      : symbol -> string
  val makestring: symbol -> string
  val kind      : symbol -> string
  val nameSpace : symbol -> namespace
  val makeSymbol: namespace * string -> symbol
end

signature ENVIRONMENT =
sig
  exception Unbound
  type environment
  type staticEnv
  val emptyEnv     : unit -> environment
  val concatEnv    : environment * environment -> environment
  val layerEnv     : environment * environment -> environment
  val staticPart   : environment -> staticEnv
  val layerStatic  : staticEnv * staticEnv -> staticEnv
  val filterEnv    : environment * symbol list -> environment
  val filterStaticEnv
		   : staticEnv * symbol list -> staticEnv
  val catalogEnv   : staticEnv -> symbol list
  val describe     : staticEnv -> symbol -> unit
  val pervasiveEnvRef : environment ref
  val topLevelEnvRef : environment ref
end

signature AST = sig

  type fixity
  type symbol
  val infixleft : int -> fixity
  val infixright : int -> fixity

  (* to mark positions in files *)
  type filePos
  (* symbolic path (Modules.spath) *)
  type path

  (* EXPRESSIONS *)

  datatype exp
    = VarExp of path		(* variable *)
    | FnExp of rule list		(* abstraction *)
    | AppExp of {function:exp,argument:exp}
				  (* application *)
    | CaseExp of{expr:exp,rules:rule list}
				  (* case expression *)
    | LetExp of {dec:dec,expr:exp} (* let expression *)
    | SeqExp of exp list		(* sequence of expressions *)
    | IntExp of int		(* integer *)
    | RealExp of string		(* floating point coded by its string *)
    | StringExp of string		(* string *)
    | RecordExp of (symbol * exp) list	(* record *)
    | TupleExp of exp list	(* tuple (derived form) *)
    | SelectorExp of symbol	(* selector of a record field *)
    | ConstraintExp of {expr:exp,constraint:ty}
				  (* type constraint *)
    | HandleExp of {expr:exp, rules:rule list}
				  (* exception handler *)
    | RaiseExp of exp		(* raise an exception *)
    | IfExp of {test:exp, thenCase:exp, elseCase:exp}
				  (* if expression (derived form) *)
    | AndalsoExp of exp * exp	(* andalso (derived form) *)
    | OrelseExp of exp * exp	(* orelse (derived form) *)
    | VectorExp of exp list       (* vector *)
    | WhileExp of {test:exp,expr:exp}
				  (* while (derived form) *)
    | MarkExp of exp * filePos * filePos	(* mark an expression *)

  (* RULE for case functions and exception handler *)
  and rule = Rule of {pat:pat,exp:exp}

  (* PATTERN *)
  and pat = WildPat				(* empty pattern *)
	  | VarPat of path			(* variable pattern *)
	  | IntPat of int			(* integer *)
	  | RealPat of string			(* floating point number *)
	  | StringPat of string			(* string *)
	  | RecordPat of {def:(symbol * pat) list, flexibility:bool}
						(* record *)
	  | TuplePat of pat list		(* tuple *)
	  | AppPat of {constr:path,argument:pat}(* application *)
	  | ConstraintPat of {pattern:pat,constraint:ty}
						  (* constraint *)
	  | LayeredPat of {varPat:pat,expPat:pat}	(* as expressions *)
          | VectorPat of pat list                 (* vector pattern *)
	  | MarkPat of pat * filePos * filePos	(* mark a pattern *)

  (* STRUCTURE EXPRESSION *)
  and strexp = VarStr of path			(* variable structure *)
	     | StructStr of dec			(* defined structure *)
	     | AppStr of path * (strexp * bool) list (* application *)
	     | LetStr of dec * strexp		(* let in structure *)
	     | MarkStr of strexp * filePos * filePos (* mark *)

  (* FUNCTOR EXPRESSION *)
  and fctexp = VarFct of path * fsigexp option	(* functor variable *)
	     | FctFct of {			(* definition of a functor *)
		  params   : (symbol option * sigexp) list,
		  body	   : strexp,
		  constraint : sigexp option}
	     | LetFct of dec * fctexp
	     | AppFct of path * (strexp * bool) list * fsigexp option
						  (* application *)
	     | MarkFct of fctexp * filePos * filePos (* mark *)

  (* SIGNATURE EXPRESSION *)
  and sigexp = VarSig of symbol			(* signature variable *)
	     | SigSig of spec list		(* defined signature *)
	     | MarkSig of sigexp * filePos * filePos	(* mark *)

  (* FUNCTOR SIGNATURE EXPRESSION *)
  and fsigexp = VarFsig of symbol			(* funsig variable *)
	      | FsigFsig of {param: (symbol option * sigexp) list, def:sigexp}
						  (* defined funsig *)
	      | MarkFsig of fsigexp * filePos * filePos	(* mark a funsig *)

  (* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
  and spec = StrSpec of (symbol * sigexp) list			(* structure *)
	   | TycSpec of ((symbol * tyvar list) list * bool)	(* type *)
	   | FctSpec of (symbol * fsigexp) list			(* functor *)
	   | ValSpec of (symbol * ty) list			(* value *)
	   | DataSpec of db list				(* datatype *)
	   | ExceSpec of (symbol * ty option) list		(* exception *)
	   | FixSpec of  {fixity: fixity, ops: symbol list} 	(* fixity *)
	   | ShareSpec of path list			(* structure sharing *)
	   | ShatycSpec of path list			(* type sharing *)
	   | LocalSpec of spec list * spec list		(* local specif *)
	   | IncludeSpec of symbol			(* include specif *)
	   | OpenSpec of path list			(* open structures *)
	   | MarkSpec of spec * filePos * filePos	(* mark a spec *)

  (* DECLARATIONS (let and structure) *)
  and dec	= ValDec of vb list			(* values *)
	  | ValrecDec of rvb list			(* recursive values *)
	  | FunDec of fb list				(* recurs functions *)
	  | TypeDec of tb list				(* type dec *)
	  | DatatypeDec of {datatycs: db list, withtycs: tb list}
							  (* datatype dec *)
	  | AbstypeDec of {abstycs: db list, withtycs: tb list, body: dec}
							  (* abstract type *)
	  | ExceptionDec of eb list			(* exception *)
	  | StrDec of strb list				(* structure *)
	  | AbsDec of strb list				(* abstract struct *)
	  | FctDec of fctb list				(* functor *)
	  | SigDec of sigb list				(* signature *)
	  | FsigDec of fsigb list				(* funsig *)
	  | LocalDec of dec * dec				(* local dec *)
	  | SeqDec of dec list				(* sequence of dec *)
	  | OpenDec of path list				(* open structures *)
	  | OvldDec of symbol * ty * exp list	(* overloading (internal) *)
	  | FixDec of {fixity: fixity, ops: symbol list}  (* fixity *)
	  | ImportDec of string list		(* import (unused) *)
	  | MarkDec of dec * filePos * filePos		(* mark a dec *)

  (* VALUE BINDINGS *)
  and vb = Vb of {pat:pat, exp:exp}
	 | MarkVb of vb * filePos * filePos

  (* RECURSIVE VALUE BINDINGS *)
  and rvb = Rvb of {var:symbol, exp:exp, resultty: ty option}
	  | MarkRvb of rvb * filePos * filePos

  (* RECURSIVE FUNCTIONS BINDINGS *)
  and fb = Fb of {var:symbol, clauses:clause list}
	 | MarkFb of fb * filePos * filePos

  (* CLAUSE: a definition for a single pattern in a function binding *)
  and clause = Clause of {pats: pat list, resultty: ty option, exp:exp}

  (* TYPE BINDING *)
  and tb = Tb of {tyc : symbol, def : ty, tyvars : tyvar list}
	 | MarkTb of tb * filePos * filePos

  (* DATATYPE BINDING *)
  and db = Db of {tyc : symbol, tyvars : tyvar list,
		  def : (symbol * ty option) list}
	 | MarkDb of db * filePos * filePos

  (* EXCEPTION BINDING *)
  and eb = EbGen of {exn: symbol, etype: ty option} (* Exception definition *)
	 | EbDef of {exn: symbol, edef: path}	  (* defined by equality *)
	 | MarkEb of eb * filePos * filePos

  (* STRUCTURE BINDING *)
  and strb = Strb of {name: symbol,def: strexp,constraint: sigexp option}
	   | MarkStrb of strb * filePos * filePos

  (* FUNCTOR BINDING *)
  and fctb = Fctb of {name: symbol,def: fctexp}
	   | MarkFctb of fctb * filePos * filePos

  (* SIGNATURE BINDING *)
  and sigb = Sigb of {name: symbol,def: sigexp}
	   | MarkSigb of sigb * filePos * filePos

  (* FUNSIG BINDING *)
  and fsigb = Fsigb of {name: symbol,def: fsigexp}
	    | MarkFsigb of fsigb * filePos * filePos

  (* TYPE VARIABLE *)
  and tyvar = Tyv of symbol
	    | MarkTyv of tyvar * filePos * filePos

  (* TYPES *)
  and ty 
      = VarTy of tyvar			(* type variable *)
      | ConTy of symbol list * ty list	(* type constructor *)
      | RecordTy of (symbol * ty) list 	(* record *)
      | TupleTy of ty list		(* tuple *)
      | MarkTy of ty * filePos * filePos	(* mark type *)
end (* structure Ast *)

signature CODE =
  sig
    structure IO : sig type instream end
    type code
    val mkCode : string -> code
    val inputCode : (IO.instream * int) -> code
    val sizeOf : code -> int
    val apply : code -> ('a -> 'b)
	(* NOTE: this cannot be more precise, since mo files and code compiled
	 * interactively have different types.
	 *)
  end (* CODE *)

signature PRETTYPRINT =
sig
  type ppstream
  type ppconsumer
  datatype break_style = CONSISTENT | INCONSISTENT
  val mk_ppstream    : ppconsumer -> ppstream
  val dest_ppstream  : ppstream -> ppconsumer
  val add_break      : ppstream -> int * int -> unit
  val add_newline    : ppstream -> unit
  val add_string     : ppstream -> string -> unit
  val begin_block    : ppstream -> break_style -> int -> unit
  val end_block      : ppstream -> unit
  val clear_ppstream : ppstream -> unit
  val flush_ppstream : ppstream -> unit
  val with_pp : ppconsumer -> (ppstream -> unit) -> unit
  val install_pp : string list -> (ppstream -> 'a -> unit) -> unit  (* unsafe! *)
  val pp_to_string : int -> (ppstream -> 'a -> unit) -> 'a -> string
end

signature COMPILE =
sig
  structure PP : sig type ppconsumer end
  structure IO : sig type instream type outstream end
  structure Ast : sig type dec end
  type source
  type staticUnit
  type codeUnit
  type compUnit (* = staticUnit * codeUnit *)
  exception Compile of string
  val makeSource  : string * int * IO.instream * bool * PP.ppconsumer -> source
  val closeSource : source -> unit
  val changeLvars : staticUnit -> staticUnit
  val elaborate   : source * staticEnv -> staticUnit
  val parse	  : source * staticEnv -> Ast.dec * staticEnv
  val compile     : source * staticEnv -> staticUnit * codeUnit
  val compileAst  : Ast.dec * staticEnv * source option -> compUnit
  val execute     : (staticUnit * codeUnit) * environment -> environment
  val eval_stream : IO.instream * environment -> environment
  val use         : string -> unit
  val use_stream  : IO.instream -> unit
end

signature CLEANUP =
sig
  datatype clean_mode
    = CleanForExportML | CleanForExportFn | CleanForQuit | CleanForInit
  val addCleaner : (string * (clean_mode -> unit)) -> bool
  val removeCleaner : string -> unit
  val cleanup : clean_mode -> unit
  val shutdown : unit -> 'a
end (* CLEANUP *)

signature WEAK = 
sig
  type 'a weak
  val weak : 'a -> 'a weak
  val strong : 'a weak -> 'a option
  type weak'
  val weak' : 'a -> weak'
  val strong' : weak' -> bool
end (* WEAK *)

signature SUSP =
  sig
    type 'a susp
    val delay : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
  end (* SUSP *)

(* fully polymorphic (and thus not typesafe) versions of the continuation
 * operations
 *)
signature POLY_CONT =
sig
  type 'a cont
  val callcc : ('a cont -> 'a) -> 'a
  val throw : 'a cont -> 'a -> 'b
  type 'a control_cont
  val capture : ('a control_cont -> 'a) -> 'a
  val escape : 'a control_cont -> 'a -> 'b
end

signature UNSAFE =
sig
  type object
  type instream and outstream
  structure Assembly : ASSEMBLY
  structure CInterface : CINTERFACE
  structure SysIO : SYSIO
  structure CleanUp : CLEANUP
  structure Weak : WEAK
  structure Susp : SUSP
  structure PolyCont : POLY_CONT
  val boxed : 'a -> bool
  val ordof : 'a * int -> int
  val slength : 'a -> int		(* string length *)
  val objLength : 'a -> int		(* object length *)
  val getObjTag : 'a -> int		(* get width_tag bits of object descriptor *)
  val special : (int * 'a) -> 'b
  val setSpecial : ('a * int) -> unit
  val getSpecial : 'a -> int
  val store : string * int * int -> unit
  val bstore : Assembly.A.bytearray * int * int -> unit
  val subscript : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
  val subscriptv : 'a vector * int -> 'a
  val subscriptf : Assembly.A.realarray * int -> real
  val updatef : Assembly.A.realarray * int * real -> unit
  val getvar : unit -> 'a
  val setvar : 'a -> unit
  val gethdlr : unit -> 'a
  val sethdlr : 'a -> unit
(*****
  val boot : 'a -> ('b -> 'c)		(* code -> ('b -> 'c) *)
*****)
  val cast : 'a -> 'b
  val blast_write : outstream * 'a -> int
  val blast_read : instream * int -> 'a
  val create_s : int -> string
  val create_b : int -> Assembly.A.bytearray
  val store_s : string * int * int -> unit
  val lookup_r : (int -> object) ref
  val lookup : int -> object
  val toplevelcont : unit cont ref
  val pstruct : {core: object, initial: object, math: object} ref
  exception Boxity
  val tuple : object -> object vector
  val string : object -> string
  val real : object -> real
  val int : object -> int

  datatype datalist = DATANIL | DATACONS of (string * (unit -> unit) * datalist)
  val datalist : datalist
  val profiling : bool ref
  val sprofiling : bool ref
end

signature SYSTEM =
sig
  structure ByteArray : BYTEARRAY
  structure Hooks : HOOKS
  structure Symbol : SYMBOL
  structure Env : ENVIRONMENT
  structure Ast : AST
  structure Code : CODE
  structure Compile: COMPILE
  structure PrettyPrint : PRETTYPRINT
  structure Print : PRINTCONTROL
  structure Control : CONTROL
  structure Tags : TAGS
  structure Timer : TIMER
  structure Stats : STATS
  structure Unsafe : UNSAFE
  structure Signals : SIGNALS
  structure Directory : DIRECTORY
  val exn_name : exn -> string
  val version : string
  val architecture : string ref
  val runtimeStamp : string ref
  val interactive : bool ref
  val system : string -> int (* execute a shell command *)
  val argv : unit -> string list
  val environ : unit -> string list
  val errorMatch : string ref
end
