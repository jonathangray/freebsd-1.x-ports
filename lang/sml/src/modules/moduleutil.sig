signature MODULEUTIL =
  sig

    exception ErrorStructure
    exception UnboundComponent of Modules.spath

    val mkStructure : Modules.binding Env.env * Symbol.symbol list
                      -> Modules.Structure
    val eqFsig : Modules.FctSignature * Modules.FctSignature -> bool
    val transType : Modules.Structure -> Types.ty -> Types.ty
    val lvarOfBinding : Modules.binding -> int option
    val last : 'a list -> 'a

    val appendAccess : Access.access * Access.slot list -> Access.access
    val bogusCONbind : Modules.binding
    val bogusFCTvar : Modules.functorVar
    val bogusSTRvar : Modules.structureVar
    val compose : ('a * Modules.trans list) option * Modules.trans list 
		  -> Modules.trans list
    val eqOrigin : Modules.Structure * Modules.Structure -> bool
    val eqSign : Modules.Signature * Modules.Signature -> bool
    val eqSignature : Modules.Signature * Modules.Signature -> bool
    val findPath : Symbol.symbol list * 'a * ('a * 'b -> bool) * 
		   (Symbol.symbol list * ('c -> 'd) -> 'b) -> string
    val getFctStamp : Modules.Functor -> Stamps.stamp
    val getOrigin : Modules.Structure -> Modules.Structure
    val getSigPos : Modules.Signature -> Symbol.symbol -> Modules.strpos
    val getSigPosGen : Modules.Signature -> Symbol.symbol -> Modules.binding
    val getSigTPos : Modules.Signature -> Symbol.symbol -> int
    val getSignStamp : Modules.Signature -> Stamps.stamp
    val getStrPath : Modules.Structure -> Modules.spath
    val getStrPos : Modules.Structure -> Symbol.symbol -> Modules.strpos
    val getStrStamp : Modules.Structure -> Stamps.stamp
    val getStrTPos : Modules.Structure -> Symbol.symbol -> int
    val lookArTYC : (Modules.binding Env.env * Normalize.env option) 
		    * Symbol.symbol list * int 
   		    * ErrorMsg.complainer -> Types.tycon
    val lookBinding : Modules.Structure * Symbol.symbol list * Access.slot list
		      -> Modules.binding
    val lookBindingSTR : Modules.Structure * Symbol.symbol list 
			 -> Modules.structureVar
    val lookBindingTYC : Modules.Structure * Symbol.symbol list -> Types.tycon
    val lookEXN : Modules.binding Env.env * Modules.spath *
		 ErrorMsg.complainer -> Types.datacon
    val lookFCT : Modules.binding Env.env * Modules.spath * 
		  ErrorMsg.complainer -> Modules.functorVar
    val lookFIX : Modules.binding Env.env * Symbol.symbol -> Fixity.fixity
    val lookFSIG : Modules.binding Env.env * Symbol.symbol
		   * ErrorMsg.complainer 
		   -> Modules.FctSignature
    val lookFormalBinding : Modules.env * Symbol.symbol list
			    -> Modules.binding * int list
    val lookGen : (Modules.binding * int list * Modules.spath -> 'a) * 'a 
		  -> Modules.binding Env.env * Modules.spath
		  * ErrorMsg.complainer -> 'a
    val lookSIG : Modules.binding Env.env * Symbol.symbol 
		  * ErrorMsg.complainer -> Modules.Signature
    val lookSTR : Modules.binding Env.env * Modules.spath
		  * ErrorMsg.complainer -> Modules.structureVar
    val lookShortVARCON : Modules.binding Env.env * Symbol.symbol
			  * ErrorMsg.complainer
			  -> Modules.binding
    val lookTYC : Modules.binding Env.env * Modules.spath
		  * ErrorMsg.complainer -> Types.tycon
    val lookVARCON : Modules.binding Env.env * Modules.spath
		     * ErrorMsg.complainer -> Modules.binding
    val makeEnv : Modules.Structure * Access.slot list
		  -> Modules.binding Env.env
    val notInitialLowerCase : string -> bool
    val openSigStructure : 
      Modules.binding Env.env * Modules.spath * Modules.binding Env.env
      * ErrorMsg.complainer -> Modules.binding Env.env
    val openStructureVar : Modules.binding Env.env * Modules.structureVar
			   -> Modules.env
    val sortEnvBindings : Modules.binding Env.env
			  -> (Symbol.symbol * Modules.binding) list
    val transPosFct : Modules.Structure -> int list -> Modules.Functor
    val transPosStr : Modules.Structure -> int list -> Modules.Structure
    val transPosTycon : Modules.Structure -> int list -> Types.tycon
  end
