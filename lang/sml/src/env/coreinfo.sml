(* Copyright 1989 by AT&T Bell Laboratories *)
(* coreinfo.sml *)

(* info extracted from Core structure *)

structure CoreInfo : COREINFO =
struct

  open Access Types Variables Modules ModuleUtil Lambda ErrorMsg

  val exnBind = ref bogusEXN
  val exnMatch = ref bogusEXN
  val exnOrd = ref bogusEXN
  val exnRange = ref bogusEXN
  val exnSubscript = ref bogusEXN
  val exnRealSubscript = ref bogusEXN
  val stringequalPath = ref[0]
  val polyequalPath = ref[0]
  val registerPath = ref[0]
  val sregisterPath = ref[0]
  val forcerPath = ref[0]
  val corePath = ref([] : int list)
  val coreLty = ref ((fn _ => impossible "CoreInfo.coreLty1") : int -> lty)
  val vector0Path = ref [0]
  val getDebugVar = ref(mkVALvar (Symbol.varSymbol "getDebug"))
  val errorMatchPath = ref Variables.ERRORvar

  fun resetCore () = 
      (exnBind := bogusEXN;
       exnMatch := bogusEXN;
       exnOrd := bogusEXN;
       exnRange := bogusEXN;
       exnSubscript := bogusEXN;
       exnRealSubscript := bogusEXN;
       stringequalPath := [0];
       polyequalPath := [0];
       registerPath := [0];
       sregisterPath := [0];
       forcerPath := [0];
       corePath := [];
       coreLty := ((fn _ => impossible "CoreInfo.coreLty2") : int -> lty);
       vector0Path := [0];
       getDebugVar = ref(mkVALvar (Symbol.varSymbol "getDebug"));
       errorMatchPath := Variables.ERRORvar)

  fun setCore(env,spath) =
      let val err = (fn _ => ErrorMsg.impossible)
          val svCore as STRvar{access=Access.PATH[lvCore],binding,...} =
                     lookSTR(env,spath,err)
          val ltyCore = TransBinding.transStrLty binding
          fun extractPath name = 
	      let val spath' = spath @ [Symbol.varSymbol name]
		  val VARbind(VALvar{access=PATH p,...}) = 
		        lookVARCON (env,spath',err)
	       in p end
          fun extractVariable name =
              let val spath' = spath @ [Symbol.varSymbol name]
                  val VARbind var =
                        lookVARCON (env,spath',fn _ => ErrorMsg.impossible)
              in var end
	  fun coreExn name = 
	      let val spath' = spath @ [Symbol.varSymbol name]
	      in lookEXN (env,spath',err)
	      end
       in exnBind := coreExn "Bind";
	  exnMatch := coreExn "Match";
          exnOrd := coreExn "Ord";
          exnRange := coreExn "Range";
          exnSubscript := coreExn "Subscript";
  	  exnRealSubscript := coreExn "RealSubscript";
	  stringequalPath := extractPath "stringequal";
	  forcerPath := extractPath "forcer_p";
	  polyequalPath := extractPath "polyequal";
	  registerPath := extractPath "profile_register";
	  sregisterPath := extractPath "profile_sregister";
          errorMatchPath := extractVariable "errorMatch";
	  getDebugVar := let val name = Symbol.varSymbol "getDebug"
			     val VARbind x =
				 lookVARCON (env,spath @ [name],
						 err)
			  in x
			 end;
          vector0Path := extractPath "vector0";
	  corePath := tl(!stringequalPath);
          coreLty := (fn x => (if (x = lvCore) then ltyCore
                               else impossible "CoreInfo.coreLty3"))
      end

end (* CoreInfo *)
