(* source.sig *)

signature SOURCE =
sig
    type charpos (* = int *)
    type region (* = charpos * charpos *)
    type inputSource (* = {fileName: string,
		           linePos: int list ref,
			   lineNum: int ref,
			   interactive: bool,
			   sourceStream: instream, 
			   anyErrors: bool ref,
			   errConsumer: PrettyPrint.ppconsumer,
			   indexStream: outstream option} *)
    val newSource: string * int * instream * bool * PrettyPrint.ppconsumer
	           * outstream option 
		   -> inputSource
    val closeSource: inputSource -> unit
    val filepos: inputSource -> charpos -> string * int * int
end