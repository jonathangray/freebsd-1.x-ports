(* Copyright (c) 1992 by Carnegie Mellon University *)

signature Conn_TOKENS =
sig
type ('a,'b) token
type svalue
val NAMESPACE:  'a * 'a -> (svalue,'a) token
val SOURCE:  'a * 'a -> (svalue,'a) token
val IMPORT:  'a * 'a -> (svalue,'a) token
val EXPORT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature Conn_LRVALS=
sig
structure Tokens : Conn_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
