                    (* the parser *)

functor Parser(Expression:EXPRESSION): PARSER =
   struct
      structure E = Expression
      open E

      datatype Token = TokOPENBR   |
                       TokCLOSEBR   |
                       TokTRUE   |
                       TokFALSE   |
                       TokIF   |
                       TokTHEN   |
                       TokELSE   |
                       TokOPENSQ   |
                       TokCLOSESQ   |
                       TokCOMMA   |
                       TokPLUS   |
                       TokMINUS   |
                       TokTIMES   |
                       TokLET   |
                       TokREC   |
                       TokIDENT of string   |
                       TokEQUALS   |
                       TokNIL   |
                       TokCOLCOL   |
                       TokIN   |
                       TokEND   |
                       TokFN   |
                       TokARROW   |
                       TokNUMBER of int

      fun exists f [] = false
        | exists f (first::rest) = f first orelse exists f rest;
      
      exception Nth
      fun nth([],n)= raise Nth
        | nth(first::rest, 0)= first
        | nth(first::rest,n)= nth(rest,n-1);

      fun length [] = 0
        | length (hd::tl) = 1+ length tl;

      fun IsNumber(s) = not(exists (fn str => str < "0" orelse str > "9")
                                   (explode s)
                           )

      fun BadLetter(s) = (s < "a" orelse s > "z")
                         andalso (s < "A" orelse s > "Z")

      fun IsIdent(s) = not(exists BadLetter (explode s))

      fun MakeNumber(digits) =
         let fun MakeNumber'(d :: drest, result) =
                    MakeNumber'(drest, result * 10 + ord(d) - ord("0"))   |
                 MakeNumber'(nil, result) = result
         in  MakeNumber'(explode digits, 0)
         end

      fun IsASpace(str) = str <= " "

      fun IsAlphanum "" = false
        | IsAlphanum str =
             let val ch = ord str
             in  (ch >= ord "a" andalso ch <= ord "z")
                 orelse (ch >= ord "A" andalso ch <= ord "Z")
                 orelse (ch >= ord "0" andalso ch <= ord "9")
             end

      fun Solo(sym) = exists (fn x => x = sym)
                             ["(", ")", "[", "]", ",", "+", "-", "*"]

      fun Glue(accum, this :: rest) =
             if IsASpace(this) then
                (if accum = "" then Glue("", rest)
                               else accum :: Glue("", rest))
             else if (IsAlphanum accum <> IsAlphanum this) then
                (if accum = "" then Glue(this, rest)
                               else accum :: Glue(this, rest))
             else if Solo(this) orelse Solo(accum) then
                (if accum = "" then Glue(this, rest)
                               else accum :: Glue(this, rest))
             else Glue(accum^this, rest)   |
          Glue(accum, nil) = if accum = "" then [] else [accum]

      fun Lex(input) = Glue("", explode input)
                    
      exception Lexical of string

      fun MakeToken("(") = TokOPENBR   |
          MakeToken(")") = TokCLOSEBR   |
          MakeToken("true") = TokTRUE   |
          MakeToken("false") = TokFALSE   |
          MakeToken("if") = TokIF   |
          MakeToken("then") = TokTHEN   |
          MakeToken("else") = TokELSE   |
          MakeToken("[") = TokOPENSQ   |
          MakeToken(",") = TokCOMMA   |
          MakeToken("]") = TokCLOSESQ   |
          MakeToken("+") = TokPLUS   |
          MakeToken("-") = TokMINUS   |
          MakeToken("*") = TokTIMES   |
          MakeToken("let") = TokLET   |
          MakeToken("rec") = TokREC   |
          MakeToken("=") = TokEQUALS   |
          MakeToken("nil") = TokNIL   |
          MakeToken("::") = TokCOLCOL   |
          MakeToken("in") = TokIN   |
          MakeToken("end") = TokEND   |
          MakeToken("fn") = TokFN   |
          MakeToken("=>") = TokARROW   |
          MakeToken(s) = if IsNumber(s) then TokNUMBER(MakeNumber s)
                         else if IsIdent(s) then TokIDENT(s)
                         else raise Lexical(s)

				(* Parsing *)
      exception SyntaxError of Token list

      fun syntaxError(x) = raise SyntaxError(x)

      fun ParseExpr(TokOPENBR :: rest): Expression * Token list =
             (case ParseExpr(rest) of
                 (E, TokCLOSEBR :: tail) => ParseExprTail(E, tail)   |
                 (_, tail) => syntaxError(tail)
             )   |

          ParseExpr(TokNUMBER(i) :: rest) =
             ParseExprTail(NUMBERexpr(i), rest)  |

          ParseExpr(TokNIL :: rest) =
             ParseExprTail(LISTexpr [], rest)  |

          ParseExpr(TokTRUE :: rest) =
             ParseExprTail(BOOLexpr(true), rest)  |

          ParseExpr(TokFALSE :: rest) =
             ParseExprTail(BOOLexpr(false), rest)  |

          ParseExpr(TokIDENT(ident) :: rest) =
             ParseExprTail(IDENTexpr(ident), rest)   |

          ParseExpr(TokOPENSQ :: TokCLOSESQ :: rest) =
             ParseExprTail(LISTexpr(nil), rest)   |

          ParseExpr(TokOPENSQ :: rest) =
             (case ParseList(rest) of
                 (Es, TokCLOSESQ :: tail) =>
                    ParseExprTail(LISTexpr(Es), tail)   |

                 (_, tail) => syntaxError(tail)
             )   |

          ParseExpr(TokLET :: TokIDENT(ident) :: TokEQUALS :: rest) =
             (case ParseExpr(rest) of
                (binding, TokIN :: tail) =>
                   (case ParseExpr(tail) of
                       (scope, TokEND :: tail') =>
                          ParseExprTail(DECLexpr(ident, binding, scope),
                                        tail')   |
                       (_, tail') => syntaxError(tail')
                   )   |
                (_, tail) => syntaxError(tail)
             )   |

          ParseExpr(TokLET :: TokREC :: TokIDENT(ident) :: TokEQUALS :: rest) =
             (case ParseExpr(rest) of
                (binding, TokIN :: tail) =>
                   (case ParseExpr(tail) of
                       (scope, TokEND :: tail') =>
                          ParseExprTail(RECDECLexpr(ident, binding, scope),
                                        tail')   |
                       (_, tail') => syntaxError(tail')
                   )   |
                (_, tail) => syntaxError(tail)
             )   |

          ParseExpr(TokIF :: rest) =
             (case ParseExpr(rest) of
                (ifpart, TokTHEN :: tail) =>
                   (case ParseExpr(tail) of
                      (thenpart, TokELSE :: tail') =>
                         let val (elsepart, tail'') = ParseExpr(tail')
                         in  ParseExprTail(CONDexpr(ifpart, thenpart, elsepart),
                                           tail'')
                         end   |
                      (_, tail) => syntaxError(tail)
                   )   |

                (_, tail) => syntaxError(tail)
             )   |

          ParseExpr(TokFN :: TokIDENT(ident) :: TokARROW :: rest) =
             let val (body, tail) = ParseExpr(rest)
             in  ParseExprTail(LAMBDAexpr(ident, body), tail)
             end   |

          ParseExpr(junk) = syntaxError(junk)

          and ParseExprTail(E, TokPLUS :: tail) =
                 let val (E', tail') = ParseExpr(tail)
                 in  ParseExprTail(SUMexpr(E, E'), tail')
                 end   |

              ParseExprTail(E, TokMINUS :: tail) =
                 let val (E', tail') = ParseExpr(tail)
                 in  ParseExprTail(DIFFexpr(E, E'), tail')
                 end   |

              ParseExprTail(E, TokTIMES :: tail) =
                 let val (E', tail') = ParseExpr(tail)
                 in  ParseExprTail(PRODexpr(E, E'), tail')
                 end   |

              ParseExprTail(E, TokEQUALS :: tail) =
                 let val (E', tail') = ParseExpr(tail)
                 in  ParseExprTail(EQexpr(E, E'), tail')
                 end   |

              ParseExprTail(E, TokCOLCOL :: tail) =
                 let val (E', tail') = ParseExpr(tail)
                 in  ParseExprTail(CONSexpr(E, E'), tail')
                 end   |

              ParseExprTail(E, TokOPENBR :: rest) =
                 (case ParseExpr(rest) of
                     (E', TokCLOSEBR :: tail) =>
                        ParseExprTail(APPLexpr(E, E'), tail)   |
                     (_, tail) => syntaxError(tail)
                 )   |

              ParseExprTail(E, tail) = (E, tail)

          and ParseList(tokens) =
                 (case ParseExpr(tokens) of
                     (E, TokCOMMA :: rest) =>
                        let val (E', tail) = ParseList(rest)
                        in  (E :: E', tail)
                        end   |

                     (E, tail) => ([E], tail)
                 )

					(* WhichItem - given the remaining
					   input tokens, magically find the
					   actual lexical string which barfed *)

      fun WhichItem(LexStrings, toks: Token list) =
         nth(rev LexStrings, length(toks) - 1)
         handle Nth => "<end-of-input>"

      exception Syntax of string

      fun parse(input) =
         let val LexStrings = Lex(input)
         in  (case ParseExpr(map MakeToken LexStrings) of
                 (E, nil) => E   |
                 (_, junk) => syntaxError(junk)
             ) handle SyntaxError(toks) =>
                         raise Syntax(WhichItem(LexStrings, toks))
         end
   end;
