%Start  CODE SHIP COMM EMP GLOB PLA
%p 5500
%e 10000
%n 1000

le     [a-zA-Z]
as     [a-zA-Z0-9]
di     [0-9]
nl     \n
ex     [eE]

%%
    static int savestate;
    static char first = 1;
    if (first)
      {
        BEGIN CODE;
        first = 0;
      }

<CODE>default   {             return(xDEFAULT); }
<CODE>empire    { BEGIN EMP; return(xEMPIRE);  }
<CODE>ship      { BEGIN SHIP; return(xSHIP);    }
<CODE>planet    { BEGIN PLA; return(xPLANET);  }
<CODE>global    { BEGIN GLOB; return(xGLOB);  }

<CODE,SHIP,EMP,GLOB,PLA>"//".*    { ; }
<CODE>"/*"      { savestate = CODE; BEGIN COMM; }
<SHIP>"/*"      { savestate = SHIP; BEGIN COMM; }
<EMP>"/*"      { savestate = EMP; BEGIN COMM; }
<GLOB>"/*"      { savestate = GLOB; BEGIN COMM; }
<PLA>"/*"      { savestate = PLA; BEGIN COMM; }

<COMM>"*/"      { BEGIN savestate; }
<COMM>.        { ; }
<COMM>\n        { ; }

<CODE,PLA,EMP,SHIP,GLOB>;            		{ BEGIN CODE; return(xSEMI);  }
<CODE,PLA,EMP,SHIP,GLOB>\,            		{ return xCOMMA;  }

<CODE,PLA,EMP,SHIP,GLOB>=       { return(xEQ);  }

<SHIP,GLOB>planet		{ return (xPLANET); }
<SHIP,GLOB>weapon		{ return(xWEAPON);       }
<SHIP,GLOB>engine		{ return(xENGINE);       }
<SHIP,GLOB>cool                 { return(xCOOL);       }
<SHIP,GLOB>phaser               { return(xPHASER);      }
<SHIP,GLOB>torp                 { return(xTORP);      }
<SHIP,GLOB>shield               { return(xSHIELD);    }
<SHIP,GLOB>cloak                { return(xCLOAK);     }
<SHIP,GLOB>mine			{ return xMINE; }
<SHIP,GLOB>lock                 { return(xLOCK);      }
<SHIP,GLOB>warp			{ return xWARP; }
<SHIP,GLOB>max			{ return xMAX; }
<SHIP,GLOB>min			{ return xMIN; }
<SHIP,GLOB>speed		{ return xSPEED; }
<SHIP,GLOB>time                  { return(xTIME);     }
<SHIP,GLOB>life            	{ return xLIFE;   }
<SHIP,GLOB>var            	{ return xVAR;   }
<SHIP,GLOB>detonate		{ return(xDETONATE);      }
<EMP,GLOB>robot               { return(xROBOT);      }
<SHIP,GLOB>teleport               { return(xTELEPORT);      }
<SHIP,GLOB>turbo                 { return(xTURBO);    }
<SHIP,GLOB>lrsensor               { return(xLRS);   }
<SHIP,GLOB>srsensor               { return(xSRS);   }

<SHIP,PLA>fuel			{ return xFUEL; }
<SHIP,PLA>armies		{ return xARMIES; }
<SHIP,PLA>repair               	{ return(xREPAIR);     }

<SHIP>turns                       { return(xTURNS);          }
<SHIP>damage                 	{ return(xDAMAGE);       }
<SHIP>range                	{ return(xRANGE);       }
<SHIP>pulses                   { return(xPULSES);    }
<SHIP>hull                 { return(xHULL);       }
<SHIP>recharge                    { return(xRECHARGE);       }
<SHIP>accint                      { return(xACCEL);          }
<SHIP>decint                      { return(xDECEL);          }
<SHIP>trans                  { return(xTRANS);     }
<SHIP>reload                      { return(xRELOAD);         }
<SHIP>burst                       { return(xBURST);          }
<SHIP>hscruise                    { return(xHSCRUISE);       }
<SHIP>cruise                      { return(xCRUISE_SPD);     }
<SHIP>battle                      { return(xBATTLE_SPD);     }
<SHIP>engage                      { return(xENGAGE);         }
<SHIP>disengage                   { return(xDISENGAGE);      }
<SHIP>shotdamage                  { return(xSHOTDAMAGE);     }
<SHIP>circledist                  { return(xCIRCLEDIST);     }
<SHIP>flee                        { return(xFLEE_SPD);       }
<SHIP>cloaked                     { return(xCLOAK_SPD);      }
<SHIP>refresh                     { return(xREFRESH);        }
<SHIP>sneaky                     { return(xSNEAKY);        }
<SHIP>cost	{ return xCOST; }
<SHIP>heat	{ return xHEAT; }
<SHIP>fail	{ return xFAIL; }

<EMP>icon                        { return(xICON);           }
<EMP>name                   	{ return(xNAME);     }

<PLA>home                        { return(xHOME);           }
<PLA>"("			{ return '('; }
<PLA>")"			{ return ')'; }
<PLA>","			{ return xCOMMA; }

<GLOB>wobble                	{ return xWOBBLE;   }
<GLOB>death                	{ return xDEATH;   }
<GLOB>fire            		{ return xFIRE;   }
<GLOB>player      		{ return xPLAYER;   }
<GLOB>explode     		{ return xEXPLODE;   }
<GLOB>self			{ return xSELF;   }
<GLOB>destruct			{ return xDESTRUCT;   }
<GLOB>giveup      		{ return xGIVEUP;   }
<GLOB>orbit			{ return xORBIT;   }

<GLOB>distance			{ return xDIST;   }
<GLOB>hit-angle			{ return xHITANG;   }
<GLOB>hit-langle		{ return xHITANGL;   }
<GLOB>auto-quit			{ return xAUTOQUIT;   }

<GLOB>penalty                	{ return xPENALTY;   }
<GLOB>fast             		{ return xFAST;  }
<GLOB>disable             		{ return xDISABLE;  }
<GLOB>enable             		{ return xENABLE;  }

<CODE,PLA,EMP,SHIP,GLOB>true		{ yylval.Int = 1; return xBOOLEAN; }
<CODE,PLA,EMP,SHIP,GLOB>false		{ yylval.Int = 0; return xBOOLEAN; }


<CODE,PLA,EMP,SHIP,GLOB>[-+]?{di}+     { sscanf(yytext, "%d", &yylval.Int);
				  return(xINT);        }
<CODE,PLA,EMP,SHIP,GLOB>[-+]?{di}+"."{di}*({ex})? |
<CODE,PLA,EMP,SHIP,GLOB>[-+]?{di}*"."{di}+({ex})? |
<CODE,PLA,EMP,SHIP,GLOB>[-+]?{di}+{ex} {  sscanf (yytext, "%lf", &yylval.Real);
                                    return(xREAL);} 

<CODE,PLA,EMP,SHIP,GLOB>{le}({le}|{di})* { yylval.Str=malloc(strlen(yytext)+1);
                                  strcpy(yylval.Str, yytext);
                                  return(xSTRING);         
                                 }
<CODE,PLA,EMP,SHIP,GLOB>\"[^\"]+\"   { int len = strlen(yytext);
                                       yylval.Str = malloc(len);
                                       strcpy(yylval.Str, yytext+1);
                                       yylval.Str[len-2] = 0;
                                       return xSTRING;}

<CODE,PLA,EMP,SHIP,GLOB>[\n\t ]			;


%%
