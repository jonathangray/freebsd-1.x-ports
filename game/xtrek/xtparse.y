/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in 
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no 
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO 
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

%{

#include <X11/Xlib.h>
#include "defs.h"
#include "data.h"

     /* Variables global to parser. */

#define NUL  '\0'


char          *xtrekFile;
static struct planet *pp;
extern struct planet *findplanet();
extern char *yytext; 
static aShip  *cs = 0;
static int curemp = 0;
  
%}

%union 
  {
    int          Int;
    int          *IntP;
    char         Char;
    char        *Str;
    double       Real;
  }

/* Lex tokens */

%start xtrekFile


%token   xDEFAULT xEMPIRE xSHIP xPLANET 

%token  xRANGE  xPULSES xDAMAGE xMIN xMAX xFIRE xVAR xLIFE xWOBBLE
%token  xSPEED     xSPEED    xTURNS xWARP
%token  xHULL          xCOST   xCLOAK  xPHASER xTORP
%token    xFAIL  xMINE xSRS xLRS
%token   xRECHARGE  xDETONATE xTELEPORT xLOCK xCOOL xHEAT xTRANS
%token  xACCEL          xDECEL        xWEAPON xENGINE
%token  xENGINE   xSHIELD xRELOAD  xBURST        xCRUISE_SPD  xHSCRUISE
%token  xFLEE_SPD     xCLOAK_SPD   xBATTLE_SPD 
%token  xENGAGE		xDISENGAGE    xSHOTDAMAGE  xCIRCLEDIST
%token  xREFRESH        xICON           xROBOT xNAME   xTURBO xTIME 

%token  xDEATH 		xPLAYER 	xEXPLODE
%token  xSELF 		xDESTRUCT 	xGIVEUP xORBIT xDIST xPFIREDIST xHITANG
%token  xHITANGL 	xAUTOQUIT 	xPENALTY xFAST

%token   xHOME xFUEL xREPAIR xARMIES xGLOB  xDISABLE xENABLE

%token   <Int> xINT  xBOOLEAN
%token   <Real> xREAL 
%token   <Str> xSTRING 

%token   xEQ xSEMI xCOMMA xSNEAKY 

%type <Int>  system 


%%

xtrekFile : commands 
          ;

commands : command 
         | commands command
         ;

command     : empire
            | ship
            | planet
            | error
	    | global
            ;


/* 
 *  Empire commands
 */

empire : empireName empireOpts xSEMI
	| empireName xSEMI
       ;

empireName	: xEMPIRE xSTRING 
         	{
                  curemp = addempire($2, NULL);
         	}
       		| xEMPIRE xSTRING xSTRING 
         	{
                  curemp = addempire($2, $3);
         	}
		;

empireOpts 	: empireOpt
		| empireOpts empireOpt
		;	


empireOpt	: xICON xEQ xSTRING 
		{ 
                  strcpy(empires[curemp].iconname, $3); 
		} 
		| xROBOT xNAME xEQ xSTRING
		{
		  strcpy(empires[curemp].robotname, $4);
		}
	        ;


/*
 *  Global commands
 */

global : xGLOB globOpts xSEMI
       ;
       
/*
 *   Ship commands.
 */

ship : shipName shipOpts xSEMI
     ;

shipName : xSHIP xSTRING 
           {
	     cs = findship($2);
             if (!cs) {
                 fprintf (stderr, "empire %s not found\n", $2);
                 exit(1);
               }
           }
	 | xDEFAULT xSHIP
           {
	     cs = findship(NULL);
             if (!cs) {
                 fprintf (stderr, "default ship sanity check failed\n");
                 exit(1);
               }
           }
         ;

shipOpts : shipOpt
         | shipOpts shipOpt
         ;

shipOpt :   
	  xTORP	xRANGE	xEQ xINT { cs->torpdrange	= $4; }
	| xMINE	xRANGE 	xEQ xINT { cs->minedrange	= $4; }
	| xPHASER xRANGE	xEQ xINT { cs->phaserdist	= $4; }
	| xPHASER	xPULSES	xEQ xINT { cs->ph_pulses	= $4; }
	| xMAX 		xDAMAGE	xEQ xINT { cs->maxdamage	= $4; }
	| xSHIELD	xMAX 	xEQ xINT { cs->maxshields         = $4; }
	| xTORP 	xSPEED	xEQ xINT { cs->torpspeed     = $4; }
	| xMAX 		xSPEED	xEQ xINT { cs->maxspeed      = $4; }
	| xSHIELD 	xREPAIR	xEQ xINT { cs->repair        = $4; }
	| xHULL 	xREPAIR	xEQ xINT { cs->repair        = $4; }
	| xMAX 		xFUEL	xEQ xINT { cs->maxfuel       = $4; }
	| xTURBO 	xSPEED	xEQ xINT { cs->turbospeed         = $4; }
	| xTURBO 	xTIME	xEQ xINT { cs->turbotime          = $4; }
	| xCLOAK	xMIN		xEQ xINT { cs->mincloak       = $4; }
	| xCLOAK	xMAX		xEQ xINT { cs->maxcloak       = $4; }
	| xMAX 		xARMIES	xEQ xINT { cs->maxarmies          = $4; }
	| xWEAPON xCOOL 	xEQ xINT { cs->wcool              = $4; }
	| xENGINE xCOOL 	xEQ xINT { cs->ecool              = $4; }
	| xTELEPORT	xRANGE	xEQ xINT { cs->telrange           = $4; }
	| xPHASER	xDAMAGE	xEQ xINT { cs->phaserdamage  = $4; }
	| xTORP		xDAMAGE	xEQ xINT { cs->torpdamage  = $4; }
	| xMINE		xDAMAGE	xEQ xINT { cs->minedamage  = $4; }
	| xTURNS          xEQ xINT { cs->turns         = $3; }   
	| xRECHARGE       xEQ xINT { cs->recharge           = $3; }
	| xACCEL          xEQ xINT { cs->accint             = $3; }
	| xDECEL          xEQ xINT { cs->decint             = $3; }
	| xRELOAD         xEQ xINT { cs->reload        = $3; }
	| xBURST          xEQ xINT { cs->burst         = $3; }
	| xCRUISE_SPD     xEQ xINT { cs->cruise        = $3; }
	| xHSCRUISE       xEQ xINT { cs->hscruise      = $3; }
	| xBATTLE_SPD     xEQ xINT { cs->battle        = $3; }
	| xFLEE_SPD       xEQ xINT { cs->flee          = $3; }
	| xCLOAK_SPD      xEQ xINT { cs->cloaked       = $3; }
	| xENGAGE	  xEQ xINT { cs->engage        = $3; }
	| xDISENGAGE      xEQ xINT { cs->disengage     = $3; }
	| xSHOTDAMAGE     xEQ xINT { cs->shotdamage    = $3; }
	| xCIRCLEDIST     xEQ xINT { cs->circledist    = $3; }
	| xREFRESH        xEQ xINT { cs->refresh       = $3; }
	| xSNEAKY        xEQ xINT { cs->sneaky       = $3; }


	| system xFAIL xEQ xINT { cs->fail[$1] = $4; }
	| system xCOST xEQ xINT { cs->cost[$1] = $4; }
	| system xHEAT xEQ xINT { cs->heat[$1] = $4; }
	;


system	: xPHASER { $$ =   (int) Phaser; }
	| xTORP	{ $$ =  (int) Torp; }
	| xMINE	{ $$ =  (int) Mine; }
	| xTRANS { $$ =  (int) Trans; }
	| xSHIELD { $$ =  (int) Shield; }
	| xCLOAK { $$ =  (int) Cloak; }
	| xSRS { $$ =  (int) SRS; }
	| xLRS { $$ =  (int) LRS; }
	| xDETONATE { $$ =  (int) Detonate; }
	| xTELEPORT {$$ =  (int) Teleport; }
	| xWARP { $$ =  (int) Warp; }
	| xLOCK { $$ =  (int) Lock; }
	| xCOOL { $$ =  (int) Cooling; }
	;

/*
 *  Planet stuff.
 */

planet : planetName planetOpts xSEMI
       ;

planetName : xPLANET xSTRING xSTRING
	     {
               addplanet($2, $3);
               pp = findplanet($2);
               if (pp == NULL)
                 {
                   fprintf (stderr, "planet %s not found\n", $2);
                   exit(1);
                 }
	     }
           | xDEFAULT xPLANET
             {
                pp = findplanet(NULL);
                if (pp == NULL)
                  {
                    fprintf (stderr, "default planet not found\n");
                    exit(1);
                  }
	      }
	    ;

planetOpts : planetOpt
           | planetOpts planetOpt
           ;

        
planetOpt :	   xHOME                { pp->pl_flags |= PLHOME; } 
		|  xREPAIR              { pp->pl_flags |= PLREPAIR; } 
		|  xFUEL                { pp->pl_flags |= PLFUEL; } 
		|  '(' xINT xCOMMA xINT ')'  { pp->pl_x = $2; pp->pl_y = $4;} 
		|  xARMIES xEQ  xINT { pp->pl_armies = $3;  } 
		;

globOpts : globOpt
         | globOpts globOpt
         ;

globOpt  : xDEATH xTIME xEQ xINT  { DEATHTIME = $4; }
	| xTORP		xLIFE xMIN xEQ xINT  { TFIREMIN = $5; }
	| xTORP		xLIFE xVAR xEQ xINT  { TFIREVAR = $5; }
	| xMINE		xLIFE xMIN xEQ xINT  { MFIREMIN = $5; }
	| xMINE		xLIFE xVAR xEQ xINT  { MFIREVAR = $5; }
	| xWEAPON	xLOCK xMIN	xEQ xINT  { PWEAPLOCKMIN = $5; }
	| xWEAPON	xLOCK xVAR	xEQ xINT  { PWEAPLOCKVAR = $5; }
	| xENGINE	xLOCK xMIN	xEQ xINT  { PENGLOCKMIN = $5; }
	| xENGINE	xLOCK xVAR	xEQ xINT  { PENGLOCKVAR = $5; }
	| xPHASER	xHITANG		xEQ xINT  { PHITANG = $4; }
	| xPHASER	xHITANGL	xEQ xINT  { PHITANGL = $4; }
	| xDETONATE	xDIST 		xEQ xINT  { DETDIST = $4; }
	| xORBIT 	xSPEED 		xEQ xINT  { ORBSPEED = $4; }
	| xORBIT	xDIST		xEQ xINT  { ORBDIST = $4; }
	| xPHASER 	xFIRE 	  xTIME xEQ xINT  { PFIRETIME = $5; }
	| xPLAYER 	xEXPLODE  xTIME xEQ xINT  { PEXPTIME = $5; }
	| xSELF 	xDESTRUCT xTIME	xEQ xINT  { PSELFDESTTIME = $5; }
	| xROBOT 	xGIVEUP   xTIME	xEQ xINT  { RGIVEUPTIME = $5; }
	| xPLANET xFIRE xDIST xEQ xINT  { PFIREDIST = $5; }
	| xAUTOQUIT xEQ xINT  { AUTOQUIT = $3; }

	| xCOOL xPENALTY xEQ xREAL  { g_coolpenalty = $4; }
	| xFAST xDESTRUCT { g_fastdestruct = 1; }
	| xMINE xDETONATE { g_minedetonate = 1; }
	| xMINE xWOBBLE xEQ xINT { MINE_WOBBLE = $4; }
	| xDISABLE xDESTRUCT { g_selfdestruct = 0; }
	| xDISABLE xTURBO { g_turbo = 0; }
	| xDISABLE xTELEPORT { g_teleport = 0; }
	| xDISABLE xMINE { g_mine = 0; }
	| xDISABLE xCLOAK { g_cloak = 0; }
	| xENABLE xDESTRUCT { g_selfdestruct = 1; }
	| xENABLE xTURBO { g_turbo = 1; }
	| xENABLE xTELEPORT { g_teleport = 1; }
	| xENABLE xMINE { g_mine = 1; }
	| xENABLE xCLOAK { g_cloak = 1; }
		;


%%

#include  "xttoken.c"

/**************************************************************************** 
*
* yyerror()
*
****************************************************************************/

yyerror(str)

  char  *str;

  {

    fflush (stdout);
    fprintf (stderr, "%s: %s near %s\n", 
             xtrekFile, str, yytext);
    fflush (stderr);

    yyClearError();
  }

/**************************************************************************** 
 *
 * yyClearError()
 *
 ****************************************************************************/

yyClearError()

  {
    char  ch;

    ch = input();
    while ( (ch != NUL) && (ch != ';') )
        ch = input();

    if (ch == ';')
      {
        fflush(stdout);
        fprintf (stderr, "  -- ignoring command.\n");
        fflush(stderr);
      }
    else
      {
        fflush(stdout);
        fprintf (stderr, "  AARRGHH -- Could not recover from errors.\n");
        fflush(stderr);
        exit(1);
      }
/*
    yyclearin;
    yyerrok;
*/
  }

