%{
/*
 *      (c) Copyright 1991 Scott Oaks
 *      See LEGAL_NOTICE file for terms of the license.
 */

#ident  "@(#)parse.l	1.3 olvwm version 6/13/92"
%}
%%

Warp	{ yylval.ival = Warp; return WARP; }
Open	{ yylval.ival = Open; return OPEN; }
RaiseLower { yylval.ival = RaiseLower; return RAISELOWER; }
Lower	{ yylval.ival = Lower; return LOWER; }
Raise	{ yylval.ival = Raise; return RAISE; }
Execute	{ yylval.ival = Execute; return EXECUTE; }
Goto	{ yylval.ival = Goto; return GOTO; }
Close	{ yylval.ival = Close; return CLOSE; }
Quit	{ yylval.ival = Quit; return QUIT; }

Shift	{ yylval.ival = XK_Shift_L; return MODIFIER; }
"Shift Lock"	{ yylval.ival = XK_Shift_Lock; return MODIFIER; }
"Caps Lock"	{ yylval.ival = XK_Caps_Lock; return MODIFIER; }
Control	{ yylval.ival = XK_Control_L; return MODIFIER; }
Ctrl	{ yylval.ival = XK_Control_L; return MODIFIER; }
Ctl	{ yylval.ival = XK_Control_L; return MODIFIER; }
Meta	{ yylval.ival = XK_Meta_L; return MODIFIER; }
Alt	{ yylval.ival = XK_Alt_L; return MODIFIER; }
Super	{ yylval.ival = XK_Super_L; return MODIFIER; }
Hyper	{ yylval.ival = XK_Hyper_L; return MODIFIER; }
Any	{ yylval.ival = yylval.ival = -1; return MODIFIER; }
Screen	{ yylval.ival = SCREEN; return SCREEN; }
WINMENU { yylval.ival = WINMENU; return WINMENU; }
"+"	{ yylval.ival = PLUS; return PLUS; }

Help	{ yylval.sval = yytext; return KEY; }
[LRF][0-9]+	{ yylval.sval = yytext; return KEY; }

[0-9]+	{ yylval.ival = atoi(yytext); return INT; }

[A-Za-z_0-9]+	{ yylval.sval = strdup(yytext); return WORD; }

\:	{ return COLON; }
\,	{ return COMMA; }

\{	{ return OPENBRACE; }
\}	{ return CLOSEBRACE; }

\"[^"]*\"	{
	    if (yytext[yyleng - 1] == '\\')
		yymore()
	    else {
		yylval.sval = strdup(yytext);
		return WORD;
	    }
	}

\'[^']*\'	{
	    if (yytext[yyleng - 1] == '\\')
		yymore()
	    else {
		yylval.sval = strdup(yytext);
		return WORD;
	    }
	}

^#.*	;
[ \t\n]	;

%%
