/* ci_lex.c - lexical analyser for ups expressions */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_lex_c_sccsid[] = "@(#)ci_lex.c	1.16 20/5/92 (UKC)";

#define DEBUG

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_util.h"
#include "ci_lex.h"
#include "ci_tokens.h"

static bool Want_debugging_output;

typedef struct message_action_entryst {
	ci_message_action_t ma_action;
	const char *ma_pat;
	struct message_action_entryst *ma_next;
} message_action_entry_t;

#ifdef DEBUG
static const char *tokname PROTO((token_t token));
#endif
static const char *parse_hash_directive PROTO((const char *line, lex_env_t *le));
static lexinfo_t *make_lexinfo PROTO((lex_env_t *le, const char *line));
static const char *skip_whitespace PROTO((lex_env_t *le, const char *line));
static int get_float_constant PROTO((lex_env_t *le, const char *line,
					const char **p_end, constant_t **p_co));
static const char *getline PROTO((lex_env_t *le));
static int get_string PROTO((lex_env_t *le, const char *line, string_const_t *sc));

static message_action_entry_t *Message_action_list;

static struct {
	const char *name;
	token_t token;
	bool need_lexinfo;
} Keytab[] = {
	"auto",		AUTO,		FALSE,
	"break",	BREAK,		TRUE,
	"case",		CASE,		FALSE,
	"char",		CHAR,		FALSE,
	"const",	CONST,		FALSE,
	"continue",	CONTINUE,	TRUE,
	"default",	DEFAULT,	FALSE,
	"do",		DO,		FALSE,
	"double",	DOUBLE,		FALSE,
	"else",		ELSE,		FALSE,
	"enum",		ENUM,		FALSE,
	"extern",	EXTERN,		FALSE,
	"float",	FLOAT,		FALSE,
	"for",		FOR,		TRUE,
	"goto",		GOTO,		FALSE,
	"if",		IF,		FALSE,
	"int",		INT,		FALSE,
	"long",		LONG,		FALSE,
	"register",	REGISTER,	FALSE,
	"return",	RETURN,		TRUE,
	"short",	SHORT,		FALSE,
	"signed",	SIGNED,		FALSE,
	"sizeof",	SIZEOF,		FALSE,
	"static",	STATIC,		FALSE,
	"struct",	STRUCT,		FALSE,
	"switch",	SWITCH,		FALSE,
	"typedef",	TYPEDEF,	FALSE,
	"union",	UNION,		FALSE,
	"unsigned",	UNSIGNED,	FALSE,
	"void",		VOID,		FALSE,
	"volatile",	VOLATILE,	FALSE,
	"while",	WHILE,		FALSE,
};
#define NKEYS (sizeof Keytab / sizeof *Keytab)

lex_env_t *Lex_env;

/*  BUG: this should be in a library somewhere.
 */
char *
strstr(s, pat)
const char *s, *pat;
{
	int len;

	len = strlen(pat);
	for (; *s != '\0'; ++s)
		if (*s == *pat && memcmp(s, pat, len) ==  0)
			return (char *)s; /* UGH */
	return NULL;
}

void
yyerror(s)
const char *s;
{
	diagf(ET_ERROR, (lexinfo_t *)NULL, "%s", s);
}

void
ci_add_message_action(pat, action)
const char *pat;
ci_message_action_t action;
{
	message_action_entry_t *ma;

	ma = (message_action_entry_t *)e_malloc(sizeof(message_action_entry_t));
	ma->ma_pat = pat;
	ma->ma_action = action;
	ma->ma_next = Message_action_list;
	Message_action_list = ma;
}

void
ci_lex_error(arg, errtype, lx, mesg)
char *arg;
errtype_t errtype;
lexinfo_t *lx;
const char *mesg;
{
	lex_env_t *le;
	char *wmesg;
	bool abort_parse;

	le = (lex_env_t *)arg;

	if (le->le_abort_parse)
		return;

	if (errtype == ET_WARNING || errtype == ET_ERROR) {
		message_action_entry_t *ma;

		for (ma = Message_action_list; ma != NULL; ma = ma->ma_next)
			if (ma->ma_pat == NULL || strstr(mesg, ma->ma_pat) != NULL)
				break;
		if (ma == NULL) {
			if (errtype == ET_WARNING)
				errtype = ET_ERROR;
		}
		else {
			if (errtype == ET_ERROR) {
				if (ma->ma_pat != NULL)
					errf("Tried to ignore error \"%s\" with pattern \"%s\"",
								mesg, ma->ma_pat);
			}
			else {
				switch (ma->ma_action) {
				case MA_IGNORE:
					errtype = ET_IGNORE;
					break;
				case MA_WARNING_ONLY:
					errtype = ET_WARNING;
					break;
				case MA_DEFAULT:
					break;
				default:
					ci_panic("bad message action in le");
				}
			}
		}
	}

	if (errtype == ET_IGNORE)
		return; 

	if (errtype == ET_WARNING) {
		wmesg = strf("Warning: %s", mesg);
		mesg = wmesg;
	}
	else
		wmesg = NULL;

	if (errtype == ET_MORE)
		abort_parse = (*le->le_report_func)((char *)NULL, 0, 0, mesg);
	else {
		const char *filename;
		int lnum, cnum;

		if (lx != NULL) {
			filename = lx->lx_filename;
			lnum = lx->lx_lnum;
			cnum = lx->lx_cnum;
		}
		else {
			filename = le->le_filename;
			lnum = le->le_lnum + 1;
			cnum = le->le_lptr - le->le_line;
		}
		abort_parse = (*le->le_report_func)(filename, lnum, cnum, mesg);
	}

	if (wmesg != NULL)
		free(wmesg);

	if (errtype == ET_ERROR)
		le->le_had_error = TRUE;

	if (abort_parse) {
		le->le_abort_parse = TRUE;
		le->le_lptr = "";
	}
}

static const char *
parse_hash_directive(line, le)
const char *line;
lex_env_t *le;
{
	int lnum, nitems;
	char name[256];

	for (; isspace(*line) && *line != '\0'; ++line)
		;
	if (*line == '\0')
		return line;

	if (strncmp(line, "pragma", 6) == 0 && isspace(line[6])) {
		for (line += 7; *line != '\0' && isspace(*line); ++line)
			;
		diagf(ET_WARNING, (lexinfo_t *)NULL, "#pragma `%.*s' ignored",
							strlen(line) - 1, line);
		return line + strlen(line);
	}

	nitems = sscanf(line, "%d \"%[^\"]\"", &lnum, name);
	if (nitems < 1) {
		diagf(ET_ERROR, (lexinfo_t *)NULL,
					"Bad # directive \"%s\"", line);
		return "";
	}
	if (nitems == 2) {
		char *buf;
		int len;

		len = strlen(name);
		buf = alloc(Parse_alloc_id, len + 1);
		(void) memcpy(buf, name, len + 1);
		le->le_filename = buf;
	}

	/*  Subtract 1 because we number internally from 0,
	 *  and 1 because we are just about to bump the
	 *  line number.
	 */
	le->le_lnum = lnum - 2;

	return line + strlen(line);
}

static lexinfo_t *
make_lexinfo(le, line)
lex_env_t *le;
const char *line;
{
	lexinfo_t *lx;

	lx = NEW(lexinfo_t);
	lx->lx_filename = le->le_filename;
	lx->lx_lnum = le->le_lnum + 1;
	lx->lx_cnum = line - le->le_line;
	return lx;
}

const char *
ci_translate_escape(s, p_res)
const char *s;
int *p_res;
{
	static const char hexdigits[] = "0123456789abcdefABCDEF";
	const char *pos, *save_s;
	int ch;

	switch (*s) {
	case 'n':
		ch = '\n';
		break;
	case 't':
		ch = '\t';
		break;
	case 'v':
		ch = '\v';
		break;
	case 'b':
		ch = '\b';
		break;
	case 'r':
		ch = '\r';
		break;
	case 'f':
		ch = '\f';
		break;
	case 'a':
		ch = '\007';
		break;
	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
		ch = 0;
		for (save_s = s; isdigit(*s) && *s < '8' && s - save_s < 3; ++s)
			ch = ch * 8 + *s - '0';
		--s;
		break;
	case 'x':
		ch = 0;
		for (; *s != '\0' && (pos = strchr(hexdigits, *s)) != NULL; ++s) {
			if (pos >= hexdigits + 16)
				pos -= 6;
			ch = ch * 16 + pos - hexdigits;
		}
		break;
	default:
		ch = *s;
		break;
	}
	*p_res = ch;
	return s;
}

/*  Based on K&P's hoc follow() function.
 */
#define follow(s, ch, ifyes, ifno) ((*(s) == (ch)) ? (++(s), (ifyes)) : (ifno))

static const char *
getline(le)
lex_env_t *le;
{
	if (le->le_abort_parse)
		return NULL;

	++le->le_lnum;
	return le->le_line = (*le->le_getline)(le->le_getline_arg);
}

/*  Skip white space and comments.
 */
static const char *
skip_whitespace(le, line)
lex_env_t *le;
const char *line;
{
#ifdef DEBUG
	bool read_another_line;
#endif
	bool incomment;

	incomment = FALSE;
#ifdef DEBUG
	read_another_line = FALSE;
#endif
	for (;;) {
		for(;;) {
			while (*line != '\0' && isspace(*line))
				++line;
			if (*line != '\0')
				break;

			if ((line = getline(le)) == NULL)
				break;
#ifdef DEBUG
			read_another_line = TRUE;
#endif
			if (*line == '#')
				line = parse_hash_directive(line + 1, le);
		}
		if (incomment) {
			if (line == NULL) {
				diagf(ET_ERROR, (lexinfo_t *)NULL,
						"Hit EOF while in a comment");
				break;
			}
			else if (*line == '*' && line[1] == '/') {
				line += 2;
				incomment = FALSE;
			}
			else
				++line;
		}
		else {
			if (line != NULL && *line == '/' && line[1] == '*') {
				line += 2;
				incomment = TRUE;
			}
			else
				break;
		}
	}

#ifdef DEBUG
	if (Want_debugging_output && read_another_line) {
		putchar('\n');
		printf("\n\"%s\", %d: %s", le->le_filename, le->le_lnum, line);
	}
#endif
	return line;
}

token_t
yylex()
{
#ifdef DEBUG
	static int pos = -1;
#endif
	lex_env_t *le;
	token_t token;
	const char *line;

	le = Lex_env;

#ifdef DEBUG
	if (pos == -1) {
		Want_debugging_output = getenv("LEX_DEBUG") != NULL;
		pos = 0;
	}
#endif
	if (le == NULL) {
#ifdef DEBUG
		if (Want_debugging_output)
			puts("\n");
#endif
		return 0;
	}

	if ((line = skip_whitespace(le, le->le_lptr)) == NULL) {
		le->le_lptr = line;
		return 0;	/* EOF */
	}

	switch (*line++) {
	case '_': case '$':

	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
	case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
	case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
	case 'v': case 'w': case 'x': case 'y': case 'z': 

	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
	case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
	case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
	case 'V': case 'W': case 'X': case 'Y': case 'Z':
		{
			const char *s;
			char *buf;
			identifier_t *id;
			int len, i;

			--line;
			for (s = line; isalnum(*s) || *s == '_' || *s == '$'; ++s)
				;
			len = s - line;

			for (i = 0; i < NKEYS; ++i)
				if (memcmp(Keytab[i].name, line, len) == 0 &&
							Keytab[i].name[len] == '\0')
					break;
			if (i < NKEYS) {
				token = Keytab[i].token;
				line += len;
				if (Keytab[i].need_lexinfo)
					yylval.lexinfo = make_lexinfo(le, line);
				else
					yylval.lexinfo = NULL; /* for safety */
				break;
			}
					
			buf = alloc(Parse_alloc_id, len + 1);
			(void) memcpy(buf, line, len);
			buf[len] = '\0';

			id = NEW(identifier_t);
			id->id_name = buf;
			id->id_lexinfo = make_lexinfo(le, s);

			line = skip_whitespace(le, s);

			id->id_lparen_follows = *line == '(';
			yylval.identifier = id;

			token = name_type(buf);
		}
		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		{
			char *end;
			long val;
			
			val = strtol(line - 1, &end, 0);
			if (end == line - 1) {
				le->le_lptr = line;
				diagf(ET_ERROR, (lexinfo_t *)NULL,
					"Badly formed integer constant \"%s\"",
					line - 1);
				token = BADTOK;
			}
			else if (*end == 'e' || *end == 'E' || *end == '.') {
				token = get_float_constant(le, line-1, &line,
								&yylval.constant);
			}
			else {
				constant_t *co;

				line = end;
				if (*line == 'L')
					++line;

				co = NEW(constant_t);
				co->co_lexinfo = make_lexinfo(le, line);
				co->co_integer_val = val;
				yylval.constant = co;
				token = INTEGER_CONSTANT;
			}
		}
		break;
	case '!':
		token = follow(line, '=', NOTEQ, NOT);
		break;
	case '=':
		token = follow(line, '=', EQEQ, EQUALS);
		break;
	case '%':
		token = follow(line, '=', PERCENT_EQUALS, PERCENT);
		break;
	case '/':
		token = follow(line, '=', SLASH_EQUALS, SLASH);
		break;
	case '^':
		token = follow(line, '=', XOR_EQUALS, XOR);
		break;
	case '*':
		token = follow(line, '=', STAR_EQUALS, STAR);
		break;
	case '[':
		token = LBRAC;
		break;
	case ']':
		token = RBRAC;
		break;
	case '{':
		token = LBRACE;
		break;
	case '}':
		token = RBRACE;
		break;
	case '(':
		token = LPAREN;
		break;
	case ')':
		token = RPAREN;
		break;
	case ',':
		token = COMMA;
		break;
	case ';':
		token = SEMI;
		break;
	case '?':
		token = QUERY;
		break;
	case ':':
		token = COLON;
		break;
	case '\'': {
		/*  BUG: no escapes etc.
		 */
		int val;

		if (*line == '\\')
			line = ci_translate_escape(line + 1, &val);
		else
			val = *line;
		++line;

		if (*line != '\'') {
			le->le_lptr = line;
			diagf(ET_ERROR, (lexinfo_t *)NULL,
					"Unterminated char constant");
			token = BADTOK;
		}
		else {
			constant_t *co;

			co = NEW(constant_t);
			co->co_lexinfo = make_lexinfo(le, line);
			co->co_integer_val = val;
			yylval.constant = co;
			token = CHARACTER_CONSTANT;
			++line;
		}
		break;
	}
	case '"': {
		constant_t *co;

		co = NEW(constant_t);
		co->co_lexinfo = make_lexinfo(le, line);
		token = get_string(le, line, &co->co_string_val);
		yylval.constant = co;
		line = le->le_lptr;
		break;
	}
	case '.':
		if (*line == '.' && line[1] == '.') {
			line += 2;
			token = ELLIPSIS;
		}
		else if (isdigit(*line))
			token = get_float_constant(le, line-1, &line, &yylval.constant);
		else
			token = DOT;
		break;
	case '~':
		token = TILDE;
		break;
	case '+':
		if (*line == '+')
			token = PLUSPLUS;
		else if (*line == '=')
			token = PLUS_EQUALS;
		else {
			token = PLUS;
			--line;
		}
		++line;
		break;
	case '-':
		if (*line == '>')
			token = ARROW;
		else if (*line == '-')
			token = MINUSMINUS;
		else if (*line == '=')
			token = MINUS_EQUALS;
		else {
			token = MINUS;
			--line;
		}
		++line;
		break;
	case '|':
		if (*line == '|')
			token = OROR;
		else if (*line == '=')
			token = OR_EQUALS;
		else {
			--line;
			token = OR;
		}
		++line;
		break;
	case '&':
		if (*line == '&')
			token = ANDAND;
		else if (*line == '=')
			token = AND_EQUALS;
		else {
			--line;
			token = AND;
		}
		++line;
		break;
	case '>':
		if (*line == '>') {
			++line;
			token = follow(line, '=', RSHIFT_EQUALS, RSHIFT);
		}
		else if (*line == '=') {
			++line;
			token = GTEQ;
		}
		else
			token = GREATERTHAN;
		break;
	case '<':
		if (*line == '<') {
			++line;
			token = follow(line, '=', LSHIFT_EQUALS, LSHIFT);
		}
		else if (*line == '=') {
			++line;
			token = LESSEQ;
		}
		else
			token = LESSTHAN;
		break;
			
	default:
		le->le_lptr = line; /* because we are about to call diagf */
		diagf(ET_ERROR, (lexinfo_t *)NULL,
			"Illegal character '%c' (0x%02x)", line[-1], line[-1]);
		token = BADTOK;
		break;
	}
	le->le_lptr = line;

#ifdef DEBUG
	if (Want_debugging_output) {
		const char *name;

		if (pos > 70) {
			putchar('\n');
			pos = 0;
		}
		name = tokname(token);
		printf("%s ", name);
		pos += strlen(name) + 1;
		fflush(stdout);
	}
#endif
	return token;
}

static int
get_string(le, line, sc)
lex_env_t *le;
const char *line;
string_const_t *sc;
{
	static const char badalloc[] =
				"Unable to allocate memory for string constant";
	static char *buf;
	static int bufsize = 0;
	int opos;
	bool ok;

	if (bufsize == 0) {
		bufsize = 50;
		if ((buf = malloc(bufsize + 1)) == NULL) {
			diagf(ET_ERROR, (lexinfo_t *)NULL, "%s", badalloc);
			return BADTOK;
		}
	}

	opos = 0;
	ok = FALSE;		/* set to TRUE on success */

	for (; *line != '\0'; ++line) {
		int ch;

		if (*line == '"') {
			const char *new_line;

			new_line = skip_whitespace(le, line + 1);
			if (new_line == NULL || *new_line != '"') {
				ok = TRUE;
				le->le_lptr = new_line;
				break;
			}

			line = new_line;
			continue;
		}

		if (*line != '\\')
			ch = *line;
		else if (*++line == '\n') {
			line = getline(le);
			ch = (line != NULL) ? *line : '\0';
		}
		else
			line = ci_translate_escape(line, &ch);

		if (line == NULL || *line == '\n' || *line == '\0') {
			le->le_lptr = line;
			diagf(ET_ERROR, (lexinfo_t *)NULL,
						"Unterminated string constant");
			break;
		}

		if (opos == bufsize) {
			bufsize *= 2;
			if ((buf = realloc(buf, bufsize + 1)) == NULL) {
				le->le_lptr = line;
				diagf(ET_ERROR, (lexinfo_t *)NULL,
							"%s", badalloc);
				break;
			}
		}
		buf[opos++] = ch;
	}
	buf[opos++] = '\0';

	if (!ok)
		return BADTOK;

	sc->sc_val = memcpy(allocstr(Parse_alloc_id, opos), buf, opos);
	sc->sc_size = opos;
	return STRING_CONSTANT;
}

static int
get_float_constant(le, line, p_end, p_co)
lex_env_t *le;
const char *line, **p_end;
constant_t **p_co;
{
	double val;
	constant_t *co;
	char *end;

	val = strtod(line, &end);

	if (end == line) {
		le->le_lptr = line;
		diagf(ET_ERROR, (lexinfo_t *)NULL,
				"Badly formed floating constant \"%s\"", line);
		return BADTOK;
	}

	co = NEW(constant_t);
	co->co_lexinfo = make_lexinfo(le, line);
	co->co_floating_val = val;

	*p_co = co;
	*p_end = end;

	return FLOATING_CONSTANT;
}

#ifdef DEBUG
static const char *
tokname(token)
token_t token;
{
	static struct {
		const char *name;
		token_t token;
	} tab[] = {
		"IF",                     IF,
		"ELSE",                   ELSE,
		"WHILE",                  WHILE,
		"FOR",                    FOR,
		"DO",                     DO,
		"GOTO",                   GOTO,
		"BREAK",                  BREAK,
		"CONTINUE",               CONTINUE,
		"RETURN",                 RETURN,
		"SWITCH",                 SWITCH,
		"CASE",                   CASE,
		"DEFAULT",                DEFAULT,
		"SIZEOF",                 SIZEOF,
		"AUTO",                   AUTO,
		"REGISTER",               REGISTER,
		"STATIC",                 STATIC,
		"EXTERN",                 EXTERN,
		"TYPEDEF",                TYPEDEF,
		"VOID",                   VOID,
		"CHAR",                   CHAR,
		"SHORT",                  SHORT,
		"INT",                    INT,
		"LONG",                   LONG,
		"FLOAT",                  FLOAT,
		"DOUBLE",                 DOUBLE,
		"SIGNED",                 SIGNED,
		"UNSIGNED",               UNSIGNED,
		"CONST",                  CONST,
		"VOLATILE",               VOLATILE,
		"STRUCT",                 STRUCT,
		"UNION",                  UNION,
		"ENUM",                   ENUM,
		"AND",                    AND,
		"TILDE",                  TILDE,
		"NOT",                    NOT,
		"LESSTHAN",               LESSTHAN,
		"GREATERTHAN",            GREATERTHAN,
		"XOR",                    XOR,
		"OR",                     OR,
		"PLUS",                   PLUS,
		"MINUS",                  MINUS,
		"SLASH",                  SLASH,
		"PERCENT",                PERCENT,
		"STAR",                   STAR,
		"DOT",                    DOT,
		"COLON",                  COLON,
		"QUERY",                  QUERY,
		"SEMI",                   SEMI,
		"COMMA",                  COMMA,
		"LPAREN",                 LPAREN,
		"RPAREN",                 RPAREN,
		"LBRACE",                 LBRACE,
		"RBRACE",                 RBRACE,
		"LBRAC",                  LBRAC,
		"RBRAC",                  RBRAC,
		"EQUALS",                 EQUALS,
		"STAR_EQUALS",            STAR_EQUALS,
		"SLASH_EQUALS",           SLASH_EQUALS,
		"PERCENT_EQUALS",         PERCENT_EQUALS,
		"PLUS_EQUALS",            PLUS_EQUALS,
		"MINUS_EQUALS",           MINUS_EQUALS,
		"LSHIFT_EQUALS",          LSHIFT_EQUALS,
		"RSHIFT_EQUALS",          RSHIFT_EQUALS,
		"AND_EQUALS",             AND_EQUALS,
		"XOR_EQUALS",             XOR_EQUALS,
		"OR_EQUALS",              OR_EQUALS,
		"ANDAND",                 ANDAND,
		"OROR",                   OROR,
		"EQEQ",                   EQEQ,
		"NOTEQ",                  NOTEQ,
		"GTEQ",                   GTEQ,
		"LESSEQ",                 LESSEQ,
		"LSHIFT",                 LSHIFT,
		"RSHIFT",                 RSHIFT,
		"PLUSPLUS",               PLUSPLUS,
		"MINUSMINUS",             MINUSMINUS,
		"ARROW",                  ARROW,
		"ELLIPSIS",               ELLIPSIS,
		"STRING_CONSTANT",        STRING_CONSTANT,
		"INTEGER_CONSTANT",       INTEGER_CONSTANT,
		"CHARACTER_CONSTANT",     CHARACTER_CONSTANT,
		"FLOATING_CONSTANT",      FLOATING_CONSTANT,
		"IDENTIFIER",             IDENTIFIER,
		"TYPEDEF_NAME",           TYPEDEF_NAME,
		"BADTOK",                 BADTOK,
		"EOF",                    0,
	};
	static char buf[100];
	int i;

	for (i = 0; i < sizeof tab / sizeof *tab; ++i)
		if (tab[i].token == token)
			return tab[i].name;

	(void) sprintf(buf, "<unknown token %d>", token);
	return buf;
}
#endif /* DEBUG */
