/* cx.c - standalone driver for the ups C interpreter */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_cx_c_sccsid[] = "@(#)cx.c	1.13 12/9/92 (UKC)";

#include <stdio.h>
#include <string.h>

#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "cx_builtins.h"

int main PROTO((int argc, const char **argv));
static const char *getline PROTO((char *arg));
static bool report_error PROTO((const char *filename, int lnum, int cnum,
								const char *mesg));

static int readdata PROTO((taddr_t addr, char *buf, int nbytes));
static int writedata PROTO((taddr_t addr, const char *buf, int nbytes));
static parse_id_t parse PROTO((const char *name, parse_id_t parse_id,
					const char *cppflags,
					block_t *block, unsigned long parse_flags));

FILE *popen PROTO((const char *name, const char *mode));
int pclose PROTO((FILE *fp));

extern int errno;

static bool
report_error(filename, lnum, cnum, mesg)
const char *filename;
int lnum, cnum;
const char *mesg;
{
	if (filename != NULL)
		fprintf(stderr, "%s, %d: %s\n", filename, lnum, mesg);
	else
		fprintf(stderr, "\t%s\n", mesg);
	fflush(stderr);
	return FALSE;
}

static int
writedata(addr, buf, nbytes)
taddr_t addr;
const char *buf;
int nbytes;
{
	memcpy((char *)addr, buf, nbytes);
	return 0;
}

static int
readdata(addr, buf, nbytes)
taddr_t addr;
char *buf;
int nbytes;
{
	memcpy(buf, (char *)addr, nbytes);
	return 0;
}

int
main(argc, argv)
int argc;
const char **argv;
{
	static const char *default_args[] = { "a.out", NULL };
	char *cppflags;
	char ap;
	long bptloc;
	int nargs;
	alloc_id_t alloc_id;
	const char **endargv, **args;
	int extvar, apvar, fpvar;
	parse_id_t parse_id;
	block_t *block;
	bool want_disassembly_only, wantblock, oldc;
	unsigned long compile_flags, parse_flags;
	char fp;
	code_id_t code_id;

	++argv;

	parse_flags = compile_flags = 0;
	want_disassembly_only = FALSE;
	oldc = FALSE;
	wantblock = FALSE;
	bptloc = 0;
	cppflags = strsave("-U__GNUC__ -D__CXC__");

	while (*argv != NULL && **argv == '-') {
		const char *flag;

		flag = *argv++ + 1;
		if (*flag == 'D' || *flag == 'I' || *flag == 'U') {
			char *new;

			new = strf("%s -%s", cppflags, flag);
			free(cppflags);
			cppflags = new;
		}
		else if (strcmp(flag, "ignore") == 0) {
			if (*argv == NULL) {
				errf("Missing string after -ignore");
				exit(1);
			}
			ci_add_message_action(*argv++, MA_IGNORE);
		}
		else if (strcmp(flag, "wantblock") == 0)
			wantblock = TRUE;
		else if (strcmp(flag, "ignoreall") == 0)
			ci_add_message_action((char *)NULL, MA_IGNORE);
		else if (strcmp(flag, "warning") == 0) {
			if (*argv == NULL) {
				errf("Missing string after -warning");
				exit(1);
			}
			ci_add_message_action(*argv++, MA_WARNING_ONLY);

		}
		else if (strcmp(flag, "warningall") == 0)
			ci_add_message_action((char *)NULL, MA_WARNING_ONLY);
		else if (strcmp(flag, "oldc") == 0)
			oldc = TRUE;
		else if (strcmp(flag, "S") == 0)
			want_disassembly_only = TRUE;
		else if (strcmp(flag, "checksp") == 0)
			compile_flags |= CI_CP_CHECKSP;
		else if (strcmp(flag, "functrace") == 0)
			compile_flags |= CI_CP_FUNCTRACE;
		else if (strcmp(flag, "dont_panic") == 0) {
			compile_flags |= CI_CP_DONT_PANIC;
			parse_flags |= CI_DONT_PANIC;
		}
		else if (strcmp(flag, "args") == 0) {
			--argv;
			break;
		}
		else if (strcmp(flag, "bpt") == 0) {
			if (*argv == NULL) {
				errf("Missing number after -bpt");
				exit(1);
			}
			if (bptloc != 0) {
				errf("Only one -bpt flag allowed");
				exit(1);
			}
			bptloc = atoi(*argv++);
		}
		else {
			errf("Unknown flag -%s", flag);
			exit(1);
		}
	}

	if (oldc) {
		char *new;

		ci_add_message_action("Call via old style function expression",
									MA_IGNORE);
		ci_add_message_action("No prototype in scope", MA_IGNORE);
		ci_add_message_action("Implicit declaration of", MA_IGNORE);

		parse_flags |= CI_MAKE_EXTERNS_TOPLEVEL;

		new = strf("%s -traditional", cppflags);
		free(cppflags);
		cppflags = new;
	}

	for (endargv = argv; *endargv != NULL; ++endargv)
		if (strcmp(*endargv, "-args") == 0)
			break;

	args = (*endargv != NULL) ? endargv + 1 : default_args;
	for (nargs = 0; args[nargs] != NULL; ++nargs)
		;

	alloc_id = alloc_create_pool();
	if (wantblock) {
		var_t *v;
		type_t *vtype;

		block = ci_make_block(alloc_id, (block_t *)NULL);

		vtype = ci_code_to_type(TY_INT);
		v = ci_make_var(alloc_id, "extvar", CL_EXT, vtype, (taddr_t)&extvar);
		block->bl_vars = v;
		v = ci_make_var(alloc_id, "fpvar", CL_AUTO, vtype,
						(taddr_t)((char *)&fpvar - &fp));
		block->bl_vars->va_next = v;
		v = ci_make_var(alloc_id, "apvar", CL_ARG, vtype,
						(taddr_t)((char *)&apvar - &ap));
		block->bl_vars->va_next->va_next = v;

		block->bl_vars->va_flags &= ~VA_IS_CI_VAR;
	}
	else {
		block = NULL;
	}

	parse_id = NULL;
	if (argv == endargv) {
		parse_id = ci_parse_file(parse_id, "stdin", block, parse_flags,
					 report_error,
					 (ci_resolve_name_func_t)NULL,
					 getline, (char *)stdin);
		if (parse_id == NULL) {
			errf("Parse of stdin failed");
			exit(1);
		}
	}
	else {
		bool many;

		many = &argv[1] != endargv;
		for (; argv != endargv; ++argv) {
			if (many) {
				fprintf(stderr, "%s:\n", *argv);
				fflush(stderr);
			}
			parse_id = parse(*argv, parse_id, cppflags,
								block, parse_flags);
			if (parse_id == NULL) {
				errf("Parse of %s failed", *argv);
				exit(1);
			}
		}
	}


	if ((code_id = ci_compile_program(parse_id, report_error, cx_getaddr,
					  (ci_checkarg_proc_t)NULL,
					  (ci_regno_to_addr_proc_t)NULL,
					  (ci_get_regaddr_proc_t)NULL,
					  (char *)NULL,
					  compile_flags,
					  nargs, args)) == NULL) {
		errf("Compilation failed");
		exit(1);
	}
	else if (want_disassembly_only)
		ci_disassemble(parse_id, code_id);
	else {
		ci_opcode_t opcode;

		opcode = 0;	/* to satisfy gcc */
		if (bptloc != 0)
			opcode = ci_install_trap_instruction(code_id, bptloc);

		extvar = 8;
		ci_execute_code(code_id, (taddr_t)&fp, (taddr_t)&ap,
				readdata, writedata,
				(ci_indirect_call_proc_t)NULL);
		
		errf("Hit trap instruction");

		if (bptloc != 0) {
			ci_uninstall_trap_instruction(code_id, bptloc, opcode);
			ci_execute_code(code_id, (taddr_t)&fp, (taddr_t)&ap,
						readdata, writedata,
						(ci_indirect_call_proc_t)NULL);
		}
	}

	exit(0);
	return 0;	/* to satisfy gcc */
}

static parse_id_t
parse(name, parse_id, cppflags, block, parse_flags)
const char *name;
parse_id_t parse_id;
const char *cppflags;
block_t *block;
unsigned long parse_flags;
{
	FILE *fp;
	int len;

	len = strlen(name);
	if ((fp = fopen(name, "r")) == NULL) {
		errf("Can't open %s (%m)", name);
		parse_id = NULL;
	}
	else if (len > 2 && strcmp(name + len - 2, ".c") == 0) {
		char *cmd;

		fclose(fp);
		cmd = strf("gcc -E %s %s", cppflags, name);
		errno = 0;
		if ((fp = popen(cmd, "r")) == NULL) {
			errf("Can't run \"%s\" (%m)", cmd);
			exit(1);
		}
		else {
			parse_id = ci_parse_file(parse_id, name, block,
						 parse_flags, report_error,
						 (ci_resolve_name_func_t)NULL,
						 getline, (char *)fp);
			pclose(fp);
		}
		free(cmd);
	}
	else {
		parse_id = ci_parse_file(parse_id, name, block, parse_flags,
					 report_error,
					 (ci_resolve_name_func_t)NULL,
					 getline, (char *)fp);
		fclose(fp);
	}

	return parse_id;
}

static const char *
getline(arg)
char *arg;
{
	return fpgetline((FILE *)arg);
}
