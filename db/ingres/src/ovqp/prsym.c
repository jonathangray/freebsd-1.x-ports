#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <tree.h>
#include "../decomp/globs.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)prsym.c	8.3	5/30/88)

void
prsym(register sym_t *s)
{
	register union symvalue	*p;
	register int		type;
	register int		len;
	union symvalue		temp;

	type = s->type;
	len = s->len & I1MASK;
	p = &s->value;
	if (type == S_VAR) {
		/* actually, S_VAR's are rendered the same as VAR's
		 * by call_ovqp70.c's ovqpnod()
		 */
		printf("s_");	/* first part of "s_var" message */
		type = VAR;	/* execute var portion */
	}
	if (type == VAR) {
		printf("var:att#=%d:", p->sym_var.attno);
		type = p->sym_var.varfrmt;
		len = p->sym_var.varfrml;
		if (type != CHAR_CONST) {
			/* move anytype to symvalue boundary */
			bmove((char *)p->sym_var.valptr, (char *)&temp, sizeof(*p));
			p = &temp;
		}
	}
	xputchar(type);
	printf("%d:value='", len);
	switch (type) {
	  case AND:
		printf("%d [AND] (operator)", p->sym_op.opno);
		break;
	  case AOP:
		printf("%d [AOP] (operator)", p->sym_op.opno);
		break;
	  case BOP:
		printf("%d [BOP] (operator)", p->sym_op.opno);
		break;
	  case OR:
		printf("%d [OR] (operator)", p->sym_op.opno);
		break;
	  case RESDOM:
		printf("%d [RESDOM] (operator)", p->sym_op.opno);
		break;
	  case UOP:
		printf("%d [UOP] (operator)", p->sym_op.opno);
		break;
	  case COP:
		printf("%d [COP] (operator)", p->sym_op.opno);
		break;

	  case INT_CONST:
		switch (len) {
		  case 1:
			printf("%d", p->sym_data.i1type);
			break;

		  case 2:
			printf("%d", p->sym_data.i2type);
			break;

		  case 4:
			printf("%ld", p->sym_data.i4type);
		}
		break;

	  case FLOAT_CONST:
		if (len == 4)
			printf("%10.3f", p->sym_data.f4type);
		else
			printf("%10.3f", p->sym_data.f8type);
		break;

	  case RESULTID:
	  case SOURCEID:
	  case CHAR_CONST:
		printf("%p=", p->sym_data.c0type);
		prstr(p->sym_data.c0type, len);
		break;

	  case AGHEAD:
		printf("AGHEAD (delim)");
		break;
	  case BYHEAD:
		printf("BYHEAD (delim)");
		break;
	  case QLEND:
		printf("QLEND (delim)");
		break;
	  case ROOT:
		printf("ROOT (delim)");
		break;
	  case TREE:
		printf("TREE (delim)");
		break;

	  case CHANGESTRAT:
	  case REOPENRES:
	  case EXIT:
	  case QMODE:
	  case RETVAL:
	  case USERQRY:
		if (len)
			printf("%d", p->sym_op.opno);
		printf(" (status)");
		break;

	  default:
		printf("\nError in prsym: bad type= %d\n", type);
	}
	printf("'\n");
}

void
prstack(register sym_t *s)
{
	if (s->type == CHAR_CONST) {
		printf("c%d:value='%p=", s->len,s->value.sym_data.cptype);
		prstr(s->value.sym_data.cptype, s->len & I1MASK);
		printf("'\n");
	}
	else
		prsym(s);
}


void
prstr(register char *p, register int l)
{
	while (--l >= 0)
		putchar(*p++);
}
