/*
 * Copyright 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#include <X11/Xfuncs.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Porthole.h>
#include <X11/Xaw/Tree.h>
#include <X11/Xaw/Command.h>

#include <sys/stat.h>
#include <stdio.h>

extern char *getenv();
extern char *malloc(), *realloc();

#include "Confirmer.h"
#include "Hyper.h"
#include "util.h"
#include "axinfo.h"
#include "version.h"

#define INFOFILE "axe.info"

#if __STDC__
#define MAKESTRING(name,string) \
    name##String = XtNewString(string); \
    name##Len = strlen(name##String);
#else
#define MAKESTRING(name,string) \
    name/**/String = XtNewString(string); \
    name/**/Len = strlen(name/**/String);
#endif

#define BCOPY(outof, into, upto) \
    into = malloc((unsigned) (upto - outof + 1)); \
    bcopy(outof, into, upto - outof); \
    into[upto - outof] = 0;

#define TABLECHUNK 20 

typedef struct {
    String tag;
    int    pos;
} TagTableRec, *TagTablePtr;

typedef struct {
    String  version;
    Boolean tree;
    String  node;
    String  dir;
} AppResRec, *AppResRecPtr;

Widget top, hyper = 0, links, tree = 0, root, porthole, lastNode = 0;
char soh = '\001', eoh = '\004';
XContext context;
String infofile, basedir, currentfile = 0, buffer;
int baselen, buflen;
TagTablePtr tagTable;
int maxTags = 0, numberTags = 0, currentTag = -1, lastTag = -1;
TagTablePtr indirectTable = 0;
int maxIndirects = 0, numberIndirects = 0;
int dragX, dragY;

String fileString, nodeString, prevString, nextString, upString;
String menuString, entryString, xrefString, btagString, etagString; 
String itagString, tagiString;
int fileLen, nodeLen, prevLen, nextLen, upLen;
int menuLen, entryLen, xrefLen, btagLen, etagLen;
int itagLen, tagiLen;

Atom
AxinfoUserAtom(display)
     Display *display;
{
    char propName[1024], *user;

    strcpy(propName, "AXE_AXINFO");

    if ( (user = getenv("USER")) )
    {
        strcat(propName, "_");
        strcat(propName, user);
    }

    return XInternAtom(display, propName, False);
}

Boolean
IsTag(tag, tagId)
     char *tag;
     int *tagId;
{
    int t;

    for (t = 0;  t < numberTags;  ++t)
    {
	if (strcmp(tag, tagTable[t].tag) == 0)
	{
	    *tagId = t;
	    return True;
	}
    }

    return False;
}

char*
Find(buffer, magic)
     char *buffer, magic;
{
    char *bp;

    for (bp = buffer;  *bp && *bp != magic;  ++bp) {}

    return *bp ? bp : 0;
}

char*
Locate(buffer, search)
     char *buffer, *search;
{
    char *bp;
    int i, len = strlen(search);

    for (bp = buffer;  *bp && *bp != INFO_COOKIE;  ++bp)
    {
	for (i = 0;  i < len;  ++i)
	{
	    if (bp[i] != search[i])
	    {
		break;
	    }
	}

	if (search[i] == 0)
	{
	    return bp + len;
	}
    }

    return bp;
}

char*
ReverseLocate(buffer, search)
     char *buffer, *search;
{
    char *bp, *rsearch;
    int i, j, len = strlen(search);

    rsearch = malloc((unsigned) (len + 1));
    for (i = 0, j = len - 1;  i < len;  ++i, --j)
    {
	rsearch[j] = search[i];
    }
    rsearch[len] = 0;

    for (bp = buffer;  *bp;  --bp)
    {
	for (i = 0;  i < len;  ++i)
	{
	    if (bp[-i] != rsearch[i])
	    {
		break;
	    }
	}

	if (search[i] == 0)
	{
	    free(rsearch);
	    return bp + 1;
	}
    }

    free(rsearch);
    return bp;
}

int
LookingAt(bp, nbp)
     char *bp, **nbp;
{
    if (*bp == INFO_COOKIE)
    {
	++bp;
	while (*bp == '\n' || *bp == INFO_FF)
	{
	    ++bp;
	}
    }

    switch (*bp)
    {
    case 'F':
	if (strncmp(bp, fileString, fileLen) == 0)
	{
	    *nbp = bp + fileLen;
	    return IS_NODE;
	}
	break;
    case 'T':
	if (strncmp(bp, btagString, btagLen) == 0)
	{
	    *nbp = bp + btagLen;
	    return IS_TAG_TABLE;
	}
	break;
    case 'I':
	if (strncmp(bp, itagString, itagLen) == 0)
	{
	    *nbp = bp + itagLen;
	    return IS_INDIRECT;
	}
	break;
    case '(':
	if (strncmp(bp, tagiString, tagiLen) == 0)
	{
	    *nbp = bp + tagiLen;
	    return IS_INDIRECT_LABEL;
	}
	break;
    case '\n':
	if (strncmp(bp, menuString, menuLen) == 0)
	{
	    *nbp = bp + menuLen;
	    return IS_MENU;
	}
	else if (strncmp(bp, entryString, entryLen) == 0)
	{
	    *nbp = bp + entryLen;
	    return IS_ENTRY;
	}
	else if (bp[1] == '*' || bp[1] == '=' || bp[1] == '-' || bp[1] == '.')
	{
	    *nbp = bp + 2;
	    return IS_SECTIONING;
	}
	break;
    case 'N':
	if (strncmp(bp, nodeString, nodeLen) == 0)
	{
	    *nbp = bp + nodeLen;
	    return IS_TAG;
	}
	else if (strncmp(bp, nextString, nextLen) == 0)
	{
	    *nbp = bp + nextLen;
	    return IS_NEXT;
	}
	break;
    case 'P':
	if (strncmp(bp, prevString, prevLen) == 0)
	{
	    *nbp = bp + prevLen;
	    return IS_PREV;
	}
	break;
    case 'U':
	if (strncmp(bp, upString, upLen) == 0)
	{
	    *nbp = bp + upLen;
	    return IS_UP;
	}
	break;
    case '*':
	if (strncmp(bp, xrefString, xrefLen) == 0)
	{
	    *nbp = bp + xrefLen;
	    return IS_XREF;
	}
	break;
    default:
	return 0;
    }

    return 0;
}

/*ARGSUSED*/
void
Exit(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    exit(1);
}

void
OpenInfofile(file)
     char *file;
{
    FILE *inf;
    struct stat fstats;
    
    if (currentfile)
    {
	if (file != currentfile)
	{
	    free(buffer);
	    currentfile = 0;
	}
	else
	{
	    return;
	}
    }

    inf = fopen(file, "r");
    if (!inf)
    {
	Widget conf;
	char message[1024];

	conf = XtVaCreateManagedWidget("confirmer", confirmerWidgetClass, top,
				       XtNgrabOnPopup, True,
				       NULL);
	XtAddCallback(conf, XtNpopdownCallback, Exit, (XtPointer) 0);

	sprintf(message, "Unable to open\n%s", file);
	ConfirmerRequestConfirmation(conf,  message,
				     "acknowledge", ConfirmerPopdown,
				     "unused", NULL,
				     (XtPointer) 0);

	XtAppMainLoop(XtWidgetToApplicationContext(conf));
    };

    if (fstat(fileno(inf), &fstats) == -1)
    {
	perror("fstat");
	exit(1);
    }

    if (!(buffer = malloc((unsigned) (fstats.st_size + 1))))
    {
	printf("Out of memory\n");
	exit(1);
    }

    if (fread(buffer, (int) fstats.st_size, 1, inf) != 1)
    {
	printf("Read error\n");
	exit(1);
    }

    buffer[fstats.st_size] = 0;
    buflen = fstats.st_size;
    currentfile = file;

    fclose(inf);
}

char*
MakeHeaderBuffer(bp)
     char *bp;
{
    char *sp = bp, *header;

    while (*bp != '\n') ++bp;

    /* +15 to be absoultely safe on length = "{}   " for Next, Prev and Up */
    header = XtMalloc((unsigned) (bp - sp + 15 + 32));

    /* Assumes this takes up less space than "File: xyz.intro" */
    sprintf(header, "Top: %cTop%c   ", soh, eoh);

    return header;
}

void
CompleteHeaderBuffer(header)
     char *header;
{
    if (lastTag >= 0)
    {
	sprintf(header, "Last: %c%s%c", soh, tagTable[lastTag].tag, eoh);
    }
}

void
HyperLink(type, bp, header, eol)
     int type;
     char *bp, **header, **eol;
{
    char *sp, *head = *header;
    int len;

    switch (type)
    {
    case IS_NEXT:
	strcat(head, nextString);
	break;
    case IS_PREV:
	strcat(head, prevString);
	break;
    case IS_UP:
	strcat(head, upString);
	break;
    }

    sp = bp;
    while (*bp == ' ')
    {
	++bp;
    }
    strncat(head, sp, bp - sp);
    sprintf(&head[strlen(head)], "%c", soh);

    sp = bp;
    while (*bp != ',' && *bp != '\n')
    {
	++bp;
    }
    strncat(head, sp, bp - sp);
    sprintf(&head[strlen(head)], "%c   ", eoh);
    
    if (*bp == '\n')
    {
	*eol = bp;
	/* Blank out back to comma terminating "Node: Tag," */
	for (--bp;  *bp != ',';  --bp)
	{
	    *bp = ' ';
	}
	*bp = ' ';
    }
    else
    {
	*bp =' '; /* overwrites ',' */
	*eol = 0;
    }
}

void
Entry(bp)
     char *bp;
{
    char *sp;

    sp = bp;
    bp = Find(bp, ':');
    if (bp[1] == ':')
    {
	bp[1] = eoh;
	for (  ;  bp > sp;  --bp)
	{
	    bp[0] = bp[-1];
	}
	sp[0] = soh;
    }
    else
    {
	++bp;
	while (*bp == ' ')
	{
	    ++bp;
	}
	bp[-1] = soh;
	while (*bp != '\t' && *bp != ',' && *bp != '.' && *bp != '\n')
	{
	    ++bp;
	}
	bp[0] = eoh;
    }
}

void
CrossRef(bp)
     char *bp;
{
    char *sp;

    while (*bp == ' ' || *bp == '\n' || *bp == '\t')
    {
	++bp;
    }
    
    sp = bp - 1;
    bp = Find(bp, ':');
    if (bp[1] == ':')
    {
	/*
	 * This is a "*Note node-name::" case.
	 * Convert it into "*Note <SOH>Node name<EOH>", i.e. sacrifice
	 * the two ':' characters to make way for the two markers.
	 */
	bp[1] = eoh;
	for (  ;  bp > sp;  --bp)
	{
	    bp[0] = bp[-1];
	}
	sp[1] = soh;
    }
    else
    {
	/*
	 * "*Note cross-reference-name: node-name[,.]" or
	 * "*Note cross-reference-name: (info-file)node-name[.,]" cases.
	 *
	 * Convert the former to
	 * "*Note cross-reference-name:<SOH>node-name<EOH>", i.e. sacrifice
	 * leading space and terminating period or comma,
	 *
	 * and the latter to 
	 * "*Note cross-reference-name:(info-file)<SOH>node-name<EOH>", i.e.
	 * shift "(info-file)" down 1 to release space in front of node
	 * name and sacrifice terminating period or comma.
	 */
	bp = bp + 1;
	while (*bp == ' ' || *bp == '\n')
	{
	    ++bp;
	}
	sp = bp - 1;

	if (*bp == '(')
	{
	    bp = Find(bp, ')');
	    for (  ;  sp < bp;  ++sp)
	    {
		sp[0] = sp[1];
	    }
	    sp = bp;
	}
	sp[0] = soh;
	
	while (*bp != '.' && *bp != ',')
	{
	    ++bp;
	}
	*bp = eoh;
    }
}

char*
ExtractNodeAndHyperize(buffer, header, body)
     char *buffer, **header, **body;
{
    char c, *sp, *bp = buffer, *node, *headerBuffer, *eol;
    int type;

    while (*bp && *bp != INFO_COOKIE)
    {
	++bp;
    }
    BCOPY(buffer, node, bp)

    bp = node;
    *header = 0;
    *body = 0;
    while (*bp)
    {
	sp = bp;
	switch ( (type = LookingAt(bp, &bp)) )
	{
	case IS_NODE:
	    headerBuffer = MakeHeaderBuffer(sp);
	    break;
	case IS_NEXT:
	case IS_PREV:
	case IS_UP:
	    if (!(*body)) /* i.e. finished with header, and hence allows */ 
	    {             /* possibility of "Up:" in text, for instance  */
		HyperLink(type, bp, &headerBuffer, &eol);
		if (eol)
		{
		    CompleteHeaderBuffer(&headerBuffer[strlen(headerBuffer)]);
		    *header = headerBuffer;
		    *body = node;
		}
	    }
	    break;
	case IS_ENTRY:
	    Entry(bp);
	    break;
	case IS_XREF:
	    CrossRef(bp);
	    break;
	default:
	    ++bp;
	    break;
	}
    }

    return node;
}

char*
MakeNode(nodename, header, body)
     char *nodename, **header, **body;
{
    char *bp, *sp, *tp, *node;
    int i, p;
    String f = infofile;

    if (!IsTag(nodename, &p))
    {
	return 0;
    }
	
    if (indirectTable)
    {
	for (i = numberIndirects - 1;  i >= 0;  --i)
	{
	    if (tagTable[p].pos >= indirectTable[i].pos)
	    {
		f = indirectTable[i].tag;
		break;
	    }
	}
    }

    OpenInfofile(f);

    if (indirectTable)
    {
	bp = &buffer[tagTable[p].pos - indirectTable[i].pos
		                                       + indirectTable[0].pos];
    }
    else
    {
	bp = &buffer[tagTable[p].pos];
    }

    if (bp[-1] == INFO_COOKIE) /* The Tag Table number doesn't always plonk */
			       /* you on the INFO_COOKIE */
    {
	++bp;
    }
    else
    {
	while (*bp && *bp != INFO_COOKIE)
	{
	    ++bp;
	}
	bp += 2;
    }
    
    sp = bp;
    if (LookingAt(bp, &bp) == IS_NODE)
    {
	for (i = 0;  i < numberTags;  ++i)
	{
	    if (strcmp(nodename, tagTable[i].tag) == 0)
	    {
		lastTag = currentTag;
		currentTag = i;
		break;
	    }
	}

	return ExtractNodeAndHyperize(sp, header, body);
    }
    else
    {
	return 0;
    }
}

void
InvertWidget(widget)
     Widget widget;
{
    Pixel fg, bg;

    if (!widget)
    {
	return;
    }

    XtVaGetValues(widget,
		  XtNforeground, &fg,
		  XtNbackground, &bg,
		  NULL);

    XtVaSetValues(widget,
		  XtNforeground, bg,
		  XtNbackground, fg,
		  NULL);
}

void
PositionNode(node, highlight)
     char *node;
     Boolean highlight;
{
    String label;
    WidgetList children;
    Cardinal numChildren;
    int child;
    Position x, y, tx, ty;
    Dimension w, h, pw, ph, tw, th;

    XtVaGetValues(tree,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    /* NB child = 1 excludes root widget */
    for (child = 1;  child < numChildren;  ++child)
    {
	XtVaGetValues(children[child],
		      XtNlabel, &label,
		      NULL);

	if (strcmp(node, label) == 0)
	{
	    if (highlight)
	    {
		InvertWidget(children[child]);
		InvertWidget(lastNode);
		lastNode = children[child];
	    }
	    break;
	}
    }

    XtVaGetValues(children[child],
		  XtNx, &x, XtNy, &y,
		  XtNwidth, &w, XtNheight, &h,
		  NULL);
    XtVaGetValues(porthole, XtNwidth, &pw, XtNheight, &ph, NULL);
    XtVaGetValues(tree, XtNx, &tx, XtNy, &ty, NULL);

/*
    if (y + h + ty < 0 || y + h + ty > ph || x + w + tx < 0 || x + w + tx > pw)
    {
*/
	XtVaSetValues(tree,
		      XtNx, -(x - pw / 2),
		      XtNy, -(y - ph / 2),
		      NULL);
/*
    }
*/
}

void
RevealNode(tag)
     char *tag;
{
    String node, header, body;
    int i;

    if (currentTag >= 0 && strcmp(tag, tagTable[currentTag].tag) == 0)
    {
	return;
    }

    if ( !(node = MakeNode(tag, &header, &body)) )
    {
	return;
    }

    HyperSetText(hyper, (body ? body : node));
    XtFree(node);

    if (header)
    {
	HyperSetText(links, header);
    }
    XtFree(header);

    if (XtIsSubclass(XtParent(hyper), viewportWidgetClass) &&
	                                                   XtIsRealized(hyper))
    {
	XawViewportSetLocation(XtParent(hyper), 0.0, 0.0);
    }

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    if (tree)
    {
	PositionNode(tag, True);
    }
#endif
}

void
ExposeNode(tag)
     char *tag;
{
    Widget popup;

    void MakeHyperWindow();

    if (hyper)
    {
	for (popup = XtParent(hyper);
	     !XtIsShell(popup);
	     popup = XtParent(popup)) {}

	XMapRaised(XtDisplay(hyper), XtWindow(popup));

	RevealNode(tag);
    }
    else
    {
	MakeHyperWindow(tag);
    }
}

void
DisplayNode(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    char *tag, *node;

    XtVaGetValues(widget, XtNlabel, &tag, NULL);
    tag = XtNewString(tag);

    ExposeNode(tag);

    XtFree(tag);
}

Widget
AddNode(name, parent)
     char *name;
     Widget parent;
{
    Widget node = XtVaCreateManagedWidget(name, commandWidgetClass, tree,
					  XtNtreeParent, parent,
					  NULL);

    XtAddCallback(node, XtNcallback, DisplayNode, (XtPointer) 0);

    return node;
}

Widget
FindNode(name)
     char *name;
{
    WidgetList children;
    Cardinal numChildren;
    int i;

    XtVaGetValues(tree,
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    for (i = 0;  i < numChildren;  ++i)
    {
	if (strcmp(XtName(children[i]), name) == 0)
	{
	    return children[i];
	}
    }

    return AddNode(name, root);
}

char*
ProcessMenuItem(buffer, parent)
     char *buffer;
     Widget parent;
{
    Widget node;
    char *sp, *bp, *nodename, *help;
    
    sp = bp = buffer;
    bp = Find(bp, ':');
    if (bp[1] != ':')
    {
	++bp;
	while (*bp == ' ')
	{
	    ++bp;
	}
	sp = bp;
        while (*bp != '\t' && *bp != ',' && *bp != '.' && *bp != '\n')
        {
            ++bp;
        }
    }

    BCOPY(sp, nodename, bp)
    node = FindNode(nodename);

    XtVaSetValues(node, XtNtreeParent, parent, NULL);
    
    free(nodename);

    return  bp;
}

char*
ProcessMenu(buffer, node)
     char *buffer;
     Widget node;
{
    char *bp;

    for(bp = buffer;  ;  )
    {
	bp = Locate(bp, INFO_MENU_ENTRY_LABEL);
	if (!*bp || *bp == INFO_COOKIE)
	{
	    break;
	}
	else
	{
	    bp = ProcessMenuItem(bp, node);
	}
    }

    return bp;
}

char*
ProcessNode(buffer)
     char *buffer;
{
    Widget node;
    char *bp, *sp, *s, c, *idx;

    /* It has got to exist */
    bp = Locate(buffer, INFO_NODE_LABEL);

    sp = bp;
    bp = Find(bp, ',');
    BCOPY(sp, s, bp)

    sp = s;
    while (*sp == ' ')
    {
	++sp;
    }
    if (strcmp(sp, "Top") == 0)
    {
	root = node = AddNode(sp, (Widget) 0);
    }
    else
    {
	/*
	 * There's potentially too much in indexes, so ignore them. And,
	 * furthermore, the repeated node names screw up the hierarchy.
	 */
	if ( (idx = rindex(sp, 'I')) && strcmp(idx, "Index") == 0)
	{
	    free(s);
	    return bp;
	}
	else
	{
	    node = FindNode(sp);
	}
    }
    free(s);

    c = *(bp = Locate(bp, INFO_MENU_LABEL));
    if (c == 0)
    {
	return 0;
    }
    else if (c != INFO_COOKIE)
    {
	bp = ProcessMenu(bp, node);
    }

    return bp;
}

Widget
InfoOf(widget)
     Widget widget;
{
    Widget popup;
    
    for (popup = XtParent(widget);
	 !XtIsShell(popup);
	 popup = XtParent(popup)) {}

    return XtNameToWidget(popup, "*info");
}

/*ARGSUSED*/
void
ShowInfo(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal num_params;
{
    XPointer value;
    String nodename;

    if(XFindContext(XtDisplay(widget),
		    (XID) XrmStringToQuark(XtName(widget)),
		    context, &value) == 0)
    {
	XtVaSetValues(InfoOf(widget), XtNlabel, (String) value, NULL);
    }
    else
    {
	XtVaSetValues(InfoOf(widget), XtNlabel, "", NULL);
    }
}

/*ARGSUSED*/
void
ClearInfo(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal num_params;
{
    XtVaSetValues(InfoOf(widget), XtNlabel, "", NULL);
}

void
BuildIndirectTable(buffer)
     char *buffer;
{
    char *f, v[16], *sp, *bp = ReverseLocate(buffer, INDIRECT_TAGS_TABLE_LABEL);
    char *subinfo;
    int len;

    indirectTable = (TagTablePtr) malloc((unsigned) TABLECHUNK
				                        * sizeof(TagTableRec));
    maxIndirects += TABLECHUNK;

    while (*bp != INFO_COOKIE)
    {
	sp = bp;
	bp = Find(bp, ':');
	BCOPY(sp, f, bp)
	sp = ++bp;
	bp = Find(sp, '\n');
	strncpy(v, sp, bp - sp);
	v[bp - sp] = 0;
        ++bp;

	if (numberIndirects == maxIndirects)
	{
	    maxIndirects += TABLECHUNK;
	    indirectTable
		= (TagTablePtr) realloc(indirectTable,
		      (unsigned) maxIndirects * sizeof(TagTableRec));
	}

	len = baselen + 1 + strlen(f) + 1;
	subinfo = XtMalloc((unsigned) len);
	strcpy(subinfo, basedir);
	strcat(subinfo, "/");
	strcat(subinfo, f);
	
	indirectTable[numberIndirects].tag = subinfo;
	indirectTable[numberIndirects].pos = atoi(v);
	++numberIndirects;
    }
}

void
BuildTagTable()
{
    char *bp, *sp, *s, v[16], *dummy;

    OpenInfofile(infofile);

    bp = ReverseLocate(buffer + buflen - 1, TAGS_TABLE_BEG_LABEL);

    if (LookingAt(bp, &dummy) == IS_INDIRECT_LABEL)
    {
	BuildIndirectTable(bp);
	
	while (*bp != '\n')
	{
	    ++bp;
	}
	++bp;
    }

    tagTable = (TagTablePtr) malloc((unsigned) TABLECHUNK
				                        * sizeof(TagTableRec));
    maxTags += TABLECHUNK;

    while (LookingAt(bp, &bp) == IS_TAG)
    {
	while (*bp == ' ')
	{
	    ++bp;
	}
	
	sp = bp;
	bp = Find(bp, INFO_TAGSEP);
	BCOPY(sp, s, bp)
	++bp;
	sp = bp;
	bp = Find(bp, '\n');
	strncpy(v, sp, bp - sp);
	v[bp - sp] = 0;
	++bp;

	if (numberTags == maxTags)
	{
	    maxTags += TABLECHUNK;
	    tagTable = (TagTablePtr)
		  realloc(tagTable, (unsigned) maxTags * sizeof(TagTableRec));
	}

	tagTable[numberTags].tag = s;
	tagTable[numberTags].pos = atoi(v);
	++numberTags;
    }
}

/*ARGSUSED*/
void
Quit(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    exit(0);
}

/*ARGSUSED*/
void
DeleteTree(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (hyper)
    {
	tree = 0;
	lastNode = 0;
	XtDestroyWidget(widget);
    }
    else
    {
	exit(0);
    }
}

/*ARGSUSED*/
void
CloseTree(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget shell;

    for (shell = XtParent(widget);
	 !XtIsShell(shell);
	 shell = XtParent(shell)) {}

    XtCallActionProc(shell, "delete-tree", (XEvent *) 0, (String *) 0, (Cardinal) 0);
}

void
MakeTreeWindow()
{
    Display *display = XtDisplay(top);
    Atom wm_delete_window;
    Widget popup, pane, form, bpane, quit, close, info, shell;
    Dimension height;
    char *bp, *dummy;
    int i;

    if (tree)
    {
	for (popup = XtParent(tree);
	     !XtIsShell(popup);
	     popup = XtParent(popup)) {}

	XMapRaised(XtDisplay(tree), XtWindow(popup));

	return;
    }

    popup = XtVaCreatePopupShell("treewin", topLevelShellWidgetClass, top,
				 NULL);

    pane = XtVaCreateManagedWidget("pane", panedWidgetClass, popup, NULL);

    form = XtVaCreateManagedWidget("form", formWidgetClass, pane, NULL);

    porthole = XtVaCreateManagedWidget("porthole", portholeWidgetClass, form,
				       NULL);

    tree = XtVaCreateManagedWidget("tree", treeWidgetClass, porthole, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("tree"), context,
			                "Press and drag any button to scroll");

    bpane = XtVaCreateManagedWidget("bpane", panedWidgetClass, pane,
				    XtNorientation, XtorientHorizontal,
				    XtNskipAdjust, True,
				    NULL);

    quit = XtVaCreateManagedWidget("quit", commandWidgetClass, bpane, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("quit"), context,
			                          "Terminate the application");
    XtAddCallback(quit, XtNcallback, Quit, (XtPointer) 0);

    close = XtVaCreateManagedWidget("close", commandWidgetClass, bpane,
				    NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("close"), context,
			                                 "Delete this window");
    XtAddCallback(close, XtNcallback, CloseTree, (XtPointer) 0);

    info = XtVaCreateManagedWidget("info", labelWidgetClass, bpane, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("info"), context,
			                "aXe Information System  Version 1.0");

    for (i = 0;  i < numberIndirects || numberIndirects == 0; ++i)
    {
	if (numberIndirects == 0)
	{ 
	    OpenInfofile(infofile);
	}
	else
	{
	    OpenInfofile(indirectTable[i].tag);
	}

	for (bp = buffer;  bp;  )
	{
	    if ( !(bp = Find(bp, INFO_COOKIE)) )
	    {
		break;
	    }

	    if (LookingAt(bp, &dummy) == IS_NODE)
	    {
		bp = ProcessNode(bp + 1);
	    }
	    else
	    {
		++bp;
	    }
	}

	if (numberIndirects == 0)
	{
	    break;
	}
    }
    
    XtRealizeWidget(popup);

    if (hyper)
    {
	for (shell = XtParent(hyper);
	     !XtIsShell(shell);
	     shell = XtParent(shell)) {}

	XtVaGetValues(shell, XtNheight, &height, NULL);
	XtVaSetValues(popup, XtNheight, height, NULL);
    }
    
    if (currentTag >= 0)
    {
	PositionNode(tagTable[currentTag].tag, True);
    }
    else
    {
	PositionNode("Top", False);
    }

    wm_delete_window = XInternAtom (display, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols(display, XtWindow(popup), &wm_delete_window, 1);

    XtPopup(popup, XtGrabNone);
}

void
GatherFromNode(buffer)
     char *buffer;
{
    char *bp, *sp, *fp, *nodename, ch, *help;

    bp = Locate(buffer, INFO_NODE_LABEL);
    
    while (*bp == ' ')
    {
	++bp;
    }
    
    sp = bp;
    bp = Find(bp, ',');
    BCOPY(sp, nodename, bp)

    while (*bp && *bp != INFO_COOKIE)
    {
	sp = bp;
	if (LookingAt(bp, &bp) == IS_SECTIONING)
	{
	    ch = sp[1]; 
	    while (*bp == ch)
	    {
		++bp;
	    }
	    if (*bp == '\n')
	    {
		fp = sp - 1;
		while (*fp != '\n')
		{
		    --fp;
		}
		++fp;
		BCOPY(fp, help, sp)

		(void) XSaveContext(XtDisplay(top),
				    (XID) XrmStringToQuark(nodename),
				    context, (XtPointer) help);
	    }
	    break;
	}
	else
	{
	    ++bp;
	}
    }

    free(nodename);

    return;
}

void
GatherSectioning()
{
    int tag, ind;
    char *node, *file;

    for (tag = 0;  tag < numberTags;  ++tag)
    {
	if (indirectTable)
	{
	    for (ind = numberIndirects - 1;  ind >= 0;  --ind)
	    {
		if (tagTable[tag].pos >= indirectTable[ind].pos)
		{
		    file = indirectTable[ind].tag;
		    break;
		}
	    }
	}
	else
	{
	    file = infofile;
	}
	
	OpenInfofile(file);
	
	if (indirectTable)
	{
	    node = &buffer[tagTable[tag].pos - indirectTable[ind].pos
			                              + indirectTable[0].pos];
	}
	else
	{
	    node = &buffer[tagTable[tag].pos];
	}
	
	GatherFromNode(node + 1);
    }
}
    
/*ARGSUSED*/
void
ShowNode(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    String tag = XtNewString(((hyperCallbackStruct *) call_data)->text);

    RevealNode(tag);

    XtFree(tag);
}

/*ARGSUSED*/
void
ExplainNode(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    hyperCallbackStruct *hcs = (hyperCallbackStruct *) call_data;

    if (hcs)
    {
	String tag = ((hyperCallbackStruct *) call_data)->text;
	XPointer value;

	if(XFindContext(XtDisplay(widget),
		            (XID) XrmStringToQuark(tag), context, &value) == 0)
	{
	    XtVaSetValues(InfoOf(widget), XtNlabel, (String) value, NULL);
	    return;
	}
    }

    XtVaSetValues(InfoOf(widget), XtNlabel, "", NULL);
}

/*ARGSUSED*/
void
Last(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    if (lastTag >= 0)
    {
	RevealNode(tagTable[lastTag].tag);
    }
}

/*ARGSUSED*/
void
Tree(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    MakeTreeWindow();
}

void
SetHyperWMHints(shell)
     Widget shell;
{
    Window window = XtWindow(shell);
    Display *display = XtDisplay(shell);
    WidgetList children;
    Widget viewport = XtNameToWidget(shell, "*view");
    Dimension wHeight, vHeight;
    int margin;
    XFontStruct *font;
    XSizeHints hints;
    long supp;

    XtVaGetValues(shell,
		  XtNheight, &wHeight,
		  NULL);

    XtVaGetValues(viewport,
		  XtNheight, &vHeight,
		  NULL);

    XtVaGetValues(hyper, 
		  XtNnormalFont, &font,
		  XtNmargin, &margin,
		  NULL);

    /*
     * All this kind of assumes that the height of
     * highlightFont is the same as that of normalFont.
     */
    if (!XGetWMNormalHints(display, window, &hints, &supp))
    {
        hints.flags = 0;
    }
    hints.flags |= PResizeInc | PBaseSize;
    hints.height_inc = font->ascent + font->descent;
    hints.width_inc = font->max_bounds.width;
    /* 4 = vSpace in MakeHyperWindow */
    hints.base_height = wHeight - vHeight + 4;  
    hints.base_width = 2 * margin;

    XSetWMNormalHints(display, window, &hints);
}

typedef struct {
    int rows;
    int columns;
} RowColRec, *RowColRecPtr;

static XtResource rowsColsResource[] =
{
    { "rows", "Rows", XtRInt, sizeof(int),
          XtOffset(RowColRecPtr, rows), XtRImmediate, (XtPointer) 24
    },
    { "columns", "Columns", XtRInt, sizeof(int),
          XtOffset(RowColRecPtr, columns), XtRImmediate, (XtPointer) 80
    },
};

void
MakeHyperWindow(nodename)
     char *nodename;
{
    Widget popup, pane, view, hpane, quit, map, info;
    char *node;
    Display *display = XtDisplay(top);
    Atom wm_delete_window;
    RowColRec rowsCols;
    XFontStruct *hyperFont, *normalFont;
    Dimension width, height;
    int rows, columns, margin, vSpace = 4;

    popup = XtVaCreatePopupShell("hyperwin", topLevelShellWidgetClass, top,
				 NULL);

    pane = XtVaCreateManagedWidget("pane", panedWidgetClass, popup, NULL);
    view = XtVaCreateManagedWidget("view", viewportWidgetClass, pane, NULL);

    hyper = XtVaCreateManagedWidget("hyper", hyperWidgetClass, view, NULL);
    XtAddCallback(hyper, XtNactivateCallback, ShowNode, (XtPointer) 0);    
    XtVaGetValues(hyper,
		  XtNnormalFont, &hyperFont,
		  XtNmargin, &margin,
		  NULL);
    XtVaSetValues(hyper,
		  XtNstartHighlight, soh,
		  XtNendHighlight, eoh,
		  NULL);

    links = XtVaCreateManagedWidget("links", hyperWidgetClass, pane,
				    XtNskipAdjust, True,
				    NULL);
    XtAddCallback(links, XtNactivateCallback, ShowNode, (XtPointer) 0);    
    XtAddCallback(links, XtNhyperCallback, ExplainNode, (XtPointer) 0);    
    XtVaGetValues(links,
		  XtNnormalFont, &normalFont,
		  NULL);
    XtVaSetValues(links,
		  XtNstartHighlight, soh,
		  XtNendHighlight, eoh,
		  NULL);

    hpane = XtVaCreateManagedWidget("hpane", panedWidgetClass, pane,
				    XtNorientation, XtorientHorizontal,
				    XtNskipAdjust, True,
				    NULL);

    quit = XtVaCreateManagedWidget("quit", commandWidgetClass, hpane, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("quit"), context,
			                       "Terminate the application");
    XtAddCallback(quit, XtNcallback, Quit, (XtPointer) 0);


#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    map = XtVaCreateManagedWidget("map", commandWidgetClass, hpane, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("map"), context,
			                      "Display the hypertext tree");
    XtAddCallback(map, XtNcallback, Tree, (XtPointer) top);
#endif

    info =  XtVaCreateManagedWidget("info", labelWidgetClass, hpane, NULL);
    (void) XSaveContext(display, (XID) XrmStringToQuark("info"), context,
			             "aXe Information System  Version 1.0");

    RevealNode(nodename);

    XtVaGetApplicationResources(popup, (XtPointer) &rowsCols,
                                rowsColsResource, XtNumber(rowsColsResource),
                                NULL);
    XtVaSetValues(view,
		  XtNheight,
		  (Dimension) (rowsCols.rows * (hyperFont->ascent +
						hyperFont->descent) + vSpace),
		  NULL);

    XtVaSetValues(hyper,
		  XtNwidth,
		    (Dimension) (rowsCols.columns * hyperFont->max_bounds.width
				 + 2 * margin),
		  NULL);

    XtVaSetValues(links,
		  XtNheight, normalFont->ascent + normalFont->descent + vSpace,
		  NULL);

    XtRealizeWidget(popup);

    wm_delete_window = XInternAtom (display, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols(display, XtWindow(popup), &wm_delete_window, 1);

    SetHyperWMHints(popup);

    XtPopup(popup, XtGrabNone);
}

/*ARGSUSED*/
void
StartDrag(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    dragX = event->xbutton.x_root;
    dragY = event->xbutton.y_root;
}

/*ARGSUSED*/
void
Drag(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Position x, y;

    XtVaGetValues(tree,
		  XtNx, &x,
		  XtNy, &y,
		  NULL);

    XtVaSetValues(tree,
		  XtNx, (Position) (x + (event->xbutton.x_root - dragX) * 10),
		  XtNy, (Position) (y + (event->xbutton.y_root - dragY) * 10),
		  NULL);

    dragX = event->xbutton.x_root;
    dragY = event->xbutton.y_root;
}

/*ARGSUSED*/
void
DeleteHyper(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (strcmp(XGetAtomName(XtDisplay(widget), event->xclient.data.l[0]),
	       "WM_DELETE_WINDOW") != 0)
    {
	return;
    }
    
    if (tree)
    {
	hyper = 0;
	XtDestroyWidget(widget);
    }
    else
    {
	exit(0);
    }
}

/*ARGSUSED*/
void 
Notify(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Atom gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;

    if (XGetWindowProperty(XtDisplay(widget),
                           XtWindow(widget),
                           XInternAtom(XtDisplay(widget), "AXE_AXINFO", False),
                           0L, 64L, /* 64 is an overestimate */
                           False,
                           XA_STRING,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
        if (gotAtom == XA_STRING)
        {
	    ExposeNode((char *) propValue);

            XtFree((char *) propValue);
        }
    }
    else
    {
	/* Ought to do something here just in case */
    }
}

XtActionsRec actions[] = {
    "delete-hyper", DeleteHyper,
    "delete-tree",  DeleteTree,
    "show-info",    ShowInfo,
    "clear-info",   ClearInfo,
    "start-drag",   StartDrag,
    "drag",         Drag,
    "notify",       Notify,
};

static XrmOptionDescRec options[] = {
    {"-tree",     ".tree",          XrmoptionNoArg,  (XPointer) "True"},
    {"-node",     ".node",          XrmoptionSepArg, (XPointer) 0},
    {"-rows",     "*hyperwin.rows", XrmoptionSepArg, (XPointer) 0},
};

static XtResource appResources[] =
{
    { "version", "Version", XtRString, sizeof(String),
          XtOffset(AppResRecPtr, version), XtRImmediate, (XtPointer) 0
    },
    { "tree", "Tree", XtRBoolean, sizeof(Boolean),
	  XtOffset(AppResRecPtr, tree), XtRImmediate, (XtPointer) False
    },
    { "node", "Node", XtRString, sizeof(String),
	  XtOffset(AppResRecPtr, node), XtRImmediate, (XtPointer) 0,
    },
    { "infoPath", "InfoPath", XtRString, sizeof(String),
          XtOffset(AppResRecPtr, dir), XtRImmediate, (XtPointer) 0
    },
};

main(argc, argv)
     int argc;
     char *argv[];
{
    XtAppContext app;
    Display *display;
    AppResRec resources;
    String file;
    int len;

    void Listen();

    context = XUniqueContext();

    top = XtVaAppInitialize(&app, "Axinfo",
                            options, XtNumber(options),
                            &argc, argv,
                            (String *) 0, NULL);
    display = XtDisplay(top);

    XtVaGetApplicationResources(top, (XtPointer) &resources,
                                appResources, XtNumber(appResources),
                                NULL);

    if (!resources.version || strcmp(resources.version, QVERSION) != 0)
    {
	char message[1024];
	Widget noapps, msg, quit;

        strcpy(message, "This is ");
        strcat(message, QVERSION);
        strcat(message, ".\n\n");
	strcat(message, "The application defaults file\n");
	strcat(message, "for this release has not been\n");
	strcat(message, "found.");

        if (resources.version)
        {
	    strcat(message, "\n\nIt may help to know in solving\n");
	    strcat(message, "the problem that the  defaults\n");
	    strcat(message, "file for ");
            strcat(message, resources.version);
            strcat(message, " was found.");
        }

	noapps = XtVaCreateManagedWidget("noapps", formWidgetClass, top, NULL);

	msg = XtVaCreateManagedWidget("msg", labelWidgetClass, noapps,
				      XtNlabel, message,
				      NULL);

	quit = XtVaCreateManagedWidget("Quit", commandWidgetClass, noapps,
					XtNfromVert, msg,
					NULL);
	XtAddCallback(quit, XtNcallback, Quit, (XtPointer) 0);
	
	XtRealizeWidget(top);
	XtAppMainLoop(app);
    }

    basedir = XtNewString(resources.dir);
    baselen = strlen(basedir);

    MAKESTRING(file,INFO_FILE_LABEL)
    MAKESTRING(node,INFO_NODE_LABEL)
    MAKESTRING(prev,INFO_PREV_LABEL)
    MAKESTRING(next,INFO_NEXT_LABEL)
    MAKESTRING(up,INFO_UP_LABEL)
    MAKESTRING(menu,INFO_MENU_LABEL)
    MAKESTRING(entry,INFO_MENU_ENTRY_LABEL)
    MAKESTRING(xref,INFO_XREF_LABEL)
    MAKESTRING(btag,TAGS_TABLE_BEG_LABEL)
    MAKESTRING(etag,TAGS_TABLE_END_LABEL)
    MAKESTRING(itag,INDIRECT_TAGS_TABLE_LABEL)
    MAKESTRING(tagi,TAGS_TABLE_IS_INDIRECT_LABEL)

    XtAppAddActions(app, actions, XtNumber(actions));

    if (argc > 1)
    {
	file = XtNewString(argv[1]);
    }
    else
    {
	file = XtNewString(INFOFILE);
    }

    len = baselen + 1 + strlen(file) + 1;
    infofile = XtMalloc((unsigned) len);
    strcpy(infofile, basedir);
    strcat(infofile, "/");
    strcat(infofile, file);
    XtFree(file);

    BuildTagTable();
    GatherSectioning();

    {
	int dummy;

	if (resources.node && IsTag(resources.node, &dummy))
	{
	    MakeHyperWindow(resources.node);
	}
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
	else if (!resources.tree)
	{
	    MakeHyperWindow("Top");
	}

	if (resources.tree)
	{
	    MakeTreeWindow();
	}
#else
	else
	{
	    MakeHyperWindow("Top");
	}
#endif
    }

    Listen();

    XtAppMainLoop(app);
}


void
Listen()
{
    Widget listen;
    Window window;
    Display *display;

    listen = XtVaCreatePopupShell("listen", topLevelShellWidgetClass, top,
				  NULL);
    (void) XtVaCreateManagedWidget("core", coreWidgetClass, listen,
				   XtNwidth, 1, XtNheight, 1,
				   NULL);
    XtRealizeWidget(listen);

    display = XtDisplay(listen);
    
    window = XtWindow(listen);

    TrapErrors(display);

    /* If axeis is already running this one will take over listening duties */
    XChangeProperty(display,
		    XDefaultRootWindow(display),
		    AxinfoUserAtom(display),
		    XA_WINDOW,
		    32,
		    PropModeReplace,
		    (unsigned char *) &window,
		    1);

    /*
     * Stamping the listening window with the AXE_AXINFO_user property
     * enables a check to be made that the window id stored under the
     * AXE_AXINFO_user property on the root window is still valid when
     * aXe comes to attempt to notify axeis. If the window exists but it
     * doesn't have the property then axeis has been terminated and
     * the id reallocated, otherwise we should be OK.
     */
    if (xerror == Success)
    {
	XChangeProperty(display,
			window,
			AxinfoUserAtom(display),
			XA_STRING,
			8,
			PropModeReplace,
			NULL,
			0);
    }

    DontTrapErrors(display);
}
