/*		Access Manager					HTAccess.c
**		==============
**
** Authors
**	TBL	Tim Berners-Lee timbl@info.cern.ch
**	JFG	Jean-Francois Groff jfg@dxcern.cern.ch
**	DD	Denis DeLaRoca (310) 825-4580  <CSP1DWD@mvs.oac.ucla.edu>
** History
**       8 Jun 92 Telnet hopping prohibited as telnet is not secure TBL
**	26 Jun 92 When over DECnet, suppressed FTP, Gopher and News. JFG
**	 6 Oct 92 Moved HTClientHost and logfile into here. TBL
**	17 Dec 92 Tn3270 added, bug fix. DD
**	 4 Feb 93 Access registration, Search escapes bad chars TBL
**		  PARAMETERS TO HTSEARCH AND HTLOADRELATIVE CHANGED
**	28 May 93 WAIS gateway explicit if no WAIS library linked in.
**
** Bugs
**	This module assumes that that the graphic object is hypertext, as it
**	needs to select it when it has been loaded.  A superclass needs to be
**	defined which accepts select and select_anchor.
*/

#ifndef DEFAULT_WAIS_GATEWAY
/* #define DEFAULT_WAIS_GATEWAY "http://info.cern.ch:8001/" */
#define DEFAULT_WAIS_GATEWAY "http://www.ncsa.uiuc.edu:8001/"
#endif

/* Implements:
*/
#include "HTAccess.h"

/* Uses:
*/

#include "HTParse.h"
#include "HTUtils.h"
#include "HTML.h"		/* SCW */

#ifndef NO_RULES
#include "HTRules.h"
#endif

#include <stdio.h>

#include "HTList.h"
#include "HText.h"	/* See bugs above */
#include "HTAlert.h"


/*	These flags may be set to modify the operation of this module
*/
PUBLIC char * HTClientHost = 0;	/* Name of remote login host if any */
PUBLIC FILE * logfile = 0;	/* File to which to output one-liners */
PUBLIC BOOL HTSecure = NO;	/* Disable access for telnet users? */

PUBLIC BOOL using_proxy = NO; /* are we using a proxy gateway? */
/*	To generate other things, play with these:
*/

PUBLIC HTFormat HTOutputFormat = NULL;
PUBLIC HTStream* HTOutputStream = NULL;	/* For non-interactive, set this */ 

PRIVATE HTList * protocols = NULL;   /* List of registered protocol descriptors */


/*	Register a Protocol				HTRegisterProtocol
**	-------------------
*/

PUBLIC BOOL HTRegisterProtocol(protocol)
	HTProtocol * protocol;
{
    if (!protocols) protocols = HTList_new();
    HTList_addObject(protocols, protocol);
    return YES;
}


/*	Register all known protocols
**	----------------------------
**
**	Add to or subtract from this list if you add or remove protocol modules.
**	This routine is called the first time the protocol list is needed,
**	unless any protocols are already registered, in which case it is not called.
**	Therefore the application can override this list.
**
**	Compiling with NO_INIT prevents all known protocols from being forced
**	in at link time.
*/
#ifndef NO_INIT
PRIVATE void HTAccessInit NOARGS			/* Call me once */
{
GLOBALREF HTProtocol HTTP, HTFile, HTTelnet, HTTn3270, HTRlogin;
#ifndef DECNET
GLOBALREF  HTProtocol HTFTP, HTNews, HTGopher;
#ifdef DIRECT_WAIS
GLOBALREF  HTProtocol HTWAIS;
#endif
    HTRegisterProtocol(&HTFTP);
    HTRegisterProtocol(&HTNews);
    HTRegisterProtocol(&HTGopher);
#ifdef DIRECT_WAIS
    HTRegisterProtocol(&HTWAIS);
#endif
#endif

    HTRegisterProtocol(&HTTP);
    HTRegisterProtocol(&HTFile);
    HTRegisterProtocol(&HTTelnet);
    HTRegisterProtocol(&HTTn3270);
    HTRegisterProtocol(&HTRlogin);
}
#endif

 /*                                                  override_proxy()
 **
 **  Check the no_proxy environment variable to get the list
 **  of hosts for which proxy server is not consulted.
 **
 **  no_proxy is a comma- or space-separated list of machine
 **  or domain names, with optional :port part.  If no :port
 **  part is present, it applies to all ports on that domain.
 **
 **  Example:
 **          no_proxy="cern.ch,some.domain:8001"
 **
 */
 PRIVATE BOOL override_proxy ARGS1(CONST char *, addr)
{
    CONST char * no_proxy = getenv("no_proxy");
    char * p = NULL;
    char * host = NULL;
    int port = 0;
    int h_len = 0;

    if (!no_proxy || !addr || !(host = HTParse(addr, "", PARSE_HOST)))
    return NO;
    if (!*host) { free(host); return NO; }

    if (p = strchr(host, ':')) {    /* Port specified */
    *p++ = 0;                       /* Chop off port */
    port = atoi(p);
    }
    else {                          /* Use default port */
    char * access = HTParse(addr, "", PARSE_ACCESS);
    if (access) {
        if      (!strcmp(access,"http"))    port = 80;
        else if (!strcmp(access,"gopher"))  port = 70;
        else if (!strcmp(access,"ftp"))     port = 21;
        free(access);
    }
    }
    if (!port) port = 80;           /* Default */
    h_len = strlen(host);

    while (*no_proxy) {
    CONST char * end;
    CONST char * colon = NULL;
    int templ_port = 0;
    int t_len;

    while (*no_proxy && (WHITE(*no_proxy) || *no_proxy==','))
        no_proxy++;                 /* Skip whitespace and separators */

    end = no_proxy;
    while (*end && !WHITE(*end) && *end != ',') {   /* Find separator */
        if (*end==':') colon = end;                 /* Port number given */
        end++;
    }

    if (colon) {
        templ_port = atoi(colon+1);
        t_len = colon - no_proxy;
    }
    else {
        t_len = end - no_proxy;
    }

    if ((!templ_port || templ_port == port)  &&
        (t_len > 0  &&  t_len <= h_len  &&
         !strncmp(host + h_len - t_len, no_proxy, t_len))) {
        free(host);
        return YES;
    }
    if (*end) no_proxy = end+1;
    else break;
    }

    free(host);
    return NO;
}

/*		Find physical name and access protocol
**		--------------------------------------
**
**
** On entry,
**	addr		must point to the fully qualified hypertext reference.
**	anchor		a pareent anchor with whose address is addr
**
** On exit,
**	returns		HT_NO_ACCESS		Error has occured.
**			HT_OK			Success
**
*/
PRIVATE int get_physical ARGS2(
	CONST char *,		addr,
	HTParentAnchor *,	anchor)
{
    char * access=0;	/* Name of access method */
    char * physical = 0;
    
#ifndef NO_RULES
    physical = HTTranslate(addr);
    if (!physical) {
	return HT_FORBIDDEN;
    }
    HTAnchor_setPhysical(anchor, physical);
    free(physical);			/* free our copy */
#else
    HTAnchor_setPhysical(anchor, addr);
#endif

    access =  HTParse(HTAnchor_physical(anchor),
    		"file:", PARSE_ACCESS);

/*	Check whether gateway access has been set up for this
**
**	This function can be replaced by the rule system above.
*/
#define USE_GATEWAYS
#ifdef USE_GATEWAYS
	/*
	 *	Make sure the using_proxy variable is false.
	 */
	using_proxy = NO;

    if(!override_proxy(addr)) {
	char * gateway_parameter, *gateway, *proxy;

	/* search for gateways */
	gateway_parameter = (char *)malloc(strlen(access)+20);
	if (gateway_parameter == NULL) outofmem(__FILE__, "HTLoad");
	strcpy(gateway_parameter, "WWW_");
	strcat(gateway_parameter, access);
	strcat(gateway_parameter, "_GATEWAY");
	gateway = (char *)getenv(gateway_parameter); /* coerce for decstation */

	/* search for proxy servers */
	strcpy(gateway_parameter, access);
        strcat(gateway_parameter, "_proxy");
	proxy = (char *)getenv(gateway_parameter);
	free(gateway_parameter);

	if(TRACE && gateway)
	    fprintf(stderr,"Gateway found: %s\n",gateway);
	if(TRACE && proxy)
	    fprintf(stderr,"proxy server found: %s\n",proxy);
	
#ifndef DIRECT_WAIS
	if (!gateway && 0==strcmp(access, "wais")) {
	    gateway = DEFAULT_WAIS_GATEWAY;
	}
#endif /* direct wais */

	/* proxy servers have precedence over gateway servers */
	if(proxy) {
	    char * gatewayed=0;
            StrAllocCopy(gatewayed,proxy);
            StrAllocCat(gatewayed,addr);
            using_proxy = YES;
            HTAnchor_setPhysical(anchor, gatewayed);
	    free(gatewayed);
	    free(access);

    	    access =  HTParse(HTAnchor_physical(anchor),
    		"http:", PARSE_ACCESS);

	} else if (gateway) {
	    char * path = HTParse(addr, "",
	    	PARSE_HOST + PARSE_PATH + PARSE_PUNCTUATION);
		/* Chop leading / off to make host into part of path */
	    char * gatewayed = HTParse(path+1, gateway, PARSE_ALL);
	    free(path);
            HTAnchor_setPhysical(anchor, gatewayed);
	    free(gatewayed);
	    free(access);
	    
    	    access =  HTParse(HTAnchor_physical(anchor),
    		"http:", PARSE_ACCESS);
	} 
    }
#endif /* use gateways */


/*	Search registered protocols to find suitable one
*/
    {
	int i, n;
#ifndef NO_INIT
        if (!protocols) HTAccessInit();
#endif
	n = HTList_count(protocols);
	for (i=0; i<n; i++) {
	    HTProtocol *p = HTList_objectAt(protocols, i);
	    if (strcmp(p->name, access)==0) {
		HTAnchor_setProtocol(anchor, p);
		free(access);
		return (HT_OK);
	    }
	}
    }

    free(access);
    return HT_NO_ACCESS;
}


/*		Load a document
**		---------------
**
**	This is an internal routine, which has an address AND a matching
**	anchor.  (The public routines are called with one OR the other.)
**
** On entry,
**	addr		must point to the fully qualified hypertext reference.
**	anchor		a pareent anchor with whose address is addr
**
** On exit,
**	returns		<0		Error has occured.
**			HT_LOADED	Success
**			HT_NO_DATA	Success, but no document loaded.
**					(telnet sesssion started etc)
**
*/
PRIVATE int HTLoad ARGS4(
	CONST char *,		addr,
	HTParentAnchor *,	anchor,
	HTFormat,		format_out,
	HTStream *,		sink)
{
    HTProtocol* p;
    int status = get_physical(addr, anchor);
    if (status == HT_FORBIDDEN) {
        return HTLoadError(sink, 500, "Access forbidden by rule");
    }
    if (status < 0) return status;	/* Can't resolve or forbidden */
    
    p = HTAnchor_protocol(anchor);
    return (*(p->load))(HTAnchor_physical(anchor),
    			anchor, format_out, sink);
}

/*		Get a save stream for a document
**		--------------------------------
*/
PUBLIC HTStream *HTSaveStream ARGS1(HTParentAnchor *, anchor)
{
    HTProtocol * p = HTAnchor_protocol(anchor);
    if (!p) return NULL;
    
    return (*p->saveStream)(anchor);
    
}


/*		Load a document - with logging etc
**		----------------------------------
**
**	- Checks or documents already loaded
**	- Logs the access
**	- Allows stdin filter option
**	- Trace ouput and error messages
**
**    On Entry,
**	  anchor	    is the node_anchor for the document
**        full_address      The address of the document to be accessed.
**        filter            if YES, treat stdin as HTML
**
**    On Exit,
**        returns    YES     Success in opening document
**                   NO      Failure 
**
*/
PUBLIC char *use_this_url_instead;

PRIVATE BOOL HTLoadDocument ARGS4(
	CONST char *,		full_address,
	HTParentAnchor *,	anchor,
	HTFormat,		format_out,
	HTStream*,		sink)

{
    int	        status;
    HText *	text;
    char * address_to_load = (char *)full_address;
    extern char LYforce_no_cache;  /* from GridText.c */

#ifdef DIRED_SUPPORT
    extern BOOLEAN lynx_edit_mode;
#endif

    use_this_url_instead = 0;

    if (TRACE) fprintf (stderr,
      "HTAccess: loading document %s\n", address_to_load);

    if (!LYforce_no_cache && (text=(HText *)HTAnchor_document(anchor))) {	
	/* Already loaded */
        if (TRACE) fprintf(stderr, "HTAccess: Document already in memory.\n");
        HText_select(text);

#ifdef DIRED_SUPPORT
	if (HTAnchor_format(anchor) == WWW_DIRED)
	   lynx_edit_mode = TRUE;
#endif
	return YES;
    }

    LYforce_no_cache = NO;  /* reset after each time through */
    
    /* I LOVE goto's! */
try_again:
    status = HTLoad(address_to_load, anchor, format_out, sink);

    
/*	Log the access if necessary
*/
    if (logfile) {
	time_t theTime;
	time(&theTime);
	fprintf(logfile, "%24.24s %s %s %s\n",
	    ctime(&theTime),
	    HTClientHost ? HTClientHost : "local",
	    status<0 ? "FAIL" : "GET",
	    full_address);
	fflush(logfile);	/* Actually update it on disk */
	if (TRACE) fprintf(stderr, "Log: %24.24s %s %s %s\n",
	    ctime(&theTime),
	    HTClientHost ? HTClientHost : "local",
	    status<0 ? "FAIL" : "GET",
	    full_address);
    }
    

    if (status == HT_LOADED) {
	if (TRACE) {
	    fprintf(stderr, "HTAccess: `%s' has been accessed.\n",
	    full_address);
	}
	return YES;
    }

    if (status == HT_REDIRECTING)
      {
        /* Exported from HTMIME.c, of all places. */
        extern char *redirecting_url;  
        if (TRACE)
          {
            fprintf (stderr, "HTAccess: '%s' is a redirection URL.\n",
                     address_to_load);
            fprintf (stderr, "HTAccess: Redirecting to '%s'\n",
                     address_to_load);
          }
	/* prevent circular references */
	if(strcmp(address_to_load, redirecting_url)) {  /* if different */
            address_to_load = redirecting_url;
            use_this_url_instead = redirecting_url;
	    StrAllocCopy(anchor->address, redirecting_url);
	    anchor->post_data = 0;
            goto try_again;
	}
      }

    if (status == HT_NO_DATA) {
	if (TRACE) {
	    fprintf(stderr, 
	    "HTAccess: `%s' has been accessed, No data left.\n",
	    full_address);
	}
	return NO;
    }
    
    if (status == HT_INTERRUPTED) {
	if (TRACE) {
	    fprintf(stderr, 
	    "HTAccess: `%s' has been accessed, transfer interrupted.\n",
	    full_address);
	}
/*	HTProgress("Data transfer interrupted."); */
	return NO;
    }

    if (status<=0) {		      /* Failure in accessing a document */
        user_message("Can't access `%s'", full_address);
	if (TRACE) fprintf(stderr, 
		"HTAccess: Can't access `%s'\n", full_address);
	HTLoadError(sink, 500, "Unable to access document.");
	return NO;
    }
 
    /* If you get this, then please find which routine is returning
       a positive unrecognised error code! */
 
    fprintf(stderr,
    "**** HTAccess: socket or file number returned by obsolete load routine!\n");
    fprintf(stderr,
    "**** HTAccess: Internal software error. Please mail www-bug@info.cern.ch!\n");
    fprintf(stderr, "**** HTAccess: Status returned was: %d\n",status);
    exit(-6996);

} /* HTLoadDocument */



/*		Load a document from absolute name
**		---------------
**
**    On Entry,
**        addr     The absolute address of the document to be accessed.
**        filter   if YES, treat document as HTML
**
**    On Exit,
**        returns    YES     Success in opening document
**                   NO      Failure 
**
**
*/

PUBLIC BOOL HTLoadAbsolute ARGS1(CONST DocAddress *,docaddr)
{
   return HTLoadDocument(docaddr->address,
       		HTAnchor_parent(HTAnchor_findAddress(docaddr)),
       			HTOutputFormat ? HTOutputFormat : WWW_PRESENT,
			HTOutputStream);
}


#ifdef NOT_USED_CODE

/*		Load a document from absolute name to stream
**		--------------------------------------------
**
**    On Entry,
**        addr     The absolute address of the document to be accessed.
**        sink     if non-NULL, send data down this stream
**
**    On Exit,
**        returns    YES     Success in opening document
**                   NO      Failure 
**
**
*/

PUBLIC BOOL HTLoadToStream ARGS3(
		CONST char *,	addr,
		BOOL, 		filter,
		HTStream *, 	sink)
{
   return HTLoadDocument(addr,
       		HTAnchor_parent(HTAnchor_findAddress(addr)),
       			HTOutputFormat ? HTOutputFormat : WWW_PRESENT,
			sink);
}

#endif /* NOT_USED_CODE */



/*		Load a document from relative name
**		---------------
**
**    On Entry,
**        relative_name     The relative address of the document
**	  		    to be accessed.
**
**    On Exit,
**        returns    YES     Success in opening document
**                   NO      Failure 
**
**
*/

PUBLIC BOOL HTLoadRelative ARGS2(
		CONST char *,		relative_name,
		HTParentAnchor *,	here)
{
    DocAddress 		full_address;
    BOOL       		result;
    char * 		mycopy = 0;
    char * 		stripped = 0;
    char *		current_address =
    				HTAnchor_address((HTAnchor*)here);

    full_address.address = 0;
    full_address.post_data = 0;
    full_address.post_content_type = 0;

    StrAllocCopy(mycopy, relative_name);

    stripped = HTStrip(mycopy);
    full_address.address = HTParse(stripped,
	           current_address,
		   PARSE_ACCESS|PARSE_HOST|PARSE_PATH|PARSE_PUNCTUATION);
    result = HTLoadAbsolute(&full_address);
    free(full_address.address);
    free(current_address);
    free(mycopy);  /* Memory leak fixed 10/7/92 -- JFG */
    return result;
}


/*		Load if necessary, and select an anchor
**		--------------------------------------
**
**    On Entry,
**        destination      	    The child or parenet anchor to be loaded.
**
**    On Exit,
**        returns    YES     Success
**                   NO      Failure 
**
*/

PUBLIC BOOL HTLoadAnchor ARGS1(HTAnchor *,destination)
{
    HTParentAnchor * parent;
    BOOL loaded = NO;
    if (!destination) return NO;	/* No link */
    
    parent  = HTAnchor_parent(destination);
    
    if (HTAnchor_document(parent) == NULL) {	/* If not alread loaded */
    						/* TBL 921202 */

        BOOL result;
        char * address = HTAnchor_address((HTAnchor*) parent);
	result = HTLoadDocument(address, parent,
		HTOutputFormat ? HTOutputFormat : WWW_PRESENT,
		HTOutputStream);
	free(address);
	if (!result) return NO;
	loaded = YES;
    }
    
    {
	HText *text = (HText*)HTAnchor_document(parent);
	if (destination != (HTAnchor *)parent) {  /* If child anchor */
	    HText_selectAnchor(text, 
		    (HTChildAnchor*)destination); /* Double display? @@ */
	} else {
	    if (!loaded) HText_select(text);
	}
    }
    return YES;
	
} /* HTLoadAnchor */


/*		Search
**		------
**  Performs a keyword search on word given by the user. Adds the keyword to 
**  the end of the current address and attempts to open the new address.
**
**  On Entry,
**       *keywords  	space-separated keyword list or similar search list
**	here		is anchor search is to be done on.
*/

PRIVATE char hex(i)
    int i;
{
    char * hexchars = "0123456789ABCDEF";
    return hexchars[i];
}

PUBLIC BOOL HTSearch ARGS2(
	CONST char *, 		keywords,
	HTParentAnchor *, 	here)
{

#define acceptable \
"1234567890abcdefghijlkmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-_"

    char *q, *u;
    CONST char * p, *s, *e;		/* Pointers into keywords */
    char * address = 0;
    BOOL result;
    char * escaped = malloc(strlen(keywords)*3+1);
    static CONST BOOL isAcceptable[96] =

    /*   0 1 2 3 4 5 6 7 8 9 A B C D E F */
    {    0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,	/* 2x   !"#$%&'()*+,-./	 */
         1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,	/* 3x  0123456789:;<=>?	 */
	 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* 4x  @ABCDEFGHIJKLMNO  */
	 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,	/* 5X  PQRSTUVWXYZ[\]^_	 */
	 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* 6x  `abcdefghijklmno	 */
	 1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0 };	/* 7X  pqrstuvwxyz{\}~	DEL */

    if (escaped == NULL) outofmem(__FILE__, "HTSearch");
    
    StrAllocCopy(address, here->isIndexAction);

/*	Convert spaces to + and hex escape unacceptable characters
*/
    for(s=keywords; *s && WHITE(*s); s++) /*scan */ ;	/* Skip white space */
    for(e = s + strlen(s); e>s && WHITE(*(e-1)) ; e--); /* Skip trailers */
    for(q=escaped, p=s; p<e; p++) {			/* scan stripped field */
        int c = (int)TOASCII(*p);
        if (WHITE(*p)) {
	    *q++ = '+';
	} else if (c>=32 && c<=(char)127 && isAcceptable[c-32]) {
	    *q++ = *p;			/* 930706 TBL for MVS bug */
	} else {
	    *q++ = '%';
	    *q++ = hex(c / 16);
	    *q++ = hex(c % 16);
	}
    } /* Loop over string */
    
    *q=0;
    				/* terminate escaped sctring */
    u=strchr(address, '?');		/* Find old search string */
    if (u) *u = 0;			        /* Chop old search off */

    StrAllocCat(address, "?");
    StrAllocCat(address, escaped);
    free(escaped);
    result = HTLoadRelative(address, here);
    free(address);
    
    return result;
}


/*		Search Given Indexname
**		------
**  Performs a keyword search on word given by the user. Adds the keyword to 
**  the end of the current address and attempts to open the new address.
**
**  On Entry,
**       *keywords  	space-separated keyword list or similar search list
**	*addres		is name of object search is to be done on.
*/

PUBLIC BOOL HTSearchAbsolute ARGS2(
	CONST char *, 	keywords,
	CONST char *, 	indexname)
{
    DocAddress abs_doc;
    HTParentAnchor * anchor;
    abs_doc.address = (char *)indexname;
    abs_doc.post_data = 0;
    abs_doc.post_content_type = 0;

    anchor = (HTParentAnchor*) HTAnchor_findAddress(&abs_doc);
    return HTSearch(keywords, anchor);
}

#ifdef NOT_USED_CODE
/*		Generate the anchor for the home page
**		-------------------------------------
**
**	As it involves file access, this should only be done once
**	when the program first runs.
**	This is a default algorithm -- browser don't HAVE to use this.
**	But consistency betwen browsers is STRONGLY recommended!
**
**	Priority order is:
**
**		1	WWW_HOME environment variable (logical name, etc)
**		2	~/WWW/default.html
**		3	/usr/local/bin/default.html
**		4	http://info.cern.ch/default.html
**
*/
PUBLIC HTParentAnchor * HTHomeAnchor NOARGS
{
    char * my_home_document = NULL;
    char * home = (char *)getenv(LOGICAL_DEFAULT);
    char * ref;
    HTParentAnchor * anchor;
    
    if (home) {
        StrAllocCopy(my_home_document, home);
    
/* 	Someone telnets in, they get a special home.
*/
#define MAX_FILE_NAME 1024					/* @@@ */
    } else  if (HTClientHost) {			/* Telnet server */
    	FILE * fp = fopen(REMOTE_POINTER, "r");
	char * status;
	if (fp) {
	    my_home_document = (char*) malloc(MAX_FILE_NAME);
	    status = fgets(my_home_document, MAX_FILE_NAME, fp);
	    if (!status) {
	        free(my_home_document);
		my_home_document = NULL;
	    }
	    fclose(fp);
	}
	if (!my_home_document) StrAllocCopy(my_home_document, REMOTE_ADDRESS);
    }

    

#ifdef unix

    if (!my_home_document) {
	FILE * fp = NULL;
	CONST char * home =  (CONST char*)getenv("HOME");
	if (home) { 
	    my_home_document = (char *)malloc(
		strlen(home)+1+ strlen(PERSONAL_DEFAULT)+1);
	    if (my_home_document == NULL) outofmem(__FILE__, "HTLocalName");
	    sprintf(my_home_document, "%s/%s", home, PERSONAL_DEFAULT);
	    fp = fopen(my_home_document, "r");
	}
	
	if (!fp) {
	    StrAllocCopy(my_home_document, LOCAL_DEFAULT_FILE);
	    fp = fopen(my_home_document, "r");
	}
	if (fp) {
	    fclose(fp);
	} else {
	if (TRACE) fprintf(stderr,
	    "HTBrowse: No local home document ~/%s or %s\n",
	    PERSONAL_DEFAULT, LOCAL_DEFAULT_FILE);
	    free(my_home_document);
	    my_home_document = NULL;
	}
    }
#endif
    ref = HTParse( my_home_document ?	my_home_document :
				HTClientHost ? REMOTE_ADDRESS
				: LAST_RESORT,
		    "file:",
		    PARSE_ACCESS|PARSE_HOST|PARSE_PATH|PARSE_PUNCTUATION);
    if (my_home_document) {
	if (TRACE) fprintf(stderr,
	    "HTAccess: Using custom home page %s i.e. address %s\n",
	    my_home_document, ref);
	free(my_home_document);
    }
    anchor = (HTParentAnchor*) HTAnchor_findAddress(ref);
    free(ref);
    return anchor;
}
#endif /* NOT_USED_CODE */

