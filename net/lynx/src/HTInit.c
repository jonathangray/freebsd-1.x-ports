/*		Configuration-specific Initialialization	HTInit.c
**		----------------------------------------
*/

/*	Define a basic set of suffixes and presentations
**	------------------------------------------------
*/

/* Implements:
*/
#include "HTInit.h"

#include "HTML.h"
#include "HTPlain.h"
#include "HTMLGen.h"
#include "HTFile.h"
#include "HTFormat.h"
#include "HTMIME.h"
#include "HTWSRC.h"

#include "HTSaveToFile.h"  /* LJM */
#include "userdefs.h"

PUBLIC void HTFormatInit NOARGS
{
#ifdef NeXT
  HTSetPresentation("application/postscript",   "open %s",	1.0, 2.0, 0.0);
  HTSetPresentation("image/x-tiff", 		"open %s", 	1.0, 2.0, 0.0);
  HTSetPresentation("audio/basic", 		"open %s", 	1.0, 2.0, 0.0);
  HTSetPresentation("*", 			"open %s", 	1.0, 0.0, 0.0);
#else
 if (getenv("DISPLAY")) {	/* Must have X11 */
  HTSetPresentation("application/postscript", "ghostview %s&",	1.0, 3.0, 0.0);
  HTSetPresentation("image/gif", 	XLOADIMAGE_COMMAND, 	1.0, 3.0, 0.0);
  HTSetPresentation("image/x-xbm", 	XLOADIMAGE_COMMAND, 	1.0, 3.0, 0.0);
  HTSetPresentation("image/x-rgb", 	XLOADIMAGE_COMMAND, 	1.0, 3.0, 0.0);
  HTSetPresentation("image/x-tiff", 	XLOADIMAGE_COMMAND, 	1.0, 3.0, 0.0);
  HTSetPresentation("image/jpeg", 	XLOADIMAGE_COMMAND, 	1.0, 3.0, 0.0);
  HTSetPresentation("video/mpeg", 	"mpeg_play %s &",1.0, 3.0, 0.0);

 }
#endif

#ifdef EXEC_SCRIPTS
 /* set quality to 999.0 for protected exec applications */
#ifndef VMS
 HTSetPresentation("application/x-csh",	"csh %s", 999.0, 3.0, 0.0);
 HTSetPresentation("application/x-sh",	"sh %s",  999.0, 3.0, 0.0);
 HTSetPresentation("application/x-ksh",	"ksh %s", 999.0, 3.0, 0.0);
#else
 HTSetPresentation("application/x-VMS_script",	"@%s", 999.0, 3.0, 0.0);
#endif /* not VMS */
#endif /* EXEC_SCRIPTS */

 HTSetConversion("www/mime",  "*",  	     HTMIMEConvert, 	1.0, 0.0, 0.0);

 HTSetConversion("application/x-wais-source",
 		      "www/present",  	     HTPlainPresent, 	1.0, 0.0, 0.0);
 HTSetConversion("text/html", "text/x-c",    HTMLToC, 	        0.5, 0.0, 0.0);
 HTSetConversion("text/html", "text/plain",  HTMLToPlain, 	0.5, 0.0, 0.0);
 HTSetConversion("text/html", "www/present", HTMLPresent, 	1.0, 0.0, 0.0);
 HTSetConversion("text/plain", "www/present", HTPlainPresent,	1.0, 0.0, 0.0);
 HTSetConversion("text/plain", "text/html",  HTPlainToHTML,	1.0, 0.0, 0.0);
 HTSetConversion("text/plain", "www/download", HTSaveToFile,	1.0, 0.0, 0.0);
 HTSetConversion("text/plain", "*", 	     HTPlainPresent,	1.0, 0.0, 0.0);
 HTSetConversion("text/html", "www/source",  HTPlainPresent, 	1.0, 0.0, 0.0);

 HTSetConversion("application/octet-stream","*",HTSaveToFile,1.0,3.0,0.0);
 HTSetConversion("application/x-compressed","*",HTSaveToFile,1.0,3.0,0.0);

 HTSetConversion("www/source",  "www/dump",     HTDumpToStdout, 1.0, 3.0, 0.0);
 HTSetConversion("*", 		"www/dump",     HTDumpToStdout, 1.0, 3.0, 0.0);
 HTSetConversion("www/source",  "www/download", HTSaveToFile ,  1.0, 3.0, 0.0);
 HTSetConversion("*", 		"www/download", HTSaveToFile ,  1.0, 3.0, 0.0);
 HTSetConversion("application/x-wais-source",
 		      "*",  	     HTWSRCConvert, 	1.0, 0.0, 0.0);
 HTSetConversion("application/x-wais-source",
 		      "www/source",  	     HTPlainPresent, 	1.0, 0.0, 0.0);
  /* save all unknown mime types to disk */
 HTSetConversion("www/source",  "www/present",  HTSaveToFile ,  1.0, 3.0, 0.0);
 HTSetConversion("*", 		"www/present",  HTSaveToFile ,  1.0, 3.0, 0.0);
 HTSetConversion("www/source",  "www/source",   HTSaveToFile ,  1.0, 3.0, 0.0);
 HTSetConversion("*", 		"www/source",   HTSaveToFile ,  1.0, 3.0, 0.0);
}



/*	Define a basic set of suffixes
**	------------------------------
**
**	The LAST suffix for a type is that used for temporary files
**	of that type.
**	The quality is an apriori bias as to whether the file should be
**	used.  Not that different suffixes can be used to represent files
**	which are of the same format but are originals or regenerated,
**	with different values.
*/

#ifndef NO_INIT
PUBLIC void HTFileInit NOARGS
{
    /* default suffix interpretation */
    HTSetSuffix("*","text/plain", "7bit", 1.0); 
    HTSetSuffix("*.*","text/plain", "7bit", 1.0); 

    /* Internal -- MIME is not recursive? */
    HTSetSuffix(".mime","www/mime", "8bit", 1.0); 

    	/* Jumping the gun a little */
    HTSetSuffix(".html","text/html", "8bit", 1.0);
    HTSetSuffix(".HTML","text/html", "8bit", 1.0);

#ifdef EXEC_SCRIPTS
 /* define these extentions for exec scripts
  */

	/* for csh exec links */
#ifndef VMS
    HTSetSuffix(".csh",	"application/x-csh", "8bit", 0.8);
    HTSetSuffix(".sh",	"application/x-sh", "8bit", 0.8);
    HTSetSuffix(".ksh",	"application/x-ksh", "8bit", 0.8);
#else
    HTSetSuffix(".com",	"application/x-VMS_script", "8bit", 0.8);
#endif /* not VMS */
#endif /* EXEC_SCRIPTS */

    /* don't worry about all the text/plain, because that is the default */
    HTSetSuffix(".txt", "text/plain", "7bit", 0.5);

    HTSetSuffix(".src",	"application/x-wais-source", "7bit", 1.0);

 if (getenv("DISPLAY")) {	/* Must have X11 */
    HTSetSuffix(".PS",	"application/postscript", "8bit", 0.8);
    HTSetSuffix(".eps",	"application/postscript", "8bit", 0.8);
    HTSetSuffix(".ai",	"application/postscript", "8bit", 0.5);	/* Adobe ill */
    HTSetSuffix(".ps",	"application/postscript", "8bit", 0.8);
    HTSetSuffix(".rtf",	"application/x-rtf", "8bit", 1.0);
    HTSetSuffix(".snd", "audio/basic", "binary", 1.0);

    HTSetSuffix(".gif", "image/gif", "binary", 1.0);
    HTSetSuffix(".GIF", "image/gif", "binary", 1.0);
    HTSetSuffix(".rgb", "image/x-rgb", "binary", 1.0);
    HTSetSuffix(".RGB", "image/x-rgb", "binary", 1.0);
    HTSetSuffix(".pict", "image/x-pict", "binary", 1.0);
    HTSetSuffix(".PICT", "image/x-pict", "binary", 1.0);

    HTSetSuffix(".xbm", "image/x-xbm", "binary", 1.0);
    HTSetSuffix(".XBM", "image/x-xbm", "binary", 1.0);

    HTSetSuffix(".tiff","image/x-tiff", "binary", 1.0);
    HTSetSuffix(".TIFF","image/x-tiff", "binary", 1.0);

    HTSetSuffix(".jpg", "image/jpeg", "binary", 1.0);
    HTSetSuffix(".JPG", "image/jpeg", "binary", 1.0);
    HTSetSuffix(".JPEG","image/jpeg", "binary", 1.0);
    HTSetSuffix(".jpeg","image/jpeg", "binary", 1.0);
    
    HTSetSuffix(".MPEG","video/mpeg", "binary", 1.0);
    HTSetSuffix(".mpg","video/mpeg", "binary", 1.0);
    HTSetSuffix(".MPG","video/mpeg", "binary", 1.0);
    HTSetSuffix(".mpeg","video/mpeg", "binary", 1.0);

} else {  /* no X11 */

    HTSetSuffix(".PS",	"application/octet-stream", "8bit", 0.8);
    HTSetSuffix(".eps",	"application/octet-stream", "8bit", 0.8);
    HTSetSuffix(".ai",	"application/octet-stream", "8bit", 0.5);/* Adobe ill */
    HTSetSuffix(".ps",	"application/octet-stream", "8bit", 0.8);
    HTSetSuffix(".rtf",	"application/octet-stream", "8bit", 1.0);
    HTSetSuffix(".snd", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".gif", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".GIF", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".rgb", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".RGB", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".xbm", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".XBM", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".tiff","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".TIFF","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".jpg", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".JPG", "application/octet-stream", "binary", 1.0);
    HTSetSuffix(".JPEG","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".jpeg","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".MPEG","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".mpg","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".MPG","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".mpeg","application/octet-stream", "binary", 1.0);
} /* end if X11 */

    HTSetSuffix(".MOV","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".mov","application/octet-stream", "binary", 1.0);

    HTSetSuffix(".hqx","application/octet-stream", "binary", 1.0);
    HTSetSuffix(".HQX","application/octet-stream", "binary", 1.0);

    HTSetSuffix(".bin",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".BIN",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".exe",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".EXE",	"application/octet-stream", "binary", 1.0);	
    HTSetSuffix(".tar",	"application/octet-stream", "binary", 1.0);

    HTSetSuffix(".Z",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".gz",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".ZIP",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".zip",	"application/octet-stream", "binary", 1.0);
    
    HTSetSuffix(".lzh",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".LZH",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".lha",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".LHA",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".dms",	"application/octet-stream", "binary", 1.0);
    HTSetSuffix(".DMS",	"application/octet-stream", "binary", 1.0);
}
#endif /* NO_INIT */

