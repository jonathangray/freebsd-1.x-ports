/*
 *   OUTPATH is where to send the output.  If you want a .ps file to
 *   be created by default, set this to "".  If you want to automatically
 *   invoke a pipe (as in lpr), make the first character an exclamation
 *   point or a vertical bar, and the remainder the command line to
 *   execute.
 *
 *   Actually, OUTPATH should be overridden by an `o' line in config.ps.
 */
#define OUTPATH ""

/*
 *   Names of config and prologue files:
 */
#ifdef MSDOS
#define DVIPSRC "dvips.ini"
#else
#ifdef VMCMS  /* IBM: VM/CMS */
#define DVIPSRC "dvips.profile"
#else
#define DVIPSRC ".dvipsrc"
#endif  /* IBM: VM/CMS */
#endif

#define HEADERFILE "tex.pro"
#define CHEADERFILE "texc.pro"
#define PSFONTHEADER "texps.pro"
#define IFONTHEADER "finclude.pro"
#define SPECIALHEADER "special.pro"
#define COLORHEADER "color.pro"  /* IBM: color */
#define CROPHEADER "crop.pro"
#define PSMAPFILE "psfonts.map"
#ifndef CONFIGFILE
#define CONFIGFILE "config.ps"
#endif

/* Environment variable lists. */
#define DVIPS_HEADER_ENVS "DVIPSHEADERS", NULL
#define DVIPS_PICT_ENVS "TEXPICTS", KPSE_TEX_ENVS
