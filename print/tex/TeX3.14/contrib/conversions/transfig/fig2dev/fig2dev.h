#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif

#define round(x)	((int) ((x) + ((x >= 0)? 0.5: -0.5)))

/* 
 * Device driver interface structure
 */
struct driver {
 	void (*option)();	/* interpret driver-specific options */
  	void (*start)();	/* output file header */
	void (*arc)();		/* object generators */
	void (*ellipse)();
	void (*line)();
	void (*spline)();
	void (*text)();
	void (*end)();		/* output file trailer */
  	int text_include;	/* include text length in bounding box */
#define INCLUDE_TEXT 1
#define EXCLUDE_TEXT 0
};

extern char *strchr();

extern char Err_badarg[];
extern char Err_incomp[];
extern char Err_mem[];

extern char *PSfontnames[];

extern char	*prog, *from;
extern int	font_size;
extern double	mag;
extern FILE	*tfp;

extern int llx, lly, urx, ury, coord_system;

extern void gendev_null();
