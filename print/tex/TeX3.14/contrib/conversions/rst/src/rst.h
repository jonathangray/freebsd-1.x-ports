/*
 * RST file format, version 0
 * (as documented in old Imprint-10 Programmer's Manual)
 *
 * An RST file consists of the following sections:
 *
 *	filemark	8 bytes (contains the word "Rast")
 *	preamble	variable length but at least 40 bytes
 *	glyph directory		usually 127 entries
 *	raster data
 */

#define	FILEMARK_SIZE	8	/* bytes */
#define	MAX_PREAMBLE_STRING	256	/* really unknown limit */
#define	PREAMBLE_FIXED_PART	36	/* up to variable length strings */
#define	GLYPH_ENTRY_SIZE	15

typedef	char	ONEB;
typedef	short	TWOB;
typedef	long	THREEB;
typedef	long	FOURB;

/*
 * The Preamble
 *
 * Value fields can be 1, 2, 3, or 4 bytes long, and
 * are stored with the most signifigant byte first.
 *
 * Character strings are of variable length; they are stored
 * as a one byte count and then the string of characters
 * (don't believe the documentation that says the strings are
 * null terminated - it lies!).
 */

struct rst_dsc {
	int	s_l;				/* length of string */
	char	s_p[MAX_PREAMBLE_STRING];	/* the string itself */
};

struct rst_preamble {
	TWOB	rp_bytes;		/* bytes in preamble (excluding self) */
	ONEB	rp_version;		/* current zero */
	THREEB	rp_glyphptr;		/* offset to glyph directory */
	TWOB	rp_firstglyph;		/* usually 0 */
	TWOB	rp_lastglyph;		/* usually 127 */
	FOURB	rp_fontmag;		/* magnification in units of .001 */
	FOURB	rp_designsize;		/* design size in fix units */
	FOURB	rp_interline;		/* 0 defaults to designsize*1.2 */
	FOURB	rp_interword;		/* 0 defaults to designsize/1.2 */
	TWOB	rp_rotation;		/* counter clockwise degrees 0..359 */
	ONEB	rp_charadvance;		/* 0 to right, 1 downward, 2 left */
	ONEB	rp_lineadvance;		/* 3 upwards */
	FOURB	rp_checksum;		/* 0 for no check */
	TWOB	rp_fontres;		/* in pixels per inch */
	/* variable length fields here (C structures can't handle this) */
	struct rst_dsc	rp_fontident;	/* name of font */
	struct rst_dsc	rp_facetype;	/* ??? */
	struct rst_dsc	rp_outdevice;	/* set to 'ImPrint-10' */
	struct rst_dsc	rp_creator;	/* name of user/origin */
};

/*
 * The Glyph Directory
 */
struct rst_glyph_entry {		/* A directory entry contains	*/
	TWOB	rg_h;	/* height */
	TWOB	rg_w;	/* width  */
	TWOB	rg_y;	/* distance from top  */
	TWOB	rg_x;	/* distance from left */
	FOURB	rg_width;	/* advance width */
	THREEB	rg_offset;	/* offset to raster data in file/memory */
};

/* The Raster Data */
/* End of RST file */
