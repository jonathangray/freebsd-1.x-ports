/* wn.h - public header file for the wn library */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)wn.h	1.20 25/4/92 (UKC) */

#ifndef WN_H_INCLUDED
#define WN_H_INCLUDED

/* Define wn__PROTO as in gendefs.h.  We don't mandate gendefs.h for
 * wn.h, so we have to define wn__PROTO here.
 */
#ifdef __STDC__
#define wn__PROTO(x) x
#else
#define wn__PROTO(x) ()
#define const
#endif /* !__STDC__ */

/* --- Names for various constants, and public structure definitions.
 */

/*  Value for wn_next_event() to say get event from any window
 */
#define WN_ANY (-1)

/*  Typedef for a window handle.
 */
typedef int window_t;

/*  Typedef for the opaque handle returned by wn_create_cursor()
 */
typedef long cursor_t;

/*  A special name for window 0, for the convenience of programs
 *  which only use the first window.
 */
#define WN_STDWIN	0

typedef int *mdfont_t;

/*  Modes for wn_set_replay_cursor_mode().
 */
typedef enum {
	WN_RP_NO_FEEDBACK,		/* no cursor feedback */
	WN_RP_WARP_MOUSE		/* warp the mouse */
} wn_replay_cursor_mode_t;

/*  Element in an array of structures used for batched subwindow creation
 *  Used in wn_create_subwin_batch().
 */
typedef struct batchwinst {
	int wb_wn;
	short wb_x;
	short wb_y;
	short wb_width;
	short wb_height;
	short wb_type;
} batchwin_t;

/*  Types of windows for wn_create_subwin().
 */
#define WN_INPUT_OUTPUT	0
#define WN_OUTPUT_ONLY	1

/*  Public structure defining a colormap entry.
 */
typedef struct colorst {
	short co_pixel;
	unsigned short co_red;
	unsigned short co_green;
	unsigned short co_blue;
} color_t;
#define colour_t	color_t

/*  Would like to call this colormap_t but Suntools has already used the name.
 */
typedef struct colormapst {
	color_t *cm_colors;
	int cm_size;
} cmap_t;

#define cm_colours	cm_colors

/*  The public structure describing a font
 */
typedef struct fontst {
	short ft_is_fixed_width;
	short ft_baseline;	/* offset of baseline from top of char */
	short ft_width;
	short ft_height;
	short ft_nchars;
	short *ft_width_tab;
	mdfont_t ft_mdfont;	/* machine dependent font ptr - don't use! */
} font_t;

/*  Alternative name for the ft_height field - include for backwards
 *  compatability only.
 */
#define ft_depth ft_height

/*  Macro to flip the bits in a short, copied from ~X/libsun/font.c
 */
extern unsigned short _wn_byteflip_tab[];
#define WN_FLIP_SHORT(s)	((_wn_byteflip_tab[(s) & 0xff]<<8) | \
				  _wn_byteflip_tab[((s)>>8) & 0xff])

/*  The public structure describing a bitmap.
 */
typedef struct bitmapst {
	unsigned short *bm_data;
	char bm_bit_order;
	char bm_byte_order;
	char bm_pixel_format;
	unsigned char bm_flags;
	unsigned char bm_pflags; /* private to wn */
	short bm_width;
	short bm_height;
	short bm_nplanes;
	short bm_xhot;
	short bm_yhot;
	short bm_lineinc;	/* # chars between lines - volatile */
	cmap_t *bm_colormap;	/* NULL for bitmaps with no colormap */
	long bm_cache_data;	/* used internally - see documentation */
} bitmap_t;

#define bm_colourmap	bm_colormap

/*  Special value for the lineinc argument of wn_set_bitmap_format() meaning
 *  set the lineinc to the right value for the machine.
 */
#define BM_NATURAL_LINEINC	(-1)

/*  Values for bm_bit_order
 */
#define BM_BIT0_LEFT		 'l'	/* bit 0 (lsb) is leftmost in image */
#define BM_BIT0_RIGHT		 'r'	/* bit 0 (lsb) is rightmost in image */
#define BM_NATURAL_BIT_ORDER	 'n'	/* `natural' order for machine */

/*  Values for bm_byte_order
 */
#define BM_MSB_FIRST		 'm'	/* Big endian (e.g. MC68020) */
#define BM_LSB_FIRST		 'l'	/* Little endian (e.g. VAX) */
#define BM_NATURAL_BYTE_ORDER	 'n' 	/* 'natural' order for this machine */

/*  Special value for bm_byte_order meaning that this bitmap is in standard
 *  machine byte order, whatever that is.  This wil be true, for example,
 *  when a static bitmap is initialised from an array of shorts.
 */
#define BM_MACHINE_BYTE_ORDER	 'M'

/*  Values for bm_pixel_format
 */
#define BM_XY_PIXELS		 'p'	/* bitmap has seperate planes */
#define BM_BYTE_PIXELS		 'b'	/* one complete 8 bit pixel per byte */
#define BM_NATURAL_PIXEL_FORMAT	 'n'	/* `natural' pixel order for machine */

/*  Flags in bm_flags.
 */
#define BM_CAN_FREE_DATA	 0x1	/* data was obtained via malloc() */
#define BM_CAN_FREE_HEADER	 0x2	/* bitmap was obtained via malloc() */
#define BM_CHOOSE_FORMAT       	 0x4	/* go to natural format on first use */
#define BM_HAS_HOTSPOT	         0x8	/* bitmap has a hot spot */

/*  Macro to create a static bitmap given the data and the dimensions.
 *  Used in lines like "static my_bitmap = wn_make_static_bm(...);
 */
#define wn_make_static_bm(w, h, np, xh, yh, bit_o, byte_o, pixfmt, lineinc, data) \
	{ data, bit_o, byte_o, pixfmt, BM_CHOOSE_FORMAT, 0, \
					w, h, np, xh, yh, lineinc, 0 }

/*  Old interface to wn_make_static_bm() for backwards compatibility.
 */
#define wn_static_bm(width, height, nplanes, xhot, yhot, bit_order, data) \
	wn_make_static_bm(width, height, nplanes, xhot, yhot, \
				bit_order, BM_MACHINE_BYTE_ORDER, BM_XY_PIXELS, \
				((width + 15) / 16) * 2, data)

/*  Old interfaces to the bitmap creation routines, for backwards
 *  compatibility.
 */
#define wn_new_bitmap(width, height, nplanes, bit_order) \
	wn_make_bitmap(width, height, nplanes, bit_order, BM_XY_PIXELS)

#define wn_data_to_bitmap(width, height, nplanes, bit_order, data, lineinc) \
	wn_make_bitmap_from_data(width, height, nplanes, data, \
					bit_order, BM_XY_PIXELS, (lineinc) * 2)

#define wn_set_bitmap_format(bm, bit_order, pixel_format, lineinc) \
	wn_change_bitmap_format(bm, bit_order, (bm)->bm_byte_order, \
							pixel_format, lineinc)

/*  A bunch of macros for manipulating the bits of single plane bitmaps.
 */
#define wn_xy_to_short(bm, x, y) ((bm)->bm_data + (y) * ((bm)->bm_lineinc >> 1) + \
									((x)>>4))

#define wn_lbitmask(x) (1 << (x & 0xf))
#define wn_rbitmask(x) ((1 << 15) >> (x & 0xf))

#define wn_lgetbit(bm, x, y) ((*wn_xy_to_short(bm, x, y) & wn_lbitmask(x)) != 0)
#define wn_rgetbit(bm, x, y) ((*wn_xy_to_short(bm, x, y) & wn_rbitmask(x)) != 0)

#define wn_getbit(bm, x, y) \
	(((bm)->bm_bit_order == BM_BIT0_RIGHT) ? wn_rgetbit(bm, x, y) : \
					         wn_lgetbit(bm, x, y))

#define wn_lsetbit_to_1(bm, x, y) (*wn_xy_to_short(bm, x, y) |= wn_lbitmask(x))
#define wn_rsetbit_to_1(bm, x, y) (*wn_xy_to_short(bm, x, y) |= wn_rbitmask(x))

#define wn_setbit_to_1(bm, x, y) \
	(((bm)->bm_bit_order == BM_BIT0_RIGHT) ? wn_rsetbit_to_1(bm, x, y) : \
					         wn_lsetbit_to_1(bm, x, y))

#define wn_lsetbit_to_0(bm, x, y) (*wn_xy_to_short(bm, x, y) &= ~wn_lbitmask(x))
#define wn_rsetbit_to_0(bm, x, y) (*wn_xy_to_short(bm, x, y) &= ~wn_rbitmask(x))

#define wn_setbit_to_0(bm, x, y) \
	(((bm)->bm_bit_order == BM_BIT0_RIGHT) ? wn_rsetbit_to_0(bm, x, y) : \
					         wn_lsetbit_to_0(bm, x, y))

#define wn_setbit(bm, x, y, val) \
	((val != 0) ? wn_setbit_to_1(bm, x, y) : wn_setbit_to_0(bm, x, y))

/*  Element in an array of cursor definitions.  See wn_make_bmc_cursor() below.
 */
typedef struct bmcursorst {
	bitmap_t bmc_cursor;
	bitmap_t bmc_mask;
	cursor_t bmc_cid;
} bmcursor_t;

/*  Macro to concatenate two names in a way that doesn't annoy lint.
 *  Copied from a Sun header file.
 */
#ifdef __STDC__
#define _WN_CAT(a,b)	a ## b
#else
#define _WN_IDENT(a) a
#define _WN_CAT(a,b) _WN_IDENT(a)b
#endif /* !__STDC__ */

/*  Convenience macro to construct an element in a bmcursor_t array from
 *  a cursor and mask bitmap.
 */
#define wn_make_bmc_cursor(curs,mask,bit_order,byte_order,lineinc) \
	wn_make_static_bm(_WN_CAT(curs,_width), _WN_CAT(curs,_height), 1, \
			  _WN_CAT(curs,_x_hot), _WN_CAT(curs,_y_hot), \
			  bit_order, byte_order, BM_XY_PIXELS, lineinc, \
			  (unsigned short *)_WN_CAT(curs,_bits)), \
	wn_make_static_bm(_WN_CAT(mask,_width), _WN_CAT(mask,_height), 1, \
			  _WN_CAT(mask,_x_hot), _WN_CAT(mask,_y_hot), \
			  bit_order, byte_order, BM_XY_PIXELS, lineinc, \
			  (unsigned short *)_WN_CAT(mask,_bits)), \
	0

#define wn_make_x11_bmc_cursor(curs,mask) \
	wn_make_bmc_cursor(curs,mask,BM_BIT0_LEFT,BM_LSB_FIRST,\
			   ((_WN_CAT(curs,_width) + 15) / 16) * 2)

/*  Old names for wn_*_image() functions, for backwards compatibility.
 */
#define wn_rop_to_mem		wn_get_image
#define wn_rop_mem_to_mem	wn_copy_image
#define wn_rop_from_mem(bm, sx, sy, width, height, wn, dx, dy, ropfunc) \
	wn_put_image(bm, sx, sy, width, height, wn, dx, dy, ropfunc, WN_FG, WN_BG)

/*  Macros for drawing lines in bitmaps.
 */
#define wn_bm_hline(bm, x, y, width, rfunc) \
	wn_copy_image((bm), (x), (y), (width), 1, (bm), (x), (y), rfunc)

#define wn_bm_vline(bm, x, y, height, rfunc) \
	wn_copy_image((bm), (x), (y), 1, (height), (bm), (x), (y), rfunc)

/*  The values of the background and foreground pixel values.
 *  Should be used only via the WN_FG and WN_BG macros.
 */
extern int _wn_Fg_pixel, _wn_Bg_pixel;

/*  Special color values meaning foreground and background.
 *
 *  The casts to int are to prevent these being accidentally used
 *  as lvalues.
 */
#define WN_FG		((int)_wn_Fg_pixel)
#define WN_BG		((int)_wn_Bg_pixel)

/*  A colour value meaning invert the foreground and background colors.
 *  Using this has unpredictable effects on pixels of other colors, but
 *  repeating the operation will restore the original colors of all pixels.
 */
#define WN_INVERT	(-1)

/*  A special color for the background color for wn_text meaning don't
 *  disturb the background.
 */
#define WN_TRANSPARENT	(-2)

/*  Old names for WN_FG and WN_BG and WN_INVERT - kept for backwards compatibility.
 */
#define WHITE	WN_BG
#define BLACK	WN_FG
#define INVERT	WN_INVERT

/*  Rop function values.
 */
#define R_RPL		0
#define R_NOT		1
#define R_AND		2
#define	R_ANDNOT	3
#define	R_OR		4
#define	R_ORNOT		5
#define	R_XOR		6
#define	R_XNOR		7

/*  Shades for wn_shade_area
 */
#define WN_GREY0	0x0000
#define WN_GREY2	0x8020
#define WN_GREY4	0x8282
#define WN_GREY6	0xa4a1
#define WN_GREY8	0xa5a5
#define WN_GREY10	(~WN_GREY6)
#define WN_GREY12	(~WN_GREY4)
#define WN_GREY14	(~WN_GREY2)

/*  Shade area grey - shade is OR'ed onto the window
 *  Defined for backwards compatibility.
 */
#define wn_shade(wn, x, y, width, height) \
		wn_shade_area(wn, x, y, width, height, WN_GREY4, R_OR);


/*  Old name for wn_mono_rop.
 */
#define wn_copy_area(wn, sx, sy, w, h, dx, dy, func) \
		wn_mono_rop(wn, sx, sy, w, h, dx, dy, func)

#define wn_bold(wn,x,y,width,height) \
		wn_mono_rop(wn, x, y, width-1, height, x+1, y, R_OR)

#define wn_invert_area(wn,x,y,width,height) \
		wn_mono_rop(wn, x, y, width, height, x, y, R_NOT)
 
/*  Event structure and flags.
 */
typedef struct eventst {
	unsigned long ev_type;	/* type of event */
	char ev_flags;		/* flags (e.g. which button went up or down) */
	char ev_char;		/* char for EV_KEY events */
	short ev_x;		/* mouse x coord */
	short ev_y;		/* mouse y coord */
	short ev_buttons;	/* mouse button state */
	long ev_time;		/* timestamp */
	int ev_wn;		/* window the event happened in */
	int ev_fdmask;		/* input fds mask for EV_OTHER_INPUT events */
} event_t;

/*  Special key values.  We would like to make these >256 but ev_char
 *  is (stupidly) a char and it's too late to change that.  Instead
 *  we use values in the range 128..255 (this of course makes wn
 *  applications non 8 bit transparent.
 */
#define WN_CH_LEFT_ARROW	150
#define WN_CH_RIGHT_ARROW	151
#define WN_CH_UP_ARROW		152
#define WN_CH_DOWN_ARROW	153
#define WN_CH_FUNCKEY(n)	(160 + (n))

/*  Mouse buttons.  In ev_buttons, these are OR'ed together to represent the
 *  current mouse state.  In ev_flags, only one is ever set, saying which
 *  button changed state in an EV_BUTTON_PRESSED or EV_BUTTON_RELEASED event.
 */
#define B_LEFT		 01
#define B_MIDDLE	 02
#define B_RIGHT		 04

/*  In an EV_BUTTON_UP or EV_BUTTON_DOWN event, these bits are set
 *  in ev_buttons if the corresponding key is down (if the window
 *  system supports this.
 *
 *  Note that these values overlap a couple of EV_ values.
 *  This is partly because they postdate them and I didn't want
 *  to change the EV_* values for fear of breaking existing code.
 *  I can't tack them on the end because the unused bit values
 *  won't fit in ev_buttons (a short).
 *
 *  Some of the backwards compatibility stuff folds ev_buttons
 *  and ev_flags together, so we pick values for shift and control
 *  that are unlikely to cause conflicts.
 */
#define B_SHIFT_KEY	 02000	/* Shift key pressed */
#define B_CONTROL_KEY	 04000	/* Control key pressed */

#define EV_WAS_PUSHED_BACK	010	/* This event has was pushed back */

/*  Event types.
 *
 *  When these appear in ev_type, they are mutually exclusive - i.e. no more
 *  than one of them is ever set. In an event mask they can be OR'ed together.
 */
#define EV_KEY			020	/* Keyboard key pressed */
#define EV_BUTTON_DOWN		040	/* Mouse button pressed */
#define EV_BUTTON_UP	       0100	/* Mouse button released */
#define EV_MOUSE_MOVED	       0200	/* Mouse moved (perhaps not really) */
#define EV_WINDOW_RESIZED      0400	/* Window changed size */
#define EV_WINDOW_EXPOSED     01000	/* Window exposed (needs repainting) */
#define EV_WINDOW_SELECTED    02000	/* User selected window */
#define EV_WINDOW_DESELECTED  04000	/* User deselected window */
#define EV_INTERRUPT	     010000	/* Interrupt occured (*not* a resize) */
#define EV_OTHER_INPUT	     020000	/* Input pending on other fds */
#define EV_SELECTION_REQUEST 040000	/* Selection requested */
#define EV_LOST_SELECTION   0100000	/* Selection lost */
#define EV_OTHER	    0200000	/* Unknown event type */

/*  Combinations of events
 */
#define EV_BUTTONS	(EV_BUTTON_UP | EV_BUTTON_DOWN)

/*  Alternative names for the event types, included mostly for backwards
 *  compatibility.
 */
#define B_STATE_CHANGE	(EV_BUTTON_UP | EV_BUTTON_DOWN)
#define B_UNGOT		EV_WAS_PUSHED_BACK
#define B_NEW_WIN_SIZE	(EV_WINDOW_RESIZED | EV_WINDOW_EXPOSED)
#define B_KEYBOARD	EV_KEY
#define B_INTR		EV_INTERRUPT

/*  Old names for the mouse buttons.  Derived from the meanings of
 *  the buttons in the spy screen editor.
 */
#define B_SELECT	 B_LEFT		/* WB_A (yellow) */
#define B_COPY		 B_MIDDLE	/* WB_B (white)  */
#define B_EXTEND	 B_RIGHT	/* WB_C (blue)   */

/*  Any mouse button
 */
#define B_ANY		(B_LEFT | B_MIDDLE | B_RIGHT)

/*  Input modes for wn_inmode
 */
#define WN_REQUEST   1
#define WN_SAMPLE    2
#define WN_NOINPUT   3
#define _WN_MININPUT 4

/*  More convenient fd mask manipulation routines.
 */
#define wn_add_to_fd_mask(fd)	    (wn_set_fd_mask(wn_get_fd_mask() | (1 << (fd)))
#define wn_remove_from_fd_mask(fd)  (wn_set_fd_mask(wn_get_fd_mask() & ~(1 << (fd)))

/*  Values of use_which for wn_xputs()
 */
#define WN_USE_BASELINE 0
#define WN_USE_TOP	1

/*  Window manager types returned by wn_get_wm_type()
 */
#define WN_PERQ		0		/* No longer supported */
#define WN_SUNVIEW	1
#define WN_MG		2		/* No longer supported */
#define WN_X10		3		/* No longer supported */
#define WN_X11		4

/*  Old name for WN_X10
 */
#define WN_XWINDOWS	WN_X10		/* No longer supported */

/*  Interfaces to wn_xtext()
 */

/*  wn_xtext() is not documented - the following two #defines are the
 *  official Wn text output functions.
 */
#define wn_text(wn, font, s, x, y, fg_color, bg_color, use_which) \
	wn_xtext(wn, font, s, x, y, R_RPL, fg_color, bg_color, use_which, 0)

#define wn_mono_text(wn, font, s, x, y, ropfunc, use_which) \
	wn_xtext(wn, font, s, x, y, ropfunc, WN_FG, WN_BG, use_which, 1)


/*  Simpler interfaces to wn_text.
 */
#define wn_ttext(wn, s, x, y, fg_color, bg_color) \
	wn_text(wn, (font_t *)NULL, s, x, y, fg_color, bg_color, WN_USE_TOP)  

#define wn_btext(wn, s, x, y, fg_color, bg_color) \
	wn_text(wn, (font_t *)NULL, s, x, y, fg_color, bg_color, WN_USE_BASELINE)

#define wn_xputs(wn, font, s, x, y, func, use_which) \
	wn_xtext(wn, font, s, x, y, func, WN_FG, WN_BG, use_which, 0)

#define wn_bputs(wn,s,x,y)	wn_btext(wn, s, x, y, WN_FG, WN_BG)

#define wn_tputs(wn,s,x,y)	wn_ttext(wn, s, x, y, WN_FG, WN_BG)

/*  Old name for wn_bputs
 */
#define wn_puts			wn_bputs


#define wn_strwidth(s, font)	wn_strnwidth((s), -1, (font))

/* --- Special cursors.
 */

/*  Special cursor types for wn_spcu().
 */
#define WN_SC_OFF	0
#define WN_SC_RECT	1
#define WN_SC_LINE	2
#define WN_SC_CROSS	3

#define WN_SC_TYPE	07

/*  Flags to be or'ed into the cursor type for wn_spcu().
 */
#define WN_SC_X1REL	 010
#define WN_SC_X2REL	 020
#define WN_SC_Y1REL	 040
#define WN_SC_Y2REL	0100

#define WN_SC_P1REL	(WN_SC_X1REL | WN_SC_Y1REL)
#define WN_SC_P2REL	(WN_SC_X2REL | WN_SC_Y2REL)

#define WN_SC_FLAGS	(WN_SC_P1REL | WN_SC_P2REL)

/*  Common combinations of type and flags.
 */
#define WN_SC_FRAME	(WN_SC_RECT | WN_SC_P1REL | WN_SC_P2REL)
#define WN_SC_RBOX	(WN_SC_RECT | WN_SC_P1REL)
#define WN_SC_RLINE	(WN_SC_LINE | WN_SC_P1REL)
#define WN_SC_XHAIR	(WN_SC_CROSS | WN_SC_P1REL)

/*  Common special cursor calls.
 */
#define wn_sc_rbox(wn,x,y)	wn_spcu((wn),WN_SC_RBOX,0,0,(x),(y))
#define wn_sc_rline(wn,x,y)	wn_spcu((wn),WN_SC_RLINE,0,0,(x),(y))
#define wn_sc_hline(wn,x)	wn_spcu((wn),WN_SC_RLINE | WN_SC_Y2REL,0,0,(x),0)
#define wn_sc_vline(wn,y)	wn_spcu((wn),WN_SC_RLINE | WN_SC_X2REL,0,0,0,(y))
#define wn_sc_off(wn)		wn_spcu((wn),WN_SC_OFF,0,0,0,0)

/*  English names for the color routines
 */
#define wn_get_pixels_by_colour	wn_get_pixels_by_color
#define wn_set_pixel_colours	wn_set_pixel_colors
#define wn_get_pixel_colours	wn_get_pixel_colors
#define wn_parse_colour		wn_parse_color
#define wn_use_colour_hint	wn_use_color_hint

/*  Typedefs for function pointers
 */
typedef void (*wn_abort_func_t)wn__PROTO((void));
typedef void (*wn_deiconise_func_t)wn__PROTO((int wn));
typedef void (*wn_draw_icon_func_t)wn__PROTO((int wn));
typedef int (*wn_event_handler_func_t)wn__PROTO((event_t *ev));

/*  Old name for wn_get_window_handle(), kept for backwards compatibility.
 */
#define wn_menu_id(wn)	wn_get_window_handle(wn)
 	
/* --- Prototype declarations for the routines
 */

/* colors */
void wn_use_color_hint wn__PROTO((int use_color));
void wn_npixels_hint wn__PROTO((int npixels));
int wn_get_pixels_by_color wn__PROTO((color_t *colors, int ncolors));
int wn_get_pixels wn__PROTO((color_t *colors, int ncolors));
int wn_get_pixels_and_planes wn__PROTO((int npixels, int nplanes, int contig, int *pixels, int *p_planes));
void wn_set_pixel_colors wn__PROTO((color_t *colors, int ncolors));
void wn_get_pixel_colors wn__PROTO((color_t *colors, int ncolors));
void wn_free_pixels wn__PROTO((color_t *colors, int ncolors));
int wn_parse_color wn__PROTO((const char *name, color_t *color));

/* fonts */
void wn_set_sysfont wn__PROTO((font_t *font));
font_t *wn_get_sysfont wn__PROTO((void));
int wn_add_font_path wn__PROTO((const char *path));
font_t *wn_open_font wn__PROTO((const char *fontfile));
void wn_close_font wn__PROTO((font_t *font));
char **wn_list_fonts wn__PROTO((const char *pattern, int maxcount, int *p_count));
void wn_free_font_names wn__PROTO((char **names));
font_t *wn_install_mdfont wn__PROTO((mdfont_t mdfont));
void wn_xtext wn__PROTO((int wn, font_t *font, const char *s, int x, int y, int ropfunc, int fg_color, int bg_color, int use_which, int mono));

/* bitmaps */
bitmap_t *wn_make_bitmap wn__PROTO((int width, int height, int nplanes, int bit_order, int pixel_format));
bitmap_t *wn_make_bitmap_from_data wn__PROTO((int width, int height, int nplanes, unsigned short *data, int bit_order, int pixel_format, int lineinc));
void wn_free_bitmap wn__PROTO((bitmap_t *bm));
void wn_change_bitmap_format wn__PROTO((bitmap_t *bm, int bit_order, int byte_order, int pixel_format, int lineinc));

/* rasterops */
void wn_rop wn__PROTO((int wn, int x, int y, int width, int height, int new_x, int new_y));
void wn_mono_rop wn__PROTO((int wn, int x, int y, int width, int height, int new_x, int new_y, int ropfunc));
int wn_last_rop_was_damaged wn__PROTO((int wn));
void wn_tile_area wn__PROTO((int wn, int x, int y, int width, int height, bitmap_t *bm, int ropfunc));
void wn_shade_area wn__PROTO((int wn, int x, int y, int width, int height, int orig_shade, int ropfunc));
void wn_move_area wn__PROTO((int wn, int x, int y, int width, int height, int new_x, int new_y, int colour));
void wn_set_area wn__PROTO((int wn, int x, int y, int width, int height, int colour));

/* rasterops between window and off screen memory */
void wn_get_image wn__PROTO((int wn, int sx, int sy, int width, int height, bitmap_t *bm, int dx, int dy, int ropfunc));
void wn_put_image wn__PROTO((bitmap_t *bm, int sx, int sy, int width, int height, int wn, int dx, int dy, int ropfunc, int fg, int bg));
void wn_copy_image wn__PROTO((bitmap_t *sbm, int sx, int sy, int width, int height, bitmap_t *dbm, int dx, int dy, int ropfunc));

/* quick saving and restoring of areas of a window */
long wn_save_area wn__PROTO((int wn, int x, int y, int width, int height));
void wn_restore_area wn__PROTO((long lsa));
void wn_free_area wn__PROTO((long lsa));

/* lines */
void wn_draw_line wn__PROTO((int wn, int x1, int y1, int x2, int y2, int colour));
void wn_invert_line wn__PROTO((int wn, int x1, int y1, int x2, int y2));
void wn_box_round wn__PROTO((int wn, int x, int y, int width, int height, int colour));
void wn_invert_box wn__PROTO((int wn, int x, int y, int width, int height));

/* cursors */
cursor_t wn_create_cursor wn__PROTO((bitmap_t *cbm, bitmap_t *mask_bm));
cursor_t wn_get_window_cursor wn__PROTO((int wn));
void wn_define_cursor wn__PROTO((int wn, cursor_t cid));
void wn_free_cursor wn__PROTO((cursor_t cid));
void wn_spcu wn__PROTO((int wn, unsigned type_and_flags, int x1, int y1, int x2, int y2));

/* old cursor functions - kept for backwards compatibility */
void wn_combine_cursors wn__PROTO((bitmap_t *dcbm, bitmap_t *scbm, int ropfunc));
void wn_print_cursor wn__PROTO((int wn, bitmap_t *cbm, int x, int y, int ropfunc));
bitmap_t *wn_set_cursor wn__PROTO((int wn, bitmap_t *cbm));
bitmap_t *wn_get_cursor wn__PROTO((int wn));

/* creation and destruction of windows */
void wn_suggest_window_position wn__PROTO((int xpos, int ypos));
void wn_suggest_window_size wn__PROTO((int width, int height));
void wn_set_classname wn__PROTO((const char *name));
const char *wn_open_display wn__PROTO((void));
int wn_open_stdwin wn__PROTO((void));
int wn_create_window wn__PROTO((const char *name));
int wn_from_wid wn__PROTO((int wid));
int wn_install_wid wn__PROTO((long wid));
int wn_create_subwin wn__PROTO((int parwn, int x, int y, int width, int height, int type));
void wn_close_window wn__PROTO((int wn));

/* icons */
void wn_iconise wn__PROTO((int wn));
int wn_deiconise wn__PROTO((int wn));
int wn_is_iconised wn__PROTO((int wn));
int (*wn_set_deiconise_func wn__PROTO((int wn, int (*func)(int fwn))))wn__PROTO((int rwn));
void wn_configure_icon_window wn__PROTO((int wn, int x, int y, int width, int height));
void (*wn_set_draw_icon_func wn__PROTO((int wn, void (*func)(int fwn))))wn__PROTO((int rwn));

/* oddments */
int wn_munge_args wn__PROTO((int argc, const char **argv));
const char **wn_unmunge_args wn__PROTO((const char **args, int pos));
int wn_get_wm_type wn__PROTO((void));
const char *wn_get_default wn__PROTO((const char *name));
void wn_bell wn__PROTO((int wn));
int wn_strnwidth wn__PROTO((const char *s, int nchars, font_t *font));
int wn_strpos wn__PROTO((const char *s, int x, font_t *font, int halfshift));
wn_abort_func_t wn_set_abort_func wn__PROTO((int wn, wn_abort_func_t func));
const char *wn_version wn__PROTO((void));

/* recording and replaying of events */
int wn_set_record_file wn__PROTO((const char *filename));
int wn_set_replay_file wn__PROTO((const char *filename));
void wn_set_replay_cursor_mode wn__PROTO((wn_replay_cursor_mode_t replay_cursor_mode));
int wn_record_event wn__PROTO((const event_t *ev));
int wn_get_recorded_event wn__PROTO((event_t *ev));

/* selection handling */
void wn_set_selection wn__PROTO((const char *buf, int nbytes));
void wn_get_selection wn__PROTO((const char **p_buf, int *p_nbytes));

/* getting window attributes */
void wn_get_window_size wn__PROTO((int wn, int *p_width, int *p_height));
const char *wn_get_window_name wn__PROTO((int wn));
long wn_get_win_data wn__PROTO((int wn));
int wn_get_width wn__PROTO((int wn));
int wn_get_height wn__PROTO((int wn));
int wn_get_nplanes wn__PROTO((void));
int wn_is_open wn__PROTO((int wn));
long wn_get_window_handle wn__PROTO((int wn));

/* setting window attributes */
long wn_set_win_data wn__PROTO((int wn, long data));
void wn_change_win wn__PROTO((int wn, int parwn, int x, int y, int width, int height));
void wn_swap_wins wn__PROTO((int wn1, int wn2));
void wn_adjust_win_size wn__PROTO((window_t wn, int x_delta, int y_delta, int w_delta, int h_delta));
void wn_set_win_size wn__PROTO((window_t wn, int width, int height));

/* coordinate transformation */
int wn_trans_coords wn__PROTO((int oldwn, int x, int y, int newwn, int *p_x, int *p_y));

/* batching graphical output */
void wn_updating_off wn__PROTO((int wn));
void wn_updating_on wn__PROTO((int wn));
void wn_show_updates wn__PROTO((int wn));

/* input */
long wn_get_wn_fds wn__PROTO((void));
long wn_get_fd_mask wn__PROTO((void));
void wn_set_fd_mask wn__PROTO((long mask));
int wn_inmode wn__PROTO((int wn, int mode));
void wn_next_event wn__PROTO((int wn, int mask, event_t *ev));
void wn_warp_mouse wn__PROTO((int wn, int x, int y));
void wn_pushback_event wn__PROTO((event_t *ev));
unsigned long wn_get_resize_event wn__PROTO((int wn));
int wn_lost_selection wn__PROTO((int wn));
void wn_wait_for_release_of wn__PROTO((int wn, int buttons));
int wn_getpuck wn__PROTO((int wn, int *p_xpos, int *p_ypos));
int wn_getc wn__PROTO((int wn, char *p_ch));
void wn_ungetpuck wn__PROTO((int wn, int x, int y, int buttons));
void wn_await_window_size_change wn__PROTO((int wn));
wn_event_handler_func_t wn_interpose_event_handler
					wn__PROTO((wn_event_handler_func_t handler));

/* special purpose, unsupported (mostly for iclguide) */
void wn_get_root_wpos wn__PROTO((int wn, int *p_x, int *p_y));
int wn_open_rootwindow wn__PROTO((void));
int wn_windowfd wn__PROTO((int wn));
void _wn_want_own_icon wn__PROTO((void));
void _wn_dont_map_windows_on_create wn__PROTO((void));
void _wn_map_iclguide_window wn__PROTO((int wn));

#endif /* !WN_H_INCLUDED */
