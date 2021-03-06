/*
 *      ztcterm.c           IBM screen module             mak
 *
 *	Copyright (C) 1993 by the Regents of the University of California
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *  
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *  
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include "logo.h"
#include "globals.h"
#include <math.h>
#include <bios.h>
#include <fg.h>
#include <conio.h>
#include <disp.h>
#include <sound.h>
#include "ztcterm.h"

extern x_margin, y_margin;
/************************************************************/

unsigned _stack = 8000; /* 5000 for debugging, 65000 for real */

BOOLEAN in_graphics_mode = FALSE, in_splitscreen = FALSE;
BOOLEAN have_been_in_graphics_mode = FALSE;
BOOLEAN in_erase_mode = FALSE;

/* NOTE NOTE NOTE:  graphics.c really really believes that the top left
 * corner of the screen has hardware coords (0,0).  Zortech has (0,0)
 * in the BOTTOM left.  It's hopeless to fix graphics.c so instead we
 * use the following kludge: go ahead and think the screen is upside down,
 * and switch to Zortech coords only in draw_line! */

int ibm_screen_bottom, ibm_turtle_bottom_max;
int current_write_mode = FG_MODE_SET;
int current_vis = 0;
fg_color_t turtle_color, bg_color;
int texth;
int MaxX, MaxY;
int ztc_penwidth = 1, ztc_linepattern = -1;
fg_box_t ztc_box, ztc_textbox, text_scroll_box, text_last_line_box, clear_box;
void erase_graphics_top();

fg_line_t the_line;
fg_coord_t ztc_x, ztc_y;
fg_color_t ztc_textcolor;
fg_coord_t ztc_graph_textx, ztc_graph_texty;
FIXNUM pen_color = 0, back_ground = 7;

void moveto(int x, int y) {
    ztc_x = (fg_coord_t)x;
    ztc_y = (fg_coord_t)y;
}

void lineto(int x, int y) {
    fg_make_line(the_line, ztc_x, MaxY-ztc_y,
    		 (fg_coord_t)x, MaxY-(fg_coord_t)y);
    fg_drawlineclip((in_erase_mode ? bg_color : turtle_color),
		   current_write_mode, ~0, FG_LINE_SOLID, the_line, ztc_box);
    moveto(x, y);
}

void outtext(char *s) {
    fg_puts(FG_WHITE, FG_MODE_SET, ~0, FG_ROT0, ztc_x, MaxY-ztc_y,
    	    s, fg.displaybox);
}

void gr_mode()
{
    int errorcode;

    if (!in_graphics_mode) {
	x_coord = x_margin;
	y_coord = y_margin;
	errorcode = fg_init();
	if (have_been_in_graphics_mode) {
	    in_graphics_mode = TRUE;
	    redraw_graphics();
	}
	else {
	    if (errorcode == 0)
		err_logo(BAD_GRAPH_INIT, NIL);
	    else {
		in_graphics_mode = have_been_in_graphics_mode = TRUE;
		pen_color = 7;
		turtle_color = FG_HIGHLIGHT;
		back_ground = 0;
		bg_color = FG_BLACK;
		ztc_textcolor = FG_WHITE;
		ztc_box[FG_X1] = ztc_textbox[FG_X1]
			       = text_scroll_box[FG_X1]
			       = text_last_line_box[FG_X1]
			       = clear_box[FG_X1]
			       = fg.displaybox[FG_X1];
		ztc_textbox[FG_Y1] = fg.displaybox[FG_Y1];
		ztc_box[FG_X2] = ztc_textbox[FG_X2]
			       = text_scroll_box[FG_X2]
			       = text_last_line_box[FG_X2]
			       = clear_box[FG_X2]
			       = MaxX = fg.displaybox[FG_X2];
		ztc_box[FG_Y2] = MaxY
			       = clear_box[FG_Y2]
			       = fg.displaybox[FG_Y2];
		y_scale = (double)fg.pixelx/(double)fg.pixely;
		{
		    FILE *fp = fopen("scrunch.dat","r");
		    if (fp != NULL) {
			fread(&x_scale, sizeof(FLONUM), 1, fp);
			fread(&y_scale, sizeof(FLONUM), 1, fp);
			fclose(fp);
		    }
		}
		if (MaxY == 479)
		    texth = 16;
		else
		    texth = (MaxY+1)/25;
		ztc_box[FG_Y1] = 4*texth+1;
		clear_box[FG_Y1] = 4*texth;
		ibm_screen_bottom = MaxY - (ztc_box[FG_Y1]);
		ztc_textbox[FG_Y2] = 4*texth-1;
		text_scroll_box[FG_Y2] = 3*texth-1;
		text_scroll_box[FG_Y1] = 0;
		text_last_line_box[FG_Y2] = texth-1;
		ibm_turtle_bottom_max = -(MaxY/2) + (4*texth);
		lclearscreen();
		lcleartext();
		in_splitscreen = TRUE;
	   }
	}
    }
}

void nop()
{
}

void init_ibm_memory()
{
}

volatile int ctrl_c_count = 0;

BOOLEAN check_ibm_stop()
{
    int key;
    int __ss *p;

/*    if (kbhit())
	getch(); */   /* allow DOS to test for control-C */

    p = &key;
    if ((int)p < 500) {
	err_logo(STACK_OVERFLOW, NIL);
	return(1);
    }

    if (((key = bioskey(1)) == (4113)) || ctrl_c_count > 0) { /* control-q */
	if (ctrl_c_count == 0) getch();
	ctrl_c_count = 0;
	err_logo(STOP_ERROR,NIL);
	return(1);
    }
    if (key == (4375)) { /* control-w */
	getch();
	to_pending = 0;
	lpause();
    }
    return (0);
}

void term_init_ibm()
{
    disp_open();
    ztc_textcolor = FG_WHITE;
    tty_charmode = 0;
    x_max = 80;
    y_max = 24;
    x_coord = y_coord = 0;
    so_arr[0] = '\1'; so_arr[1] = '\0';
    se_arr[0] = '\2'; se_arr[1] = '\0';
}

void ibm_gotoxy(int x, int y)
{
    disp_move(y, x);
    disp_flush();
}

void ibm_clear_text() {
    if (in_graphics_mode) {
	if (ibm_screen_bottom != 0)
	    erase_graphics_top();
    } else {
	disp_move(0,0);
	disp_eeop();
	disp_flush();
    }
}

void ibm_clear_screen()
{
    fg_fillbox(bg_color, FG_MODE_SET, ~0, clear_box);
}

void ibm_plain_mode()
{
    if (in_graphics_mode)
	ztc_textcolor = FG_WHITE;
    else
	disp_endstand();
}

void ibm_bold_mode()
{
    if (in_graphics_mode)
	ztc_textcolor = FG_HIGHLIGHT;
    else
	disp_startstand();
}

void erase_graphics_top()
{
    fg_fillbox(FG_BLACK, FG_MODE_SET, ~0, ztc_textbox);
    ztc_graph_textx = 0;
    ztc_graph_texty = 3*texth;
}

/************************************************************/
/* These are the machine-specific graphics definitions.  All versions must
   provide a set of functions analogous to these. */

void save_pen(p)
pen_info *p;
{
    p->h = ztc_x;
    p->v = ztc_y;
    p->vis = current_vis;
    p->width = ztc_penwidth;
    p->color = turtle_color;
    get_pen_pattern(p->pattern);
    p->mode = get_ibm_pen_mode();
}

void restore_pen(p)
pen_info *p;
{
    moveto(p->h, p->v);
    current_vis = p->vis;
    ztc_penwidth = p->width;
    set_ibm_pen_mode(p->mode);  /* must restore mode before color */
    turtle_color = p->color;
    set_pen_pattern(p->pattern);
}

fg_color_t color_table[8] = {0, 9, 10, 11, 12, 13, 14, 15};

FIXNUM hw_color(FIXNUM c) {
    if (c >= 0 && c < 8) return color_table[c];
    if (c < 0) return c;
    return c-8;
}

void plain_xor_pen()
{
    ztc_penwidth = 1;
    turtle_color = hw_color(pen_color)^bg_color;
    current_write_mode = FG_MODE_XOR;
    in_erase_mode = FALSE;
}

void ztc_set_penc(FIXNUM c)
{
    draw_turtle();
    pen_color = c;
    turtle_color = hw_color(c);
    if (current_write_mode == FG_MODE_XOR)
	turtle_color = hw_color(pen_color)^bg_color;
    draw_turtle();
}

void ztc_set_bg(FIXNUM c) {
    back_ground = c;
    bg_color = hw_color(c);
    redraw_graphics();
}

void ibm_pen_down()
{
    current_write_mode = FG_MODE_SET;
    turtle_color = hw_color(pen_color);
    in_erase_mode = FALSE;
}

void ibm_pen_xor()
{
    current_write_mode = FG_MODE_XOR;
    turtle_color = hw_color(pen_color)^bg_color;
    in_erase_mode = FALSE;
}

void ibm_pen_erase()
{
    if (!in_erase_mode) {
	current_write_mode = FG_MODE_SET;
	turtle_color = hw_color(pen_color);
	in_erase_mode = TRUE;
    }
}

int get_ibm_pen_mode()
{
    if (in_erase_mode)
	return 2;
    else
	return current_write_mode;
}

void set_ibm_pen_mode(int m)
{
    switch (m) {
	case 2: ibm_pen_erase(); break;
	case FG_MODE_SET: ibm_pen_down(); break;
	case FG_MODE_XOR: ibm_pen_xor(); break;
    }
}

int get_ibm_pen_width()
{
    return ztc_penwidth;
}

void set_pen_pattern(char *pat)
{
    ztc_linepattern = *(int *)pat;
}

void set_list_pen_pattern(NODE *arg)
{
    NODE *temp;

    temp = cnv_node_to_numnode(car(arg));
    ztc_linepattern = getint(temp);
    gcref(temp);
}

void get_pen_pattern(char *pat)
{
    *(int *)pat = ztc_linepattern;
}

NODE *Get_node_pen_pattern()
{
    return(cons(make_intnode(-1)), NIL);
}

NODE *Get_node_pen_mode()
{
    if (in_erase_mode)
	return(make_static_strnode("erase"));
    if (current_write_mode == FG_MODE_XOR)
	return(make_static_strnode("reverse"));
    return(make_static_strnode("paint"));
}

void label(char *s)
{
    gr_mode();
    moveto(g_round(screen_x_coord), g_round(screen_y_coord));
    outtext(s);
}

void logofill()
{
    fg_fill(ztc_x, MaxY-ztc_y, turtle_color, turtle_color);
}

void erase_screen()
{
    ibm_clear_screen();
}

void t_screen()
{
    if (in_graphics_mode) {
	fg_term();
	in_graphics_mode = FALSE;
	in_splitscreen = FALSE;
    }
}

void s_screen()
{
    int save_vis;

    ibm_turtle_bottom_max = -(MaxY/2) + (4*texth);
    if (in_graphics_mode && !in_splitscreen) {
	if (turtle_shown) {
	    save_vis = current_vis;
	    current_vis = -1;
	    lhome();
	    current_vis = save_vis;
	}
	erase_graphics_top();
    }
    in_splitscreen = TRUE;
    ztc_box[FG_Y1] = 4*texth+1;
    clear_box[FG_Y1] = 4*texth;
    ibm_screen_bottom = MaxY - (ztc_box[FG_Y1]);
    gr_mode();
}

void f_screen()
{
    if (in_graphics_mode && in_splitscreen)
	erase_graphics_top();
    in_splitscreen = FALSE;
    ztc_box[FG_Y1] = clear_box[FG_Y1] = 0;
    ibm_screen_bottom = MaxY;
    ibm_turtle_bottom_max = -(MaxY/2);
    gr_mode();
}

FIXNUM mickey_x()
{
    return 0;
}

FIXNUM mickey_y()
{
    return 0;
}

BOOLEAN Button()
{
    return FALSE;
}

void tone(pitch, duration)
FIXNUM pitch, duration;
{
    FIXNUM period = 200000L/pitch;
    FIXNUM cycles = pitch/60;
    sound_tone((int)cycles, (int)period, (int)period);
}

FIXNUM t_height()
{
    return 18;
}

FLONUM t_half_bottom()
{
    return 6.0;
}

FLONUM t_side()
{
    return 19.0;
}

void check_scroll(void)
{
    fg_blit(text_scroll_box, 0, texth, fg.activepage, fg.activepage);
    fg_fillbox(FG_BLACK, FG_MODE_SET, ~0, text_last_line_box);
}

void ztc_put_char(int ch) {
    if (in_graphics_mode && !in_splitscreen)
	lsplitscreen();
    if (ch != '\1' && ch != '\2') {
	if (in_graphics_mode) {
	    if (ch == '\n') {
		ztc_graph_textx = 0;
		if (ztc_graph_texty - texth >= 0)
		    ztc_graph_texty -= texth;
		else check_scroll();
	    } else if (ch == '\b') {
		if (ztc_graph_textx > 0)
		    fg_adjustxy(FG_ROT180, 1, &ztc_graph_textx,
				&ztc_graph_texty, fg.charbox);
	    } else if (ch == '\t') {
		fg_adjustxy(FG_ROT0, 8 - (x_coord&07),
			    &ztc_graph_textx,
			    &ztc_graph_texty, fg.charbox);
		if (ztc_graph_textx >= MaxX) print_char(stdout,'\n');
	    } else {
		fg_putc(ztc_textcolor, FG_MODE_SET, ~0, FG_ROT0,
			ztc_graph_textx, ztc_graph_texty,
			ch, fg.displaybox);
		fg_adjustxy(FG_ROT0, 1, &ztc_graph_textx,
			    &ztc_graph_texty, fg.charbox);
		if (ztc_graph_textx >= MaxX) print_char(stdout,'\n');
	    }
	} else
    	    disp_putc(ch);
    }
}

BOOLEAN cursor_off = 0;

void fix_cursor(void) {
    if (!cursor_off && !in_graphics_mode) {
    	disp_hidecursor();
	cursor_off++;
    }
}

void zflush(void) {
    if (in_graphics_mode)
	fg_flush();
    else {
	disp_flush();
	if (cursor_off) disp_showcursor();
	cursor_off = 0;
    }
}

void newline_bugfix(void) {
    if (!in_graphics_mode) {
	if (disp_cursorrow+1 < disp_numrows)
    	    new_line(stdout);
	else
	    disp_move(disp_numrows-1,0);
	disp_flush();
    }
}

char graph_line_buffer[200];
char *graph_line_pointer = NULL;

int ztc_getc(FILE *strm) {
    int ch;

    if (strm != stdin || !interactive || !in_graphics_mode)
	return getc(strm);
    if (graph_line_pointer != NULL) {
	ch = *graph_line_pointer++;
	if (ch == '\t') graph_line_pointer += 2;
	if (ch != '\0') return ch;
    }
    graph_line_pointer = graph_line_buffer;
    do {
	fg_putc(FG_WHITE, FG_MODE_XOR, ~0, FG_ROT0,
		ztc_graph_textx, ztc_graph_texty,
		'_', fg.displaybox);
	ch = getch();
	fg_putc(FG_WHITE, FG_MODE_XOR, ~0, FG_ROT0,
		ztc_graph_textx, ztc_graph_texty,
		'_', fg.displaybox);
	if (ch == '\r' || ch == '\n') {
	    ch = '\n';
	    *graph_line_pointer++ = ch;
	    print_char(stdout,'\n');
	    *graph_line_pointer = '\0';
	    graph_line_pointer = graph_line_buffer;
	    return *graph_line_pointer++;
	} else if (ch == '\t') {
	    int i = 8 - (x_coord & 07);
	    *graph_line_pointer++ = '\t';
	    *graph_line_pointer++ = (char)i;
	    *graph_line_pointer++ = '\t';
	    print_char(stdout, '\t');
	} else if (ch == 0177 || ch == '\b') {
	    if (graph_line_pointer != graph_line_buffer) {
		graph_line_pointer--;
		if (*graph_line_pointer == '\t') {
		    int i = (int)*--graph_line_pointer;
		    --graph_line_pointer;
		    while (i-- > 0) print_char(stdout, '\b');
		} else {
		    print_char(stdout, '\b');
		    ztc_textcolor = FG_BLACK;
		    print_char(stdout, *graph_line_pointer);
		    ztc_textcolor = FG_WHITE;
		    print_char(stdout, '\b');
		}
	    }
	} else if (ch == 17 || ch == 23) {
	    print_char(stdout,'\n');
	    graph_line_pointer = NULL;
	    return ch;
	} else {
	    *graph_line_pointer++ = ch;
	    print_char(stdout, ch);
	}
    } while (ch != '\n');
    graph_line_pointer = graph_line_buffer;
    return *graph_line_pointer++;
}

void ztc_getcr(void) {
    (void) ztc_getc(stdin);
    graph_line_pointer = NULL;
}
