/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
# ifndef PI
# define PI 3.14159265358979323846
# endif PI

#define nil 0L

extern LVAL newrandomstate(), integer_list_2(), make_string();
extern xlfail();
extern double macheps();
extern unsigned long time(), ticks_per_second();
extern long time_stamp;
extern char buf[];

#ifdef UNIX
extern char *getunixenv();
#endif UNIX

/* symbols */
LVAL s_true, s_ielement, s_ivalue, s_sublist, s_select, s_elt, s_vector;
LVAL s_list, s_icontents, s_displacedto, s_arrayident, s_linked_plots;
LVAL s_subarray, a_float, s_pi;
LVAL s_selecting, s_brushing, s_user;
LVAL s_invisible, s_normal, s_hilited, s_selected, s_solid, s_dashed;
LVAL sk_point_labels;
LVAL s_left, s_center, s_right, s_top, s_bottom;
LVAL s_xor;
LVAL sk_allocate, sk_dispose, sk_resize, sk_redraw, sk_do_idle,
  sk_do_click, sk_do_motion,
  sk_do_key, sk_install, sk_remove, sk_update, sk_select,
  sk_mark_points_in_rect, sk_adjust_screen, 
  s_hardware_address,  s_number_of_variables, s_menu,
  sk_draw, sk_redraw_background, sk_redraw_content, sk_redraw_overlays,
  sk_overlay_click, sk_resize_overlays, sk_new,
  s_histogram_proto, sk_new_menu, sk_add_points, sk_add_lines, 
  s_black_on_white, sk_variable_labels, s_variable_labels,
  s_scatterplot_proto, sk_adjust_to_data, sk_showing_labels,
  s_showing_labels, sk_scale, s_spin_proto, s_scatmat_proto,
  s_name_list_proto, s_depth_cuing,
  s_showing_axes, sk_type, s_dotword,
  s_title, s_items, s_id, s_menu_list, s_key,
  s_mark, s_style, s_action, s_enabled, s_menu_proto, s_apple_menu_proto,
  s_menu_item_proto, sk_do_action, s_bold, s_italic, s_underline,
  s_outline, s_shadow, s_condense, s_extend, 
  sk_enabled, s_type, s_go_away, s_default_button,
  s_text, s_location, s_size, s_dialog,
  s_min_value, s_max_value, s_page_increment, s_editable,
  s_list_data, s_columns, sk_scroll_action, s_dialog_proto, sk_go_away,
  s_dialog_item_proto, s_button_item_proto, s_toggle_item_proto,
  s_text_item_proto, sk_editable, s_choice_item_proto, s_scroll_item_proto,
  sk_min_value, sk_max_value, sk_page_increment, s_list_item_proto,
  sk_columns, s_modeless, s_modal, s_hardware_objects, s_bind_to_file,
  sk_clobber, sk_own, sk_print, s_arrow, sk_activate, sk_close, s_value,
  s_features, s_and, s_or, s_not, s_listener, s_random_state, s_input_stream,
  sk_tolerance, sk_max_iters, sk_adjust_points_in_rect, sk_adjust_screen_point,
  s_self, s_function_documentation, s_variable_documentation, s_documentation,
  s_compound_data_proto, sk_data_length, sk_data_seq, sk_make_data,
  sk_select_data,
  s_machine_epsilon, s_graph, s_instance_slots, s_proto_name,
  s_content_variables, s_rotation_type, s_rotation_angle, s_rotation_controls,
  s_histogram_internals, sk_adjust_depth_cuing, sk_draw_axes, s_mode_list,
  s_scale_type, s_fixed, s_variable, sk_apply_transformation,
  sk_unselect_all_points, s_set_slot_hook, s_message_hook, s_mouse_mode,
  s_has_h_scroll,  s_has_v_scroll, s_fixed_aspect, sk_width, sk_color,
  sk_symbol, s_multiple, s_internals, s_color_index, s_cursor_index, 
  s_symbol_index, s_internal_ticks, s_pitching, s_rolling, s_yawing, sk_basis,
  sk_links, sk_linked, sk_show, sk_show_window;
  
statfinit()
{
  LVAL features;
#ifdef UNIX
  char *libdir;
#endif UNIX

  time_stamp = time((unsigned long *) NULL);
  
  s_hardware_objects = xlenter("*HARDWARE-OBJECTS*");
  s_linked_plots = xlenter("_linked_plots_");
  setvalue(s_linked_plots, NIL);
  setvalue(s_hardware_objects, NIL);
  s_random_state = xlenter("*RANDOM-STATE*");
  setvalue(s_random_state, newrandomstate());
  s_input_stream = xlenter("*INPUT-STREAM*");
  setvalue(s_input_stream, newustream());
  s_machine_epsilon = xlenter("MACHINE-EPSILON");
  setvalue(s_machine_epsilon, cvflonum((FLOTYPE) macheps()));
  setconstant(s_machine_epsilon, TRUE);
  s_internal_ticks = xlenter("INTERNAL-TIME-UNITS-PER-SECOND");
  setvalue(s_internal_ticks, cvfixnum((FIXTYPE) ticks_per_second()));
  setconstant(s_internal_ticks, TRUE);
  StInitGraphics();
  init_colors();
  init_cursors();
  init_plot_symbols();
  init_objects();
  
  features = xlenter("*FEATURES*");
  setvalue(features, NIL);
  setvalue(features, cons(xlenter("XLISP"), getvalue(features)));
#ifdef MACINTOSH
  setvalue(features, cons(xlenter("MACINTOSH"), getvalue(features)));
  if (is_small_machine())
    setvalue(features, cons(xlenter("SMALL-MACHINE"), getvalue(features)));
  get_directory(buf);
  setvalue(xlenter("*DEFAULT-PATH*"), make_string(buf));
  set_variable_docstring(xlenter("*DEFAULT-PATH*"), 
                         "Default path for require");  
#endif MACINTOSH
#ifdef SUNVIEW
  setvalue(features, cons(xlenter("SUNVIEW"), getvalue(features)));
#endif SUNVIEW
#ifdef X11WINDOWS
  setvalue(features, cons(xlenter("X11"), getvalue(features)));
#endif X11WINDOWS
#ifdef UNIX
  setvalue(features, cons(xlenter("UNIX"), getvalue(features)));
  if (! StHasWindows()) {
  	setfunction(xlenter("PLOT-POINTS"), 
		    getfunction(xlenter("GNU-PLOT-POINTS")));
  	setfunction(xlenter("PLOT-LINES"), 
		    getfunction(xlenter("GNU-PLOT-LINES")));
  }
  libdir = getunixenv("XLISPLIB");
  if (libdir == nil) libdir = "";
  setvalue(xlenter("*DEFAULT-PATH*"), make_string(libdir));
  set_variable_docstring(xlenter("*DEFAULT-PATH*"), 
			 "Default path for require");
#endif UNIX
#ifdef AMIGA
  setvalue(features, cons(xlenter("AMIGA"), getvalue(features)));
  setvalue(xlenter("*DEFAULT-PATH*"), make_string(""));
  set_variable_docstring(xlenter("*DEFAULT-PATH*"), 
			 (LVAL)"Default path for require"); /* cast added JKL */
#endif AMIGA
  if (StScreenHasColor()) 
    setvalue(features, cons(xlenter("COLOR"), getvalue(features)));
  if (StHasWindows()) {
    setvalue(features, cons(xlenter("DIALOGS"), getvalue(features)));
    setvalue(features, cons(xlenter("WINDOWS"), getvalue(features)));
  }
}

static init_color(index, sym)
	int index;
	LVAL sym;
{
  StGWSetColRefCon(index, sym);
  xlputprop(sym, cvfixnum((FIXTYPE) index), xlenter("color-index"));
}

static init_colors()
{
  init_color(0, xlenter("WHITE"));
  init_color(1, xlenter("BLACK"));
  init_color(2, xlenter("RED"));
  init_color(3, xlenter("GREEN"));
  init_color(4, xlenter("BLUE"));
  init_color(5, xlenter("CYAN"));
  init_color(6, xlenter("MAGENTA"));
  init_color(7, xlenter("YELLOW"));
}

static init_cursor(index, sym)
	int index;
	LVAL sym;
{
  StGWSetCursRefCon(index, sym);
  xlputprop(sym, cvfixnum((FIXTYPE) index), xlenter("cursor-index"));
}

static init_cursors()
{
  init_cursor(0, xlenter("ARROW"));
  init_cursor(1, xlenter("WATCH"));
  init_cursor(2, xlenter("CROSS"));
  init_cursor(3, xlenter("BRUSH"));
  init_cursor(4, xlenter("HAND"));
  init_cursor(5, xlenter("FINGER"));
  init_cursor(6, xlenter("HOUR-GLASS"));
  init_cursor(7, xlenter("TRASH-BAG"));
  init_cursor(8, xlenter("TRASH-CAN"));
}

static init_plot_symbol(i1, i2, sym)
	int i1, i2;
	LVAL sym;
{
  StGWSetSymRefCon(i1, sym);
  xlputprop(sym, integer_list_2(i1, i2), xlenter("symbol-index"));
}

static init_plot_symbols()
{
  init_plot_symbol(0, 3, xlenter("DOT"));
  init_plot_symbol(0, 5, xlenter("DOT1"));
  init_plot_symbol(1, 5, xlenter("DOT2"));
  init_plot_symbol(2, 5, xlenter("DOT3"));
  init_plot_symbol(3, 5, xlenter("DOT4"));
  init_plot_symbol(4, 5, xlenter("DISK"));
  init_plot_symbol(6, 7, xlenter("DIAMOND"));
  init_plot_symbol(8, 9, xlenter("CROSS"));
  init_plot_symbol(10, 11, xlenter("SQUARE"));
  init_plot_symbol(12, 13, xlenter("WEDGE1"));
  init_plot_symbol(14, 15, xlenter("WEDGE2"));
  init_plot_symbol(16, 17, xlenter("X"));
}

statsymbols()
{
  statobsymbols();
  s_true = xlenter("T");
  setconstant(s_true, TRUE);
  s_ielement = xlenter(":INITIAL-ELEMENT");
  s_ivalue = xlenter(":INITIAL-VALUE");
  s_sublist = xlenter("SUBLIST");
  s_select = xlenter("SELECT");
  s_elt = xlenter("ELT");
  s_vector = xlenter("VECTOR");
  s_list = xlenter("LIST");
  s_ielement = xlenter(":INITIAL-ELEMENT");
  s_icontents = xlenter(":INITIAL-CONTENTS");
  s_displacedto = xlenter(":DISPLACED-TO");
  s_arrayident = xlenter("_array-ident_");
  s_subarray = xlenter("SUBARRAY");
  a_float = xlenter("FLOAT");
  s_pi = xlenter("PI");
  setvalue(s_pi, cvflonum((FLOTYPE) PI));
  setconstant(s_pi, TRUE);
  
  s_selecting = xlenter("SELECTING");
  s_brushing = xlenter("BRUSHING");
  s_user = xlenter("USER");
  s_invisible = xlenter("INVISIBLE");
  s_normal = xlenter("NORMAL");
  s_hilited = xlenter("HILITED");
  s_selected = xlenter("SELECTED");
  s_solid = xlenter("SOLID");
  s_dashed = xlenter("DASHED");
  sk_point_labels = xlenter(":POINT-LABELS");
  s_left = xlenter("LEFT");
  s_center = xlenter("CENTER");
  s_right = xlenter("RIGHT");
  s_top = xlenter("TOP");
  s_bottom = xlenter("BOTTOM");
  s_xor = xlenter("XOR");
  
  sk_allocate = xlenter(":ALLOCATE");
  sk_dispose = xlenter(":DISPOSE");
  sk_resize = xlenter(":RESIZE");
  sk_redraw = xlenter(":REDRAW");
  sk_do_idle = xlenter(":DO-IDLE");
  sk_do_click = xlenter(":DO-CLICK");
  sk_do_motion = xlenter(":DO-MOTION");
  sk_do_key = xlenter(":DO-KEY");
  sk_install = xlenter(":INSTALL");
  sk_remove = xlenter(":REMOVE");
  sk_update = xlenter(":UPDATE");
  sk_select = xlenter(":SELECT");
  sk_mark_points_in_rect = xlenter(":MARK-POINTS-IN-RECT");
  sk_adjust_screen = xlenter(":ADJUST-SCREEN");

  s_hardware_address = xlenter("HARDWARE-ADDRESS");
  s_number_of_variables = xlenter("NUMBER-OF-VARIABLES");
  s_menu = xlenter("MENU");
  sk_draw = xlenter(":DRAW");
  sk_redraw_background = xlenter(":REDRAW-BACKGROUND");
  sk_redraw_content = xlenter(":REDRAW-CONTENT");
  sk_redraw_overlays = xlenter(":REDRAW-OVERLAYS");
  sk_resize_overlays = xlenter(":RESIZE-OVERLAYS");
  sk_overlay_click = xlenter(":OVERLAY-CLICK");
  sk_new = xlenter(":NEW");
  s_histogram_proto = xlenter("HISTOGRAM-PROTO");
  sk_new_menu = xlenter(":NEW-MENU");
  sk_add_points = xlenter(":ADD-POINTS");
  sk_add_lines = xlenter(":ADD-LINES");
  s_black_on_white = xlenter("BLACK-ON-WHITE");
  sk_variable_labels = xlenter(":VARIABLE-LABELS");
  s_variable_labels = xlenter("VARIABLE-LABELS");
  s_scatterplot_proto = xlenter("SCATTERPLOT-PROTO");
  sk_adjust_to_data = xlenter(":ADJUST-TO-DATA");
  sk_showing_labels = xlenter(":SHOWING-LABELS");
  s_showing_labels = xlenter("SHOWING-LABELS");
  sk_scale = xlenter(":SCALE");
  s_spin_proto = xlenter("SPIN-PROTO");
  s_scatmat_proto = xlenter("SCATMAT-PROTO");
  s_name_list_proto = xlenter("NAME-LIST-PROTO");
  s_depth_cuing = xlenter("DEPTH-CUING");
  s_showing_axes = xlenter("SHOWING-AXES");
  sk_type = xlenter(":TYPE");
  s_dotword = xlenter("DOT");
  s_title = xlenter("TITLE");
  s_items = xlenter("ITEMS");
  s_enabled = xlenter("ENABLED");
  s_id = xlenter("ID");
  s_menu_list = xlenter("MENU-LIST");
  s_title = xlenter("TITLE");
  s_key = xlenter("KEY");
  s_mark = xlenter("MARK");
  s_style = xlenter("STYLE");
  s_action = xlenter("ACTION");
  s_menu_proto = xlenter("MENU-PROTO");
  s_apple_menu_proto = xlenter("APPLE-MENU-PROTO");
  s_menu_item_proto = xlenter("MENU-ITEM-PROTO");
  sk_do_action = xlenter(":DO-ACTION");
  s_bold = xlenter("BOLD");
  s_italic = xlenter("ITALIC");
  s_underline = xlenter("UNDERLINE");
  s_outline = xlenter("OUTLINE");
  s_shadow = xlenter("SHADOW");
  s_condense = xlenter("CONDENSE");
  s_extend = xlenter("EXTEND");
  sk_enabled = xlenter(":ENABLED");
  s_type = xlenter("TYPE");
  s_go_away = xlenter("GO-AWAY");
  s_default_button = xlenter("DEFAULT-BUTTON");
  s_text = xlenter("TEXT");
  s_location = xlenter("LOCATION");
  s_size = xlenter("SIZE");
  s_dialog = xlenter("DIALOG");
  s_min_value = xlenter("MIN-VALUE");
  s_max_value = xlenter("MAX-VALUE");
  s_page_increment = xlenter("PAGE-INCREMENT");
  s_editable = xlenter("EDITABLE");
  s_list_data = xlenter("LIST-DATA");
  s_columns = xlenter("COLUMNS");
  sk_scroll_action = xlenter(":SCROLL-ACTION");
  s_dialog_proto = xlenter("DIALOG-PROTO");
  sk_go_away = xlenter(":GO-AWAY");
  s_dialog_item_proto = xlenter("DIALOG-ITEM-PROTO");
  s_button_item_proto = xlenter("BUTTON-ITEM-PROTO");
  s_toggle_item_proto = xlenter("TOGGLE-ITEM-PROTO");
  s_text_item_proto = xlenter("TEXT-ITEM-PROTO");
  sk_editable = xlenter(":EDITABLE");
  s_choice_item_proto = xlenter("CHOICE-ITEM-PROTO");
  s_scroll_item_proto = xlenter("SCROLL-ITEM-PROTO");
  sk_min_value = xlenter(":MIN-VALUE");
  sk_max_value = xlenter(":MAX-VALUE");
  sk_page_increment = xlenter(":PAGE-INCREMENT");
  s_list_item_proto = xlenter("LIST-ITEM-PROTO");
  sk_columns = xlenter(":COLUMNS");
  s_modeless = xlenter("MODELESS");
  s_modal = xlenter("MODAL");
  s_hardware_objects = xlenter("*HARDWARE-OBJECTS*");
  s_bind_to_file = xlenter("BIND-TO-FILE");
  sk_clobber = xlenter(":CLOBBER");
  sk_own = xlenter(":OWN");
  sk_print = xlenter(":PRINT");
  s_arrow = xlenter("ARROW");
  sk_activate = xlenter(":ACTIVATE");
  sk_close = xlenter(":CLOSE");
  s_value = xlenter("VALUE");
  s_features = xlenter("*FEATURES*");
  s_and = xlenter("AND");
  s_or = xlenter("OR");
  s_not = xlenter("NOT");
  s_listener = xlenter("*LISTENER*");
  s_random_state = xlenter("*RANDOM-STATE*");
  s_input_stream = xlenter("*INPUT-STREAM*");
  sk_tolerance = xlenter(":TOLERANCE");
  sk_max_iters = xlenter(":MAX-ITERS");
  sk_adjust_points_in_rect = xlenter(":ADJUST-POINTS-IN-RECT");
  sk_adjust_screen_point = xlenter(":ADJUST-SCREEN-POINT");
  s_self = xlenter("SELF");
  s_function_documentation = xlenter("FUNCTION-DOCUMENTATION");
  s_variable_documentation = xlenter("VARIABLE-DOCUMENTATION");
  s_documentation = xlenter("DOCUMENTATION");
  s_compound_data_proto = xlenter("COMPOUND-DATA-PROTO");
  sk_data_length = xlenter(":DATA-LENGTH");
  sk_data_seq = xlenter(":DATA-SEQ");
  sk_make_data = xlenter(":MAKE-DATA");
  sk_select_data = xlenter(":SELECT-DATA");
  s_random_state = xlenter("*RANDOM-STATE*");
  s_input_stream = xlenter("*INPUT-STREAM*");
  s_machine_epsilon = xlenter("MACHINE-EPSILON");
  s_graph = xlenter("GRAPH");
  s_instance_slots = xlenter("INSTANCE-SLOTS");
  s_proto_name = xlenter("PROTO-NAME");
  s_content_variables = xlenter("CONTENT-VARIABLES");
  s_rotation_type = xlenter("ROTATION-TYPE");
  s_rotation_angle = xlenter("ROTATION-ANGLE");
  s_rotation_controls = xlenter("ROTATION-CONTROLS");
  s_histogram_internals = xlenter("HISTOGRAM-INTERNALS");
  sk_adjust_depth_cuing = xlenter(":ADJUST-DEPTH-CUING");
  sk_draw_axes = xlenter(":DRAW-AXES");
  s_mode_list = xlenter("MODE-LIST");
  s_scale_type = xlenter("SCALE-TYPE");
  s_fixed = xlenter("FIXED");
  s_variable = xlenter("VARIABLE");
  sk_apply_transformation = xlenter(":APPLY-TRANSFORMATION");
  sk_unselect_all_points = xlenter(":UNSELECT-ALL-POINTS");
  s_set_slot_hook = xlenter("*SET-SLOT-HOOK*");
  s_message_hook = xlenter("*MESSAGE-HOOK*");
  s_mouse_mode = xlenter("MOUSE-MODE");
  s_has_h_scroll = xlenter("HAS-H-SCROLL");
  s_has_v_scroll = xlenter("HAS-V-SCROLL");
  s_fixed_aspect = xlenter("FIXED-ASPECT");
  sk_width = xlenter(":WIDTH");
  sk_color = xlenter(":COLOR");
  sk_symbol = xlenter(":SYMBOL");
  s_multiple = xlenter("MULTIPLE");
  s_internals = xlenter("INTERNALS");
  s_color_index = xlenter("color-index");
  s_cursor_index = xlenter("cursor-index");
  s_symbol_index = xlenter("symbol-index");
  s_internal_ticks = xlenter("INTERNAL-TIME-UNITS-PER-SECOND");
  s_pitching = xlenter("PITCHING");
  s_rolling = xlenter("ROLLING");
  s_yawing = xlenter("YAWING");
  sk_basis = xlenter(":BASIS");
  sk_linked = xlenter(":LINKED");
  sk_links = xlenter(":LINKS");
  sk_show = xlenter(":SHOW");
  sk_show_window = xlenter(":SHOW-WINDOW");
}
