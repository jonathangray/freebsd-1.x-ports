#ifndef XLISP_ONLY
/************************************************************************/
/************************************************************************/
/**                                                                    **/
/**                          Object Methods                            **/
/**                                                                    **/
/************************************************************************/
/************************************************************************/

/* objects.c */
{	NULL,				S, xsmessage_method		}, /* 300 */
{	NULL,				S, xsreparent_object		}, 
{	NULL,				S, xshas_slot			}, 
{	NULL,				S, xshas_method			},
{	NULL,				S, xsadd_slot			},
{	NULL,				S, xsdelete_slot		},
{	NULL,				S, xsadd_method			},
{	NULL,				S, xsdelete_method		},
{	NULL,				S, xsshow_object		},
{	NULL,				S, xsobject_isnew		},
{	NULL,				S, xsparents			},
{	NULL,				S, xsprecedence_list		},
{	NULL,				S, xsobject_slots		},
{	NULL,				S, xsobject_methods		},
{	NULL,				S, xsobject_documentation	},
{	NULL,				S, xsmakeproto			},

/* hardware-objects */
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

/* windows.c */
{	NULL,				S, xsshowwindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xswindow_title		},
{	NULL,				S, xswindow_location		},
{	NULL,				S, xswindow_size		},
{	NULL,				S, xswindow_frame_location	},
{	NULL,				S, xswindow_frame_size		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

#ifdef MACINTOSH
/* edit.c */
{	NULL,				S, xsedit_window_isnew		},
{	NULL,				S, xsedit_window_allocate	},
{	NULL,				S, xsedit_window_cut		},
{	NULL,				S, xsedit_window_copy		},
{	NULL,				S, xsedit_window_paste		},
{	NULL,				S, xsedit_window_revert		},
{	NULL,				S, xsedit_window_save		},
{	NULL,				S, xsedit_window_save_as	},
{	NULL,				S, xsedit_window_save_copy	},
{	NULL,				S, xsedit_window_paste_stream	},
{	NULL,				S, xsedit_window_paste_string	},
{	NULL,				S, xsedit_window_flush_window	},
{	NULL,				S, xsedit_window_selection_stream},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_remove		},
{	NULL,				S, xsedit_window_activate	},
{	NULL,				S, xsedit_window_update		},
{	NULL,				S, xsedit_window_find		},
{	NULL,				S, xslistener_isnew		},
{	NULL,				S, xslistener_allocate		},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
{	NULL,				S, xshidewindow			},
#endif MACINTOSH

/* menus.c */
{	NULL,				S, xsmenu_isnew			},
{	NULL,				S, xsallocate_menu		},
{	NULL,				S, xsdispose_menu		},
{	NULL,				S, xsinstall_menu		},
{	NULL,				S, xsremove_menu		},
{	NULL,				S, xsmenu_enabled		},
{	NULL,				S, xsupdate_menu		},
{	NULL,				S, xsallocated_p		},
{	NULL,				S, xsmenu_title			},
{	NULL,				S, xsmenu_items			},
{	NULL,				S, xsinstalled_p		},
{	NULL,				S, xsappend_items		},
{	NULL,				S, xsdelete_items		},
{	NULL,				S, xsmenu_select		},
{	NULL,				S, xsmenu_popup			},
#ifdef MACINTOSH
{	NULL,				S, xsapple_menu_isnew		}, 
{	NULL,				S, xsapple_menu_select		}, 
#endif MACINTOSH
{	NULL,				S, xsitem_isnew			},
{	NULL,				S, xsitem_title			},
{	NULL,				S, xsitem_key			},
{	NULL,				S, xsitem_mark			},
{	NULL,				S, xsitem_style			},
{	NULL,				S, xsitem_action		},
{	NULL,				S, xsitem_enabled		},
{	NULL,				S, xsitem_installed_p		},
{	NULL,				S, xsitem_update		},
{	NULL,				S, xsitem_do_action		},

/* dialog.c */
{	NULL,				S, xsdialog_isnew		},
{	NULL,				S, xsdialog_allocate		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_remove		},
{	NULL,				S, xsdialog_allocated_p		},
{	NULL,				S, xsdialog_default_button	},
{	NULL,				S, xsdialog_modal		},
{	NULL,				S, xsdialog_item_do_action	},
{	NULL,				S, xsdialog_item_action		},
{	NULL,				S, xsbutton_item_isnew		},
{	NULL,				S, xstoggle_item_isnew		},
{	NULL,				S, xstoggle_item_value		},
{	NULL,				S, xstext_item_isnew		},
{	NULL,				S, xstext_item_text		},
{	NULL,				S, xschoice_item_isnew		},
{	NULL,				S, xschoice_item_value		},
{	NULL,				S, xsscroll_item_isnew		},
{	NULL,				S, xsscroll_item_value		},
{	NULL,				S, xsscroll_item_max		},
{	NULL,				S, xsscroll_item_min		},
{	NULL,				S, xsscroll_item_action		},
{	NULL,				S, xslist_item_isnew		},
{	NULL,				S, xslist_item_action		},
{	NULL,				S, xslist_item_text		},
{	NULL,				S, xslist_item_selection	},

/* xsiviewwindow.c */
{	NULL,				S, iview_window_isnew		},
{	NULL,				S, iview_window_allocate	},

{	NULL,				S, iview_window_idle_on		},

{	NULL,				S, iview_window_menu		},

{	NULL,				S, xsiview_window_update	},
{	NULL,				S, xsiview_window_activate	},

{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_remove		},
{	NULL,				S, iview_window_while_button_down},
{	NULL,				S, iview_window_show_window	},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

{	NULL,				S, iview_window_canvas_width	},
{	NULL,				S, iview_window_canvas_height	},
{	NULL,				S, iview_window_line_type	},
{	NULL,				S, iview_window_draw_mode	},
{	NULL,				S, iview_window_draw_color	},
{	NULL,				S, iview_window_back_color	},
{	NULL,				S, iview_window_use_color	},
{	NULL,				S, iview_window_reverse_colors	},
{	NULL,				S, iview_window_view_rect	},
{	NULL,				S, iview_window_line_width	},
{	NULL,				S, iview_window_clip_rect	},
{	NULL,				S, iview_window_cursor		},

{	NULL,				S, iview_window_has_h_scroll	},
{	NULL,				S, iview_window_has_v_scroll	},
{	NULL,				S, iview_window_scroll		},
{	NULL,				S, iview_window_h_scroll_incs	},
{	NULL,				S, iview_window_v_scroll_incs	},

{	NULL,				S, iview_window_draw_line	},
{	NULL,				S, iview_window_draw_point	},
{	NULL,				S, iview_window_erase_rect	},
{	NULL,				S, iview_window_frame_rect	},
{	NULL,				S, iview_window_paint_rect	},
{	NULL,				S, iview_window_erase_oval	},
{	NULL,				S, iview_window_frame_oval	},
{	NULL,				S, iview_window_paint_oval	},
{	NULL,				S, iview_window_erase_arc	},
{	NULL,				S, iview_window_frame_arc	},
{	NULL,				S, iview_window_paint_arc	},
{	NULL,				S, iview_window_erase_poly	},
{	NULL,				S, iview_window_frame_poly	},
{	NULL,				S, iview_window_paint_poly	},

{	NULL,				S, iview_window_text_ascent	},
{	NULL,				S, iview_window_text_descent	},
{	NULL,				S, iview_window_text_width	},
{	NULL,				S, iview_window_draw_string	},
{	NULL,				S, iview_window_draw_string_up	},
{	NULL,				S, iview_window_draw_text	},
{	NULL,				S, iview_window_draw_text_up	},

{	NULL,				S, iview_window_draw_symbol	},
{	NULL,				S, iview_window_replace_symbol	},

{	NULL,				S, iview_window_start_buffering	},
{	NULL,				S, iview_window_buffer_to_screen},

#ifdef MACINTOSH
{	NULL,				S, iview_window_copy_to_clip	},
#endif MACINTOSH
{	NULL,				S, iview_window_drag_grey_rect	},
{	NULL,				S, iview_window_dump_image	},
{	NULL,				S, gw_draw_bitmap		},

/* xsiview.c */
{	NULL,				S, iview_isnew			},
{	NULL,				S, iview_allocate		},

{	NULL,				S, iview_std_resize		},
{	NULL,				S, iview_std_redraw		},
{	NULL,				S, iview_std_redraw_background	},
{	NULL,				S, iview_std_redraw_content	},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_std_adjust_screen	},
{	NULL,				S, iview_std_adjust_points_in_rect},
{	NULL,				S, iview_std_adjust_screen_point},
{	NULL,				S, iview_std_mark_points_in_rect},

{	NULL,				S, iview_content_rect		},
{	NULL,				S, iview_content_origin		},
{	NULL,				S, iview_content_variables	},
{	NULL,				S, iview_click_range		},
{	NULL,				S, iview_mouse_mode		},
{	NULL,				S, iview_showing_labels		},
{	NULL,				S, iview_margin			},
{	NULL,				S, iview_fixed_aspect		},
{	NULL,				S, iview_dirty			},

{	NULL,				S, iview_x_axis			},
{	NULL,				S, iview_y_axis			},

{	NULL,				S, iview_brush			},
{	NULL,				S, iview_erase_brush		},
{	NULL,				S, iview_draw_brush		},
{	NULL,				S, iview_move_brush		},
{	NULL,				S, iview_resize_brush		},

{	NULL,				S, iview_do_click		},
{	NULL,				S, iview_do_motion		},
{	NULL,				S, iview_std_click		},
{	NULL,				S, iview_std_click		},
{	NULL,				S, iview_std_motion		},
{	NULL,				S, iview_unselect_all_points	},
{	NULL,				S, iview_erase_selection	},
{	NULL,				S, iview_mask_selection		},
{	NULL,				S, iview_unmask_all_points	},
{	NULL,				S, iview_points_showing		},
{	NULL,				S, iview_points_hilited		},
{	NULL,				S, iview_points_selected	},
{	NULL,				S, iview_points_selected	},
{	NULL,				S, iview_show_all_points	},
{	NULL,				S, iview_all_points_showing	},
{	NULL,				S, iview_all_points_unmasked	},
{	NULL,				S, iview_any_points_selected	},

{	NULL,				S, iview_linked			},
#ifndef OLDLINKS
{	NULL,				S, iview_links			},
#endif OLDLINKS

{	NULL,				S, iview_num_variables		},
{	NULL,				S, iview_variable_label		},
{	NULL,				S, iview_range			},
{	NULL,				S, iview_scaled_range		},
{	NULL,				S, iview_screen_range		},
{	NULL,				S, iview_transformation		},
{	NULL,				S, iview_apply_transformation	},

{	NULL,				S, iview_add_points		},
{	NULL,				S, iview_clear_points		},
{	NULL,				S, iview_num_points		},
{	NULL,				S, iview_point_coordinate	},
{	NULL,				S, iview_point_screen_coordinate},
{	NULL,				S, iview_point_transformed_coordinate	},
{	NULL,				S, iview_point_masked		},
{	NULL,				S, iview_point_color		},
{	NULL,				S, iview_point_state		},
{	NULL,				S, iview_point_screen_state	},
{	NULL,				S, iview_point_marked		},
{	NULL,				S, iview_point_label		},
{	NULL,				S, iview_point_symbol		},
{	NULL,				S, iview_point_selected		},
{	NULL,				S, iview_point_hilited		},
{	NULL,				S, iview_point_showing		},

{	NULL,				S, iview_add_lines		},
{	NULL,				S, iview_clear_lines		},
{	NULL,				S, iview_num_lines		},
{	NULL,				S, iview_line_coordinate	},
{	NULL,				S, iview_line_screen_coordinate	},
{	NULL,				S, iview_line_transformed_coordinate	},
{	NULL,				S, iview_line_masked		},
{	NULL,				S, iview_line_color		},
{	NULL,				S, iview_line_next		},
{	NULL,				S, iview_line_type		},
{	NULL,				S, iview_line_width		},

#ifdef USESTRINGS
{	NULL,				S, iview_add_strings		},
{	NULL,				S, iview_clear_strings		},
{	NULL,				S, iview_num_strings		},
{	NULL,				S, iview_string_coordinate	},
{	NULL,				S, iview_string_screen_coordinate},
{	NULL,				S, iview_string_transformed_coordinate	},
{	NULL,				S, iview_string_masked		},
{	NULL,				S, iview_string_color		},
{	NULL,				S, iview_string_modifiers	},
#endif /* USESTRINGS */

{	NULL,				S, iview_draw_data_points	},
{	NULL,				S, iview_draw_data_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_draw_data_strings	},
#endif /* USESTRINGS */

{	NULL,				S, iview_rotate_2		},

{	NULL,				S, iview_adjust_to_data		},
{	NULL,				S, iview_visible_range		},
{	NULL,				S, iview_scale_to_range		},
{	NULL,				S, iview_scale			},
{	NULL,				S, iview_shift			},

{	NULL,				S, iview_clear_masks		},
{	NULL,				S, iview_slice_variable		},
{	NULL,				S, iview_real_to_screen		},
{	NULL,				S, iview_screen_to_real		},
{	NULL,				S, iview_scaled_to_screen	},
{	NULL,				S, iview_screen_to_scaled	},
{	NULL,				S, iview_points_in_rect		},
{	NULL,				S, iview_adjust_depth_cuing	},

{	NULL,				S, iview_spin_allocate		},
{	NULL,				S, iview_spin_content_variables	},
{	NULL,				S, iview_spin_showing_axes	},
{	NULL,				S, iview_spin_depth_cuing	},
{	NULL,				S, iview_spin_resize		},
{	NULL,				S, iview_spin_redraw_content	},
{	NULL,				S, iview_spin_rotate		},
{	NULL,				S, iview_spin_angle		},
{	NULL,				S, iview_spin_rotate		},
{	NULL,				S, iview_spin_draw_axes		},

{	NULL,				S, iview_scatmat_allocate	},
{	NULL,				S, iview_scatmat_resize		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_scatmat_redraw_content	},
{	NULL,				S, iview_scatmat_click		},
{	NULL,				S, iview_scatmat_motion		},
{	NULL,				S, iview_scatmat_add_points	},
{	NULL,				S, iview_scatmat_add_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_scatmat_add_strings	},
#endif /* USESTRINGS */
{	NULL,				S, iview_scatmat_adjust_screen_point},
{	NULL,				S, iview_scatmat_adjust_points_in_rect},
{	NULL,				S, iview_scatmat_mark_points_in_rect},

{	NULL,				S, iview_list_allocate		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, iview_list_redraw_content	},
{	NULL,				S, iview_list_add_points	},
{	NULL,				S, iview_list_adjust_screen_point},
{	NULL,				S, iview_list_adjust_points_in_rect},
{	NULL,				S, iview_list_mark_points_in_rect},

{	NULL,				S, iview_hist_isnew		},
{	NULL,				S, iview_hist_allocate		},
{	NULL,				S, iview_hist_add_points	},
{	NULL,				S, iview_hist_clear_points	},
{	NULL,				S, iview_hist_resize		},
{	NULL,				S, iview_hist_redraw_content	},
{	NULL,				S, iview_hist_adjust_screen	},
{	NULL,				S, iview_hist_num_bins		},
{	NULL,				S, iview_hist_bin_counts	},
{	NULL,				S, iview_hist_adjust_to_data	},
{	NULL,				S, iview_hist_adjust_screen_point},
{	NULL,				S, iview_hist_adjust_points_in_rect},
{	NULL,				S, iview_hist_mark_points_in_rect},

{	NULL,				S, iview_plot2d_add_points	},
{	NULL,				S, iview_plot2d_add_lines	},
#ifdef USESTRINGS
{	NULL,				S, iview_plot2d_add_strings	},
#endif /* USESTRINGS */
{	NULL,				S, iview_plot2d_adjust_to_data	},

/* compound.c */
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},
{	NULL,				S, xsobject_null_method		},

/************************************************************************/
/************************************************************************/
/**                                                                    **/
/**                        Reguler Functions                           **/
/**                                                                    **/
/************************************************************************/
/************************************************************************/

/* dialog.c */
{	"SYSBEEP",			S, xssysbeep			},
{	"NUM-TO-STRING",		S, xsnumtostring		},
#ifdef MACINTOSH
{	"ABOUT-XLISP-STAT",		S, xsabout_xlisp_stat		},

/* edit.c */
{	"OPEN-FILE-DIALOG",		S, xsopenfiledialog		},
{	"SET-FILE-DIALOG",		S, xssetfiledialog		},
{	"SET-VOLUME",			S, xssetvolume			},
{	"FRONT-WINDOW",			S, xsfrontwindow		},
{	"HIDE-FRONT-WINDOW",		S, xshidefrontwindow		},
{	"SYSTEM-EDIT",			S, xssystem_edit		},
#endif MACINTOSH

/* experimental.c */
{	"TIME",				F, xstime			},
{	"GET-INTERNAL-RUN-TIME",	S, xs_get_internal_run_time	},
{	"GET-INTERNAL-REAL-TIME",	S, xs_get_internal_real_time	},
{	"GETENV",			S, xsgetenv			},

/* common.c */
{	"APROPOS",			S, xsapropos			},
{	"APROPOS-LIST",			S, xsaproposlist		},
{	"IDENTITY",			S, xsidentity			},
{	"MAKE-LIST",			S, xsmklist			},
{	"ADJOIN",			S, xsadjoin			},
{	"FILE-POSITION",		S, xsfileposition		},
{	"FORMAT",			S, xsformat			},
{	"FORCE-OUTPUT",			S, xsforce_output		},
{	"COPY-LIST",			S, xscopylist			},
{	"COPY-SEQ",			S, xscopyseq			},
{	"REDUCE",			S, xsreduce			},
{	"MAP",				S, xsmap			},
{	"ELT",				S, xselt			},
{	"COERCE",			S, xscoerce			},
{	"APPLY",			S, xsapply			},
{	"EVAL",				S, xseval			},
{	"LOAD",				S, xsload			},
{	"COMPLEXP",			S, xscomplexp			},
{	"COMPLEX",			S, xsrcomplex			},
{	"CONJUGATE",			S, xsrconjugate			},
{	"REALPART",			S, xsrrealpart			},
{	"IMAGPART",			S, xsrimagpart			},
{	"EQL",				S, xseql			},
{	"EQUAL",			S, xsequal			},
{	"DEFCONSTANT",			F, xsdefconstant		},
{	"DEFPARAMETER",			F, xsdefparameter		},
{	"DEFVAR",			F, xsdefvar			},
{	"MAKUNBOUND",			S, xsmakunbound			},
{	"FMAKUNBOUND",			S, xsfmakunbound		},
{	"MAKE-STRING",			S, xsmake_string		},
{	"CONCATENATE",			S, xsconcatenate		},
{	"SOME",				S, xssome			},
{	"EVERY",			S, xsevery			},
{	"NOTANY",			S, xsnotany			},
{	"NOTEVERY",			S, xsnotevery			},
{	"UNION",			S, xsunion			},
{	"INTERSECTION",			S, xsintersection		},
{	"SET-DIFFERENCE",		S, xsset_difference		},
{	"SUBSETP",			S, xssubsetp			},
{	"REMOVE-DUPLICATES",		S, xsremove_duplicates		},
{	"BUTLAST",			S, xsbutlast			},
{	"FIND",				S, xsfind			},
{	"POSITION",			S, xsposition			},
{	"ERROR",			S, xserror			},
{	"BREAK",			S, xsbreak			},

/* commonarrays.c */
{	"ARRAYP",			S, xsarrayp			},
{	"ARRAY-IN-BOUNDS-P",		S, xsarrayinboundsp		},
{	"ARRAY-DIMENSIONS",		S, xsarraydimensions		},
{	"ARRAY-RANK",			S, xsarrayrank			},
{	"ARRAY-TOTAL-SIZE",		S, xsarraytotalsize		},
{	"ARRAY-DIMENSION",		S, xsarraydimension		},
{	"ARRAY-ROW-MAJOR-INDEX",	S, xsarrayrowmajorindex		},
{	"AREF",				S, xsaref			},
{	"MAKE-ARRAY",			S, xsmakearray			},

/* distributions.c */
{	"NORMAL-CDF",			S, xsrnormalcdf			},
{	"BETA-CDF",			S, xsrbetacdf			},
{	"GAMMA-CDF",			S, xsrgammacdf			},
{	"CHISQ-CDF",			S, xsrchisqcdf			},
{	"T-CDF",			S, xsrtcdf			},
{	"F-CDF",			S, xsrfcdf			},
{	"CAUCHY-CDF",			S, xsrcauchycdf			},
{	"LOG-GAMMA",			S, xsrloggamma			},
{	"BIVNORM-CDF",			S, xsrbnormcdf			},
{	"NORMAL-QUANT",			S, xsrnormalquant		},
{	"CAUCHY-QUANT",			S, xsrcauchyquant		},
{	"BETA-QUANT",			S, xsrbetaquant			},
{	"GAMMA-QUANT",			S, xsrgammaquant		},
{	"CHISQ-QUANT",			S, xsrchisqquant		},
{	"T-QUANT",			S, xsrtquant			},
{	"F-QUANT",			S, xsrfquant			},
{	"NORMAL-DENS",			S, xsrnormaldens		},
{	"CAUCHY-DENS",			S, xsrcauchydens		},
{	"BETA-DENS",			S, xsrbetadens			},
{	"GAMMA-DENS",			S, xsrgammadens			},
{	"CHISQ-DENS",			S, xsrchisqdens			},
{	"T-DENS",			S, xsrtdens			},
{	"F-DENS",			S, xsrfdens			},
{	"UNIFORM-RAND",			S, xsruniformrand		},
{	"NORMAL-RAND",			S, xsrnormalrand		},
{	"CAUCHY-RAND",			S, xsrcauchyrand		},
{	"GAMMA-RAND",			S, xsrgammarand			},
{	"CHISQ-RAND",			S, xsrchisqrand			},
{	"T-RAND",			S, xsrtrand			},
{	"BETA-RAND",			S, xsrbetarand			},
{	"F-RAND",			S, xsrfrand			},

/* ddistributions.c */
{	"BINOMIAL-CDF",			S, xsrbinomialcdf		},
{	"POISSON-CDF",			S, xsrpoissoncdf		},
{	"BINOMIAL-PMF",			S, xsrbinomialpmf		},
{	"POISSON-PMF",			S, xsrpoissonpmf		},
{	"BINOMIAL-QUANT",		S, xsrbinomialquant		},
{	"POISSON-QUANT",		S, xsrpoissonquant		},
{	"BINOMIAL-RAND",		S, xsrbinomialrand		},
{	"POISSON-RAND",			S, xsrpoissonrand		},

/* linalg.c */
{	"LU-DECOMP",			S, xslu_decomp			},
{	"LU-SOLVE",			S, xslu_solve			},
{	"DETERMINANT",			S, xslu_determinant		},
{	"INVERSE",			S, xslu_inverse			},
{	"SV-DECOMP",			S, xssv_decomp			},
{	"QR-DECOMP",			S, xsqr_decomp			},
{	"CHOL-DECOMP",			S, xschol_decomp		},
{	"RCONDEST",			S, xsrcondest			},
{	"MAKE-ROTATION",		S, xsmake_rotation		},
{	"SPLINE",			S, xsspline			},
{	"KERNEL-DENS",			S, xskernel_dens		},
{	"KERNEL-SMOOTH",		S, xskernel_smooth		},
{	"base-lowess",			S, xsbase_lowess		},
{	"SURFACE-CONTOUR",		S, xssurface_contour		},
{	"FFT",				S, xsfft			},

/* matrices1.c */
{	"MATMULT",			S, xsmatmult			},
{	"%*",				S, xsmatmult			},
{	"INNER-PRODUCT",		S, xsmatmult			},
{	"CROSS-PRODUCT",		S, xscrossproduct		},
{	"DIAGONAL",			S, xsdiagonal			},
{	"IDENTITY-MATRIX",		S, xsidentitymatrix		},
{	"OUTER-PRODUCT",		S, xsouterproduct		},
{	"ROW-LIST",			S, xsrowlist			},
{	"COLUMN-LIST",			S, xscolumnlist			},
{	"BIND-ROWS",			S, xsbindrows			},
{	"BIND-COLUMNS",			S, xsbindcols			},
{	"TRANSPOSE",			S, xstranspose			},

/* matrices2.c */
{	"MAKE-SWEEP-MATRIX",		S, xsmakesweepmatrix		},
{	"SWEEP-OPERATOR",		S, xssweepoperator		},

/* basics.c */
{	"SEQUENCEP",			S, xssequencep			},
{	"COPY-VECTOR",			S, xscopyvector			},
{	"COPY-ARRAY",			S, xscopyarray			},
{	"SPLIT-LIST",			S, xssplitlist			},
{	"WHICH",			S, xswhich			},
{	"ISEQ",				S, xsiseq			},
{	"REPEAT",			S, xsrepeat			},
{	"SELECT",			S, xsselect			},
{	"SET-SELECT",			S, xssetselect			},
{	"PERMUTE-ARRAY",		S, xspermutearray		},
#ifdef SAVERESTORE
{	"RESTORE",			S, xsrestore			},
#endif

/* compound.c */
{	"COMPOUND-DATA-P",		S, xscompoundp			},
{	"COMPOUND-DATA-LENGTH",		S, xscompound_length		},
{	"COMPOUND-DATA-SEQ",		S, xscompound_seq		},
{	"MAP-ELEMENTS",			S, xsmap_elements		},

/* math.c */
{	"+",				S, xsradd			},
{	"-",				S, xsrsub			},
{	"*",				S, xsrmul			},
{	"/",				S, xsrdiv			},
{	"REM",				S, xsrrem			},
{	"MOD",				S, xsrmod			},
{	"PMIN",				S, xsrmin			},
{	"PMAX",				S, xsrmax			},
{	"^",				S, xsrexpt			},
{	"**",				S, xsrexpt			},
{	"EXPT",				S, xsrexpt			},
{	"LOG",				S, xsrlog			},

{	"LOGAND",			S, xsrlogand			},
{	"LOGIOR",			S, xsrlogior			},
{	"LOGXOR",			S, xsrlogxor			},
{	"LOGNOT",			S, xsrlognot			},

{	"ABS",				S, xsrabs			},
{	"1+",				S, xsradd1			},
{	"1-",				S, xsrsub1			},
{	"SIN",				S, xsrsin			},
{	"COS",				S, xsrcos			},
{	"TAN",				S, xsrtan			},
{	"EXP",				S, xsrexp			},
{	"SQRT",				S, xsrsqrt			},
{	"TRUNCATE",			S, xsrfix			},
{	"FLOAT",			S, xsrfloat			},
{	"RANDOM",			S, xsrrand			},
{	"FLOOR",			S, xsrfloor			},
{	"CEILING",			S, xsrceil			},
{	"ROUND",			S, xsrround			},
{	"ASIN",				S, xsrasin			},
{	"ACOS",				S, xsracos			},
{	"ATAN",				S, xsratan			},
{	"PHASE",			S, xsrphase			},

{	"MINUSP",			S, xsrminusp			},
{	"ZEROP",			S, xsrzerop			},
{	"PLUSP",			S, xsrplusp			},
{	"EVENP",			S, xsrevenp			},
{	"ODDP",				S, xsroddp			},

{	"<",				S, xsrlss			},
{	"<=",				S, xsrleq			},
{	"=",				S, xsrequ			},
{	"/=",				S, xsrneq			},
{	">=",				S, xsrgeq			},
{	">",				S, xsrgtr			},

/* objects.c */
{	"KIND-OF-P",			S, xskind_of_p			},
{	"SLOT-VALUE",			S, xsslot_value			},
{	"MAKE-OBJECT",			S, xsmake_object		},
{	"SEND",				S, xmsend			},
{	"SEND-SUPER",			S, xmsendsuper			},
{	"CALL-NEXT-METHOD",		S, xscall_next			},
{	"CALL-METHOD",			S, xscall_method		},
{	"DEFMETH",			F, xsdefmeth			},
{	"DEFPROTO",			F, xsdefproto			},

/* optimize.c */
#ifdef OPTIMIZE
{	"BRACKET-SEARCH",		S, xsbracket_search		},
{	"GOLDEN-SEARCH",		S, xsgolden_search		},
{	"PARABOLIC-SEARCH",		S, xsparabolic_search		},
#endif OPTIMIZE

/* sortdata.c */
{	"SORT-DATA",			S, xssortdata			},
{	"ORDER",			S, xsorder			},
{	"RANK",				S, xsrank			},

/* statistics.c */
{	"QUANTILE",			S, xsquantile			},
{	"SUM",				S, xssum			},
{	"PROD",				S, xsprod			},
{	"MIN",				S, xsmin			},
{	"MAX",				S, xsmax			},
{	"COUNT-ELEMENTS",		S, xscount			},
{	"ELEMENT-SEQ",			S, xselement_seq		},
{	"IF-ELSE",			S, xsifelse			},
{	"MEAN",				S, xsmean			},
{	"SAMPLE",			S, xssample			},

/* uni.c */
{	"MAKE-RANDOM-STATE",		S, xsmake_random_state		},
{	"RANDOM-STATE-P",		S, xsrandom_state_p		},

/* windows.c */
{	"SCREEN-SIZE",			S, xsscreen_size		},
{	"SCREEN-HAS-COLOR",		S, xsscreen_has_color		},
{	"FLUSH-GRAPHICS",		S,  xsflush_graphics		},

/* xsiviewwindow.c */
{	"RESET-GRAPHICS-BUFFER",	S, iview_window_reset_buffer	},
{	"MAKE-COLOR",			S, gw_make_color		},
{	"FREE-COLOR",			S, gw_free_color		},
{	"MAKE-CURSOR",			S, gw_make_cursor		},
{	"FREE-CURSOR",			S, gw_free_cursor		},

/* xsiviewinternal.c */
{	"UNLINK-ALL-WINDOWS",		S, iview_unlink_all_windows	},

/* xsiview.c */
{	"GET-NICE-RANGE",		S, iview_get_nice_range		},

/* xsgraphics.c */
{	"HISTOGRAM",			S, xshistogram			},
{	"PLOT-POINTS",			S, xsplot_points		},
{	"PLOT-LINES",			S, xsplot_lines			},
{	"SPIN-PLOT",			S, xsspin_plot			},
{	"SCATTERPLOT-MATRIX",		S, xsscatterplot_matrix		},
{	"NAME-LIST",			S, xsnamelist			},
#ifdef UNIX
{	"GNU-PLOT-POINTS",		S, gnupointplot			},
{	"GNU-PLOT-LINES",	       	S, gnulineplot			},
#endif UNIX

#ifdef MACINTOSH
/* macxsgraph.c */
{	"PICK-COLOR",			S, xspick_color			},

/* macdynload.c */
{	"OPEN-RESOURCE-FILE",		S, xsopen_resfile		},
{	"CLOSE-RESOURCE-FILE",		S, xsclose_resfile		},
{	"CALL-CFUN",			S, xscall_cfun			},
#endif MACINTOSH

#ifdef X11WINDOWS
{	"PARSE-COLOR",			S, xsparse_color		},
{	"BEST-CURSOR-SIZE",		S, xsbest_cursor_size		},
{	"BITMAP-FROM-FILE",		S, xsbitmap_from_file		},
{	"X11-OPTIONS",			S, xsx11_options		},
#endif X11WINDOWS

#ifdef FOREIGNCALL
{	"DYN-LOAD",			S, xsdynload			},
{	"CALL-CFUN",			S, xscall_cfun			},
{	"CALL-FSUB",			S, xscall_fsub			},
{	"CALL-LFUN",			S, xscall_lfun			},
#endif FOREIGNCALL
#ifdef UNIX
{	"SYSTEM",			S, xssystem			},
#endif UNIX

/* xsbayes.c */
{	"NUMGRAD",			S, xsnumgrad			},
{	"NUMHESS",			S, xsnumhess			},
{	"minfo-isnew",			S, xsminfo_isnew		},
{	"minfo-maximize",		S, xsminfo_maximize		},
{	"minfo-loglap",			S, xsminfo_loglap		},
{	"AX+Y",				S, xsaxpy			},
#endif /* XLISP_ONLY */
