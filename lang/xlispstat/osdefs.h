#ifndef XLISP_ONLY
/* menus.c */
extern LVAL xsmenu_isnew(), xsallocate_menu(), xsdispose_menu(),
  xsinstall_menu(), xsremove_menu(), xsupdate_menu(), xsallocated_p(),
  xsmenu_title(), xsmenu_items(), xsinstalled_p(), xsappend_items(),
  xsdelete_items(), xsmenu_select(), xsmenu_popup(), xsmenu_enabled();

#ifdef MACINTOSH
extern LVAL xsapple_menu_isnew(), xsapple_menu_select();
#endif MACINTOSH

extern LVAL xsitem_isnew(), xsitem_title(), xsitem_key(), xsitem_style(),
  xsitem_mark(), xsitem_installed_p(), xsitem_update(), xsitem_action(),
  xsitem_do_action(), xsitem_enabled();
  
#ifdef MACINTOSH
extern LVAL xsabout_xlisp_stat();
#endif MACINTOSH

/* dialog.c */
extern LVAL xsdialog_isnew(), xsdialog_allocate(), xsdialog_remove(),
  xsdialog_allocated_p(), xsdialog_default_button(), xsdialog_modal(),
  xsdialog_item_do_action(), xsdialog_item_action(), xsbutton_item_isnew(),
  xstoggle_item_isnew(), xstoggle_item_value(), xstext_item_isnew(),
  xstext_item_text(), xschoice_item_isnew(), xschoice_item_value(),
  xsscroll_item_isnew(), xsscroll_item_value(), xsscroll_item_max(),
  xsscroll_item_min(), xsscroll_item_action(),
  xslist_item_isnew(), xslist_item_action(), xslist_item_text(),
  xslist_item_selection();
extern LVAL xssysbeep(), xsnumtostring();

#ifdef MACINTOSH
/* edit.c */
extern LVAL xsopenfiledialog(), xssetfiledialog(), xsfrontwindow(), xshidefrontwindow(),
  xssystem_edit(), xssetvolume();
extern LVAL xslistener_isnew(), xslistener_allocate();
extern LVAL xsedit_window_isnew(), xsedit_window_allocate(), xsedit_window_cut(),
  xsedit_window_copy(),
  xsedit_window_paste(), xsedit_window_revert(), xsedit_window_save(), 
  xsedit_window_save_as(), xsedit_window_save_copy(),
  xsedit_window_selection_stream(), xsedit_window_paste_stream(),
  xsedit_window_flush_window(), xsedit_window_paste_string(),
  xsedit_window_remove(), xsedit_window_activate(), xsedit_window_update(),
  xsedit_window_find();
#endif MACINTOSH

/* experimental.c */
extern LVAL xstime(), xsgetenv(), xs_get_internal_run_time(),
  xs_get_internal_real_time();

/* common.c */
extern LVAL xsapropos(), xsaproposlist(), xsidentity(), xsmklist(), xsadjoin(),
  xsfileposition(), xsformat(), xsforce_output(), xscopylist(), xscopyseq(),
  xsreduce(), xsmap(), xselt(), xscoerce(), xsapply(), xseval(), xsload(),
  xscomplexp(), xsrcomplex(), xsrconjugate(), xsrrealpart(), xsrimagpart(),
  xseql(), xsequal(), xsdefconstant(), xsdefparameter(), xsdefvar(), xsmakunbound(),
  xsfmakunbound(), xsconcatenate(), xssome(), xsevery(), xsnotany(), xsnotevery(),
  xsunion(), xsintersection(), xsset_difference(), xssubsetp(), 
  xsremove_duplicates(), xsbutlast(), xsmake_string(), xsfind(), xsposition(),
  xserror(), xsbreak();

/* commonarrays.c */
extern LVAL xsarrayp(), xsarrayinboundsp(), xsarraydimensions(),
  xsarraydimension(), xsarrayrank(), xsarraytotalsize(), 
  xsarrayrowmajorindex(), xsaref(), xsmakearray();

/* commonmath.c */
extern LVAL xlog(), xfloor(), xceil(), xround(), xasin(), xacos(), xatan();

/* basics.c */
extern LVAL xssequencep(), xscopyvector(), xscopyarray(), xssplitlist(),
  xswhich(), xsiseq(), xsrepeat(), xsselect(), xssetselect(),
  xspermutearray();
#ifdef SAVERESTORE
extern LVAL xsrestore();
#endif

/* compound.c */
extern LVAL xscompoundp(), xscompound_length(), xscompound_seq(),
  xsmap_elements();

/* distributions.c */
extern LVAL xsrnormalcdf(), xsrbetacdf(), xsrgammacdf(), xsrchisqcdf(),
  xsrtcdf(), xsrfcdf(), xsrcauchycdf(), xsrloggamma(), xsrbnormcdf(),
  xsrnormalquant(), xsrcauchyquant(), xsrbetaquant(), xsrgammaquant(), 
  xsrchisqquant(), xsrtquant(), xsrfquant(), xsruniformrand(),
  xsrnormalrand(), xsrcauchyrand(), xsrgammarand(), xsrchisqrand(),
  xsrtrand(), xsrbetarand(), xsrfrand(),
  xsrnormaldens(), xsrcauchydens(), xsrbetadens(), xsrgammadens(),
  xsrchisqdens(), xsrtdens(), xsrfdens(), xsruniformrand();

/* ddistributions.c */
extern LVAL xsrbinomialcdf(), xsrpoissoncdf(), xsrbinomialpmf(), xsrpoissonpmf(),
  xsrbinomialquant(), xsrpoissonquant(), xsrbinomialrand(), xsrpoissonrand();

/* linalg.c */
extern LVAL xslu_decomp(), xslu_solve(), xslu_determinant(), xslu_inverse(), 
  xssv_decomp(), xsqr_decomp(), xschol_decomp(), xsrcondest(),
  xsmake_rotation(), xsspline(), xskernel_dens(), xskernel_smooth(), 
  xsbase_lowess(), xssurface_contour(), xsfft();

/* matrices1.c */
extern LVAL xsmatmult(), xscrossproduct(), xsdiagonal(), xsidentitymatrix(),
  xsouterproduct(), xsrowlist(), xscolumnlist(), xsbindrows(), xsbindcols(),
  xstranspose();

/* matrices2.c */
extern LVAL xssolve(), xsdeterminant(), xsinverse(), xsmakesweepmatrix(),
  xssweepoperator();

/* math.c */
extern LVAL xsradd(), xsrsub(), xsrmul(), xsrdiv(), xsrrem(), xsrmod(),
  xsrmin(), xsrmax(), xsrexpt(), xsrlog();

extern LVAL xsrlogand(), xsrlogior(), xsrlogxor(), xsrlognot();

extern LVAL xsrabs(), xsradd1(), xsrsub1(), xsrsin(), xsrcos(), xsrtan(),
  xsrexp(), xsrsqrt(), xsrfix(), xsrfloat(), xsrrand(), xsrfloor(), xsrceil(),
  xsrround(), xsrasin(), xsracos(), xsratan(), xsrphase();

extern LVAL xsrminusp(), xsrzerop(), xsrplusp(), xsrevenp(), xsroddp();

extern LVAL xsrlss(), xsrleq(), xsrequ(), xsrneq(), xsrgeq(), xsrgtr();

/* objects.c */
extern LVAL xskind_of_p(), xsslot_value(), xsobject_null_method();
extern LVAL xsmake_object(), xsreparent_object(), xshas_slot(), xsadd_slot(),
  xsdelete_slot(), xsmslot_value(), xshas_method(), xsadd_method(),
  xsdelete_method(), xsmessage_method(), xmsend(), xscall_method(), xscall_next(),
  xsshow_object(), xmsendsuper(), xsobject_isnew(), xsparents(),
  xsprecedence_list(), xsobject_slots(), xsobject_methods(), xsdefmeth(),
  xsobject_documentation(), xsdefproto(), xsmakeproto();
  
/* optimize.c */
#ifdef OPTIMIZE
extern LVAL xsbracket_search(), xsgolden_search(), xsparabolic_search();
#endif OPTIMIZE

/* sortdata.c */
extern LVAL xssortdata(), xsorder(), xsrank();

/* statistics.c */
extern LVAL xsquantile(), xssum(), xsprod(), xsmin(), xsmax(), xscount(),
  xselement_seq(), xsifelse(), xsmean(), xssample();

/* uni.c */
extern LVAL xsmake_random_state(), xsrandom_state_p();

/* windows.c */
extern LVAL xsshowwindow(), xshidewindow();
extern LVAL  xsscreen_size(), xsscreen_has_color(), xswindow_title(),
  xswindow_location(), xswindow_size(), xswindow_frame_location(),
  xswindow_frame_size(), xsflush_graphics();

/* xsiviewwindow.c */
extern LVAL iview_window_isnew(), iview_window_allocate();
extern LVAL xsiview_window_update(), xsiview_window_activate();
extern LVAL iview_window_idle_on();
extern LVAL iview_window_menu();
extern LVAL iview_window_remove(), iview_window_while_button_down(),
  iview_window_show_window();
extern LVAL iview_window_canvas_width(), iview_window_canvas_height(),
  iview_window_line_type(), iview_window_draw_mode(),
  iview_window_draw_color(), iview_window_back_color(), iview_window_use_color(),
  iview_window_reverse_colors(), iview_window_view_rect(),
  iview_window_line_width();
extern LVAL iview_window_has_h_scroll(), iview_window_has_v_scroll(),
  iview_window_scroll(), iview_window_h_scroll_incs(),
  iview_window_v_scroll_incs();
extern LVAL iview_window_draw_line(), iview_window_draw_point(),
  iview_window_erase_rect(),
  iview_window_frame_rect(), iview_window_paint_rect(),
  iview_window_erase_oval(), iview_window_frame_oval(),
  iview_window_paint_oval(), iview_window_erase_arc(),
  iview_window_frame_arc(), iview_window_paint_arc(),
  iview_window_erase_poly(), iview_window_frame_poly(),
  iview_window_paint_poly();
extern LVAL iview_window_text_ascent(), iview_window_text_descent(), 
  iview_window_text_width(),
  iview_window_draw_string(), iview_window_draw_string_up(),
  iview_window_draw_text(), iview_window_draw_text_up();
extern LVAL iview_window_draw_symbol(), iview_window_replace_symbol();
extern LVAL iview_window_start_buffering(), iview_window_buffer_to_screen();
extern LVAL iview_window_clip_rect(), iview_window_cursor(),
  iview_window_reset_buffer();
extern LVAL iview_window_dump_image();
extern LVAL gw_make_color(), gw_free_color(), gw_make_cursor(),
  gw_free_cursor(), gw_draw_bitmap();

/* xsiviewinternal.c */
extern LVAL iview_isnew(), iview_allocate();
extern LVAL iview_content_rect(), iview_content_origin(),
  iview_content_variables(), iview_click_range(), iview_mouse_mode(),
  iview_showing_labels(), iview_margin(), iview_fixed_aspect(), iview_dirty();
extern LVAL iview_x_axis(), iview_y_axis();
extern LVAL iview_brush(), iview_erase_brush(), iview_draw_brush(),
  iview_move_brush(), iview_resize_brush();
extern LVAL iview_std_click(), iview_std_motion(), iview_do_click(),
  iview_do_motion(), iview_unselect_all_points(),
  iview_erase_selection(), iview_mask_selection(),
  iview_unmask_all_points(), iview_show_all_points(),
  iview_all_points_showing(), iview_all_points_unmasked(),
  iview_any_points_selected();
extern LVAL iview_linked(), iview_links(), iview_unlink_all_windows();

/* xsiview.c */
extern LVAL iview_num_variables(), iview_variable_label(), iview_range(),
  iview_scaled_range(), iview_screen_range(), iview_transformation(),
  iview_apply_transformation();

extern LVAL iview_add_points(), iview_clear_points(), iview_num_points(), 
  iview_point_coordinate(), iview_point_screen_coordinate(),
  iview_point_transformed_coordinate(),
  iview_point_masked(), iview_point_color(), iview_point_state(),
  iview_point_screen_state(), iview_point_marked(),
  iview_point_label(), iview_point_symbol();
  
extern LVAL iview_add_lines(), iview_clear_lines(), iview_num_lines(),
  iview_line_coordinate(), iview_line_screen_coordinate(),
  iview_line_transformed_coordinate(),
  iview_line_masked(), iview_line_color(), iview_line_next(),
  iview_line_type(), iview_line_width();

#ifdef USESTRINGS
extern LVAL iview_add_strings(), iview_clear_strings(), iview_num_strings(),
  iview_string_coordinate(), iview_string_screen_coordinate(),
  iview_string_transformed_coordinate(),
  iview_string_masked(), iview_string_color(), iview_string_modifiers();
#endif /* USESTRINGS */

extern LVAL iview_draw_data_points(), iview_draw_data_lines();
#ifdef USESTRINGS
extern LVAL iview_draw_data_strings();
#endif /* USESTRINGS */

extern LVAL iview_std_resize(), iview_std_redraw(),
  iview_std_redraw_background(), iview_std_redraw_content(), 
  iview_std_adjust_screen(), iview_std_adjust_points_in_rect(),
  iview_std_adjust_screen_point(), iview_std_mark_points_in_rect();

extern LVAL iview_rotate_2();
extern LVAL iview_get_nice_range();
extern LVAL iview_adjust_depth_cuing();

/* xsiviewcustom.c */
extern LVAL iview_spin_allocate(),
  iview_spin_content_variables(), 
  iview_spin_showing_axes(), iview_spin_depth_cuing(),
  iview_spin_resize(), 
  iview_spin_angle(), iview_spin_rotate(),
  iview_spin_redraw_content();
extern LVAL iview_scatmat_allocate(), iview_scatmat_resize(),
  iview_scatmat_redraw_content(),iview_scatmat_click(), 
  iview_scatmat_motion(),
  iview_scatmat_add_points(), iview_scatmat_add_lines(),
  iview_scatmat_adjust_screen_point(), iview_scatmat_adjust_points_in_rect(),
  iview_scatmat_mark_points_in_rect();
extern LVAL iview_list_allocate(), iview_list_redraw_content(), 
  iview_list_add_points(), iview_list_adjust_screen_point(),
  iview_list_adjust_points_in_rect(), iview_list_mark_points_in_rect();
extern LVAL iview_hist_isnew(), iview_hist_allocate(), iview_hist_add_points(),
  iview_hist_clear_points(), iview_hist_resize(), 
  iview_hist_redraw_content(), iview_hist_adjust_screen(),
  iview_hist_num_bins(), iview_hist_bin_counts(),
  iview_hist_adjust_screen_point(), iview_hist_adjust_points_in_rect(),
  iview_hist_mark_points_in_rect();
extern LVAL iview_plot2d_add_points(), iview_plot2d_add_lines();
#ifdef USESTRINGS
extern LVAL iview_scatmat_add_strings(), iview_plot2d_add_strings();
#endif /* USESTRINGS */
  
/* xsgraphics.c */
extern LVAL iview_point_selected(), iview_point_hilited(),
  iview_point_showing(),
  iview_hist_adjust_to_data(), iview_plot2d_adjust_to_data(),
  iview_adjust_to_data(), iview_spin_draw_axes();
#ifdef MACINTOSH
extern LVAL iview_window_copy_to_clip();
#endif MACINTOSH
extern LVAL xshistogram(), xsplot_points(), xsplot_lines(), xsspin_plot(),
  xsscatterplot_matrix(), xsnamelist();
extern LVAL iview_visible_range(), iview_scale_to_range(), iview_scale(),
  iview_shift(), iview_clear_masks(), iview_slice_variable(), 
  iview_real_to_screen(), iview_screen_to_real(), iview_scaled_to_screen(), 
  iview_screen_to_scaled(), iview_points_in_rect(),
  iview_window_drag_grey_rect(), iview_points_showing(),
  iview_points_hilited(), iview_points_selected(),
  iview_cycle_selection_symbols();

#ifdef MACINTOSH
/* macxsgraph.c */
extern LVAL xspick_color();

/* macdynload.c */
extern LVAL xsopen_resfile(), xsclose_resfile(), xscall_cfun();
#endif MACINTOSH
#ifdef X11WINDOWS
extern LVAL xsparse_color(), xsbest_cursor_size(), xsbitmap_from_file(),
  xsx11_options();
#endif X11WINDOWS

#ifdef UNIX
extern LVAL gnupointplot(), gnulineplot();
#endif UNIX
#ifdef FOREIGNCALL
extern LVAL xsdynload(), xscall_cfun(), xscall_fsub(), xscall_lfun();
#endif FOREIGNCALL
#ifdef UNIX
extern LVAL xssystem();
#endif UNIX

/* xsbayes.c */
extern LVAL xsnumgrad(), xsnumhess(), xsminfo_isnew(),
  xsminfo_maximize(), xsminfo_loglap(), xsaxpy();
#endif /* XLISP_ONLY */
