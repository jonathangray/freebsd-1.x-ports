/* #define USE_OWN_GUS_VOL */
/*
 * xmp -- play midi files
 *	Greg Lee, June, 1993
 *	file menu code adapted from "menu_dir2.c", Dan Heller,
 *		XView Programming Manual, appendix F.
 */

/*
 * (some configuration options)
 */
/* comment out the following for true file names in file menu */
/*#define TRY_REAL_FILENAME*/

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/icon.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/cms.h>
#include <xview/font.h>
#include <sspkg/canshell.h>
#include <sspkg/drawobj.h>
#include <sspkg/disp_list.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/dir.h>
#ifdef USE_SHM
#include <sys/ipc.h>	/* ftok */
#include <sys/shm.h>	/* shmget .. */
#endif
#include <X11/Xos.h>

#include <ctype.h>
#define NO_LC_DEFINES
#include "midifile.h"
#include "adagio.h"
#include "allphase.h"
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif
#include "xmp.bits"
#include "dot.bits"
#include "stipple.bits"

#define MAIN_COLOR_GREEN (CMS_CONTROL_COLORS+0)
#define MAIN_COLOR_RED (CMS_CONTROL_COLORS+1)
#define MAIN_COLOR_BLACK (CMS_CONTROL_COLORS+2)

void phase2();

static Frame frame, settings_frame;
static Xv_Font font_small;
static Panel main_panel, settings_panel;
static Frame meter_frame;
static Panel meter_panel;
static Panel_item meter_clr_button, meter_set_button, meter_wte_button;
static Canvas_shell meter_shell;
static Drawarea meter_area;
#ifndef USE_SHM
static int mplay_pipe[2];
#endif
static Cms meter_cms;
static int meter_visible = FALSE;
static Notify_client mplayer = (Notify_client)10;
static Frame carousel_frame;
static Panel carousel_panel;
static Panel_item carousel_list;
static int carousel_visible = FALSE;
static int carousel_auto = TRUE;
static int recycle_carousel = FALSE;
static Menu file_menu;
static Panel_item midi_file_item;
#ifdef USE_SHM
static Panel_item running_time_item;
#endif
static Panel_item dot_play_button;
static Panel_item file_menu_button;
static Panel_item meter_color_choice;
static Panel_item meter_ctlr_choice;
static char midi_file_selected = FALSE;
static char midi_file_playing = FALSE;
static Xv_notice misc_notice;
static Rect *working_rect;

#ifdef USE_SHM
int *shm_setting_pstereo;
int *shm_setting_gus_tuning;
int *shm_setting_gus_volume;
int *shm_setting_meter_color;
int *shm_setting_reverb;
int *shm_setting_chorus_spread;
int *shm_setting_vibrato_depth;
int *shm_setting_vibrato_speed;
int *shm_setting_vibrato_sweep;
int *shm_expression;
int *shm_main_volume;
int *shm_chorus_depth;
int *shm_reverberation;
int *shm_ext_pan;
long *shm_running_time;
#endif

static unsigned xmp_time = 0;
static int xmp_epoch = -1;
static unsigned xxmp_epoch = 0;

static struct itimerval timer;
static Notify_value
timer_service()
{
	int last = xv_get(carousel_list, PANEL_LIST_NROWS);
	static void play_midi_file();
	static void update_meter();

	update_meter();

	xmp_time++;
	if ((xmp_time % 6) == 0 && midi_file_playing == TRUE)
		xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_BLACK, NULL);
	if ((xmp_time % 6) == 3 && midi_file_playing == TRUE)
		xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_RED, NULL);

	if (!midi_file_playing && !midi_file_selected && last > 0 && carousel_auto)
		play_midi_file((Panel_item)0, 0, (Event *)0);
#ifdef USE_SHM
	if (running_time != *shm_running_time) {
		static char rt_string[18];
		running_time = *shm_running_time;
		if (running_time >= 0)
		    sprintf(rt_string, "%d:%02d",
			running_time / 6000, (running_time/100) % 60);
		else
		    sprintf(rt_string, "Track %2d", -running_time);
		xv_set(running_time_item, PANEL_LABEL_STRING, rt_string, NULL);
	}
#endif

	return(NOTIFY_DONE);
}
static void
start_timer()
{
	timer.it_value.tv_usec = 100000;
	timer.it_interval.tv_usec = 100000;
	notify_set_itimer_func(frame, timer_service,
		ITIMER_REAL, &timer, NULL);
}
static void
message(char *prompt)
{
	xv_set(misc_notice,
		NOTICE_MESSAGE_STRING, prompt,
		XV_SHOW, TRUE, NULL);
}

static int child_pid = -1;
#ifndef USE_SHM
static int total_packets = 0;
#endif

static void finish_up_playing()
{
	child_pid = -1;
	xv_set(midi_file_item, PANEL_BUSY, FALSE, NULL);
	xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_GREEN, NULL);
	xv_set(dot_play_button, PANEL_BUSY, FALSE, NULL);
	midi_file_playing = FALSE;
	if (carousel_visible && carousel_auto && xv_get(carousel_list, PANEL_LIST_NROWS))
		midi_file_selected = FALSE;
#ifdef USE_SHM
	*shm_running_time = 0;
#else
	total_packets = 0;
	notify_set_input_func(mplayer, NOTIFY_FUNC_NULL,
		mplay_pipe[0]);
#endif
	xmp_epoch = -1;
}

static void
quit_xmp()
{
	int status;

	if (child_pid >= 0) {
		kill(child_pid, SIGKILL /*SIGINT*/);
		waitpid(child_pid, &status, 0);
		finish_up_playing();
		return;
	}
	exit(0);
}

static Notify_value
wait3_handle(Notify_client me, int pid, int *status, struct rusage *rusage)
{
	if (WIFEXITED(*status)) {
		finish_up_playing();
		return(NOTIFY_DONE);
	}
	return(NOTIFY_IGNORED);
}

#define CHECK_EACH_MID

#ifdef CHECK_EACH_MID
static int
it_is_not_midi(char *path)
{
	int f;
	static char mbuff[5];

	if ((f = open(path, O_RDONLY, 0)) == -1) return(1);
#ifdef READ_MODS
	if (!strcmp(".mod", path + strlen(path) - 4)) {
		close(f);
		return(0);
	}
#endif
	if (read(f, mbuff, 4) != 4) {
		close(f);
		return(2);
	}
	mbuff[4] = '\0';
	if (strcmp(mbuff, "MThd")) {
		close(f);
		return(3);
	}
	close(f);
	return(0);
}
#endif

static int
crsltoplay(char *fname, char *path)
{
#ifndef CHECK_EACH_MID
	int f;
#endif
	int prob = 0;
	static char ybuff[80];

	midi_file_selected = TRUE;
#ifdef CHECK_EACH_MID
	if ((prob = it_is_not_midi(path)))
		midi_file_selected = FALSE;
#else
	if ((f = open(path, O_RDONLY, 0)) == -1) {
		prob = 1;
		midi_file_selected = FALSE;
	}
	if (midi_file_selected && read(f, ybuff, 4) != 4) {
		prob = 2;
		midi_file_selected = FALSE;
	}
	ybuff[4] = '\0';
	if (midi_file_selected && strcmp(ybuff, "MThd")) {
		prob = 3;
		midi_file_selected = FALSE;
	}
	if (f != -1) close(f);
#endif
	strcpy(ybuff, "play: ");
	if (midi_file_selected) strcat(ybuff, fname);
	xv_set(midi_file_item, PANEL_LABEL_STRING, ybuff, NULL);

	if (midi_file_selected) {
		xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_GREEN, NULL);
		strcpy(midi_file_path, path);
		return(TRUE);
	}
	midi_file_path[0] = '\0';
/* playing from carousel, pass over bad files silently (??) */
	if (!carousel_visible)
	switch (prob) {
		case 1:
			message("can't open file");
			break;
		case 2:
		case 3: message("not a midi file");
			break;
	}
	return(FALSE);
}

static void
play_midi_file(Panel_item it, int value, Event *ev)
{
	static int do_midi();
#ifndef USE_SHM
	static Notify_value read_mplayer();
#endif
	if (midi_file_playing) return;
	if (!midi_file_selected && carousel_visible && carousel_auto) {
		int last = xv_get(carousel_list, PANEL_LIST_NROWS);
		if (last) {
			int ret;
			char *path = (char *)xv_get(carousel_list, 
				PANEL_LIST_CLIENT_DATA, 0, NULL);
			if (path == NULL) {
				xv_set(carousel_list, PANEL_LIST_DELETE, 0, NULL);
				return;
			}
			ret = crsltoplay((char *)xv_get(carousel_list, PANEL_LIST_STRING, 0),
				path);
			if (recycle_carousel && ret) {
				if (last > 1) {
					char buf[MAXPATHLEN];
					strcpy(buf, (char *)xv_get(carousel_list,
						PANEL_LIST_STRING, 0));
					xv_set(carousel_list,
						PANEL_LIST_DELETE, 0,
						PANEL_LIST_INSERT, last - 1,
						PANEL_LIST_STRING, last - 1, buf,
						PANEL_LIST_CLIENT_DATA, last - 1, path,
						NULL);
				}
			}
			else {
				free(path);
				xv_set(carousel_list, PANEL_LIST_DELETE, 0, NULL);
			}
			if (ret == FALSE) return;
		}
		else
		{
			message("use the midi file button to select a file");
			return;
		}
	}
	if (!midi_file_selected) return;

	xmp_epoch = -1;
	midi_file_playing = TRUE;

#ifndef USE_SHM
	pipe(mplay_pipe);
#endif
	switch (child_pid = fork()) {
		case -1: perror("fork"); exit(1);
		case  0:
#ifndef USE_SHM
			 dup2(mplay_pipe[1], 1);
			 close(mplay_pipe[0]);
#endif
			 (void)do_midi(midi_file_path);
			 _exit(0);
		default:
#ifndef USE_SHM
			 close(mplay_pipe[1]);
			 (void)notify_set_input_func(mplayer, read_mplayer, mplay_pipe[0]);
#endif
			 (void)notify_set_wait3_func(mplayer, wait3_handle, child_pid);
	}
}

static void
toggle_options(Panel_item item, unsigned int value, Event *ev)
{
	static int settings_visible = FALSE;
	Rect r;
	int opt = 1;

	frame_get_rect(frame, &r);

	if (value & opt) {
		if (!carousel_visible) {
			xv_set(carousel_frame, XV_X, r.r_left-4,
				XV_Y, r.r_top+r.r_height, NULL);
		}
		carousel_visible = TRUE;
	}
	else if (carousel_visible) {
		carousel_visible = FALSE;
	}
	xv_set(carousel_frame, XV_SHOW, (value & opt)? TRUE:FALSE, NULL);
	opt <<= 1;
	if (value & opt) {
		if (!meter_visible) {
			xv_set(meter_frame, XV_X, r.r_left+188,
				XV_Y, r.r_top+r.r_height, NULL);
		}
		meter_visible = TRUE;
	}
	else meter_visible = FALSE;
	xv_set(meter_frame, XV_SHOW, (value & opt)? TRUE:FALSE, NULL);
	opt <<= 1;
	if (value & opt) {
		if (!settings_visible) {
			xv_set(settings_frame, XV_X, r.r_left+r.r_width+3,
				XV_Y, r.r_top, NULL);
		}
		settings_visible = TRUE;
	}
	else settings_visible = FALSE;
	xv_set(settings_frame, XV_SHOW, (value & opt)? TRUE:FALSE, NULL);

}
static int req_no_solo = FALSE, req_piano_only = FALSE;
static void
choose_synth_opt(Panel_item item, unsigned int value, Event *ev)
{
	exclude_fm = ((value & 1) == 0);
	exclude_gus = ((value & 2) == 0);
	extflag = ((value & 4) != 0);
	setting_drum_rolls = ((value & 8) != 0);
	setting_pstereo = ((value & 16) != 0);
	setting_4op_mode = ((value & 32) != 0);
	req_no_solo = ((value & 64) != 0);
	req_piano_only = ((value & 128) != 0);
#ifdef USE_SHM
	*shm_setting_pstereo = setting_pstereo;
#endif
}

static void
reverb_proc(Panel_item item, int value, Event *ev)
{
	setting_reverb = value;
#ifdef USE_SHM
	*shm_setting_reverb = setting_reverb;
#endif
}
static void
chorus_spread_proc(Panel_item item, int value, Event *ev)
{
	setting_chorus_spread = value;
#ifdef USE_SHM
	*shm_setting_chorus_spread = setting_chorus_spread;
#endif
}
static void
vibrato_depth_proc(Panel_item item, int value, Event *ev)
{
	setting_vibrato_depth = value;
#ifdef USE_SHM
	*shm_setting_vibrato_depth = setting_vibrato_depth;
#endif
}
static void
vibrato_speed_proc(Panel_item item, int value, Event *ev)
{
	setting_vibrato_speed = value;
#ifdef USE_SHM
	*shm_setting_vibrato_speed = setting_vibrato_speed;
#endif
}
static void
vibrato_sweep_proc(Panel_item item, int value, Event *ev)
{
	setting_vibrato_sweep = value;
#ifdef USE_SHM
	*shm_setting_vibrato_sweep = setting_vibrato_sweep;
#endif
}
static void
gus_voices_proc(Panel_item item, int value, Event *ev)
{
	setting_gus_voices = value;
}
static void
gus_tuning_proc(Panel_item item, int value, Event *ev)
{
	setting_gus_tuning= value;
#ifdef USE_SHM
	*shm_setting_gus_tuning = setting_gus_tuning;
#endif
}
#ifdef USE_OWN_GUS_VOL
static void
gus_volume_proc(Panel_item item, int value, Event *ev)
{
	setting_gus_volume= value;
#ifdef USE_SHM
	*shm_setting_gus_volume = setting_gus_volume;
#endif
}
#endif
static void
meter_color_proc(Panel_item item, int value, Event *ev)
{
	static void reset_controllers();
	if (value == USER_NUM_CTLS) {
		reset_controllers();
		xv_set(meter_color_choice, PANEL_VALUE, setting_meter_color, NULL);
		return;
	}
	setting_meter_color = value;
	xv_set(meter_color_choice, PANEL_VALUE, setting_meter_color, NULL);
	xv_set(meter_ctlr_choice, PANEL_VALUE, setting_meter_color, NULL);
#ifdef USE_SHM
	*shm_setting_meter_color = setting_meter_color;
#endif
}
static void
meter_column_proc(Panel_item item, int value, Event *ev)
{
	setting_meter_column = value;
}
static int setting_meter_sync = 0;
static void
meter_sync_proc(Panel_item item, int value, Event *ev)
{
	setting_meter_sync = value;
}

int
main(int argc, char *argv[])
{
	DIR *dirp;
	char *top_dir;
	Menu_item mi;
	static Menu_item add_path_to_menu();
	Icon icon;
	Rect icon_rect;
	Server_image closed_image, dot_image;
	Cms panel_cms;
	static void setup_settings();
	static void setup_meter();
	static void setup_carousel();

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	frame = (Frame)xv_create(
		XV_NULL,
		FRAME,
		XV_X,		50,
		XV_Y,		30,
		XV_WIDTH,	400,
		FRAME_LABEL,	argv[0],
		NULL);

	closed_image = xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, xmp_width,
		XV_HEIGHT, xmp_height,
		SERVER_IMAGE_X_BITS, xmp_bits,
		NULL);
	icon = (Icon)xv_create(frame, ICON,
		ICON_IMAGE,	closed_image,
		ICON_TRANSPARENT, TRUE,
		NULL);
	icon_rect.r_top = 10;
	icon_rect.r_left = 118;
	icon_rect.r_width = xmp_width;
	icon_rect.r_height = xmp_height;
	xv_set(frame,
		FRAME_ICON, icon,
		FRAME_CLOSED_RECT, &icon_rect,
		NULL);
	panel_cms = (Cms)xv_create(XV_NULL, CMS,
		CMS_SIZE,		CMS_CONTROL_COLORS + 3,
		CMS_CONTROL_CMS,	TRUE,
		CMS_NAMED_COLORS,	"green", "red", "black", NULL,
		NULL);

	font_small = (Xv_Font)xv_find(frame, FONT,
		FONT_FAMILY,	FONT_FAMILY_DEFAULT,
		FONT_STYLE,	FONT_STYLE_NORMAL,
		FONT_SCALE,	WIN_SCALE_SMALL,
		NULL);

	main_panel = (Panel)xv_create(frame, PANEL,
		WIN_CMS,	panel_cms,
		XV_X,		0,
		XV_Y,		0,
		NULL);
/*
 * first row
 */
	(void)xv_create(main_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"quit",
		PANEL_NOTIFY_PROC,	quit_xmp,
		NULL);

	if (argc > 1) top_dir = argv[1];
	else {
		top_dir = MIDIDIR;
		if (!(dirp = opendir(top_dir))) top_dir = ".";
		else closedir(dirp);
	}
	if ( (mi = add_path_to_menu(top_dir)) == XV_NULL ) {
		fprintf(stderr, "can't open directory %s\n", top_dir);
		exit(1);
	}
#ifdef CHECK_EACH_MID
	if (mi == (Menu_item)-1 || mi == (Menu_item)-2) {
		fprintf(stderr, "can't open directory %s\n", top_dir);
		exit(1);
	}
#endif
	file_menu = (Menu)xv_get(mi, MENU_PULLRIGHT);
	free((char *)xv_get(mi, MENU_CLIENT_DATA));
	xv_destroy(mi);

	file_menu_button = xv_create(main_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"midi file",
		PANEL_ITEM_MENU,	file_menu,
		NULL);

	setup_settings();
	setup_meter();
	setup_carousel();

	(void)xv_create(main_panel, PANEL_TOGGLE,
		PANEL_CHOICE_STRINGS,	"carousel", "meter", "settings", NULL,
		PANEL_NOTIFY_PROC,	toggle_options,
		PANEL_VALUE,		0,
		NULL);
/*
 * second row
 */

	dot_image = xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, dot_width,
		XV_HEIGHT, dot_height,
		SERVER_IMAGE_X_BITS, dot_bits,
		NULL);
	dot_play_button = (Panel_item)xv_create(main_panel, PANEL_BUTTON,
		PANEL_NEXT_ROW,		-1,
		PANEL_LABEL_IMAGE,	dot_image,
		PANEL_NOTIFY_PROC,	play_midi_file,
		NULL);
	xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_RED, NULL);

	midi_file_item = (Panel_item)xv_create(main_panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING,	"play:             ",
		PANEL_NOTIFY_PROC,	play_midi_file,
		NULL);
#ifdef USE_SHM
	running_time_item = (Panel_item)xv_create(main_panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING,	"0:00     ",
		XV_X,			400 - 90,
		NULL);
#endif
/*
 * end of rows
 */

	misc_notice = (Xv_notice)xv_create(main_panel, NOTICE,
		NOTICE_LOCK_SCREEN,	TRUE,
		NOTICE_NO_BEEPING,	TRUE,
		XV_SHOW,		FALSE,
		NULL);

	window_fit(main_panel);
	window_fit(frame);

	start_timer();

	window_main_loop(frame);
	return(0);
}

static void
setup_settings()
{
	Panel_item settings_item;
	unsigned int def_value;
	void card_init(char *devname);

	settings_frame = (Frame) xv_create(frame, FRAME,
		XV_X,		420,
		XV_Y,		50,
		XV_WIDTH,	300,
		WIN_TOP_LEVEL_NO_DECOR,	TRUE,
		FRAME_LABEL,		"settings",
		NULL);

	settings_panel = (Panel)xv_create(settings_frame, PANEL,
		PANEL_ITEM_Y_GAP,	1,
		PANEL_LAYOUT,	PANEL_VERTICAL,
		XV_FONT, font_small,
		NULL);

	midi_file_path[0] = '\0';
	card_init("/dev/sequencer");
	close(seq_fd);
	exclude_fm = (sb_dev < 0);
	exclude_gus = (gus_dev < 0);
	extflag = (ext_dev >= 0);

	def_value = 0;
	if (!exclude_fm) def_value |= 1;
	if (!exclude_gus) def_value |= 2;
	if (extflag) def_value |= 4;
	if (setting_drum_rolls) def_value |= 8;
	if (setting_pstereo) def_value |= 16;
	if (setting_4op_mode) def_value |= 32;
	if (no_solo) def_value |= 64;
	if (piano_only) def_value |= 128;

	settings_item = (Panel_item)xv_create(settings_panel, PANEL_CHECK_BOX,
		/*PANEL_LAYOUT,		PANEL_VERTICAL,*/
		PANEL_CHOICE_NCOLS,	2,
		PANEL_CHOICE_STRINGS,
			"fm card",
			"gus card",
			"external synth",
			"drum rolls",
			"pseudo stereo",
			"4op fm",
			"(not used)",
			"piano only",
			NULL,
		PANEL_NOTIFY_PROC,	choose_synth_opt,
		PANEL_VALUE,		def_value,
		NULL);

	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"reverb delay",
		PANEL_VALUE,		setting_reverb,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	reverb_proc,
		NULL);
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"chorus spread",
		PANEL_VALUE,		setting_chorus_spread,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	chorus_spread_proc,
		NULL);
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"vibrato depth",
		PANEL_VALUE,		setting_vibrato_depth,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	vibrato_depth_proc,
		NULL);
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"vibrato speed",
		PANEL_VALUE,		setting_vibrato_speed,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	vibrato_speed_proc,
		NULL);
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"vibrato sweep",
		PANEL_VALUE,		setting_vibrato_sweep,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	vibrato_sweep_proc,
		NULL);
	if (gus_dev >= 0)
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"gus voices",
		PANEL_VALUE,		setting_gus_voices,
		PANEL_MIN_VALUE,	14,
		PANEL_MAX_VALUE,	32,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		18,
		PANEL_NOTIFY_PROC,	gus_voices_proc,
		NULL);
#ifdef USE_OWN_GUS_VOL
	if (gus_dev >= 0)
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"gus volume",
		PANEL_VALUE,		setting_gus_volume,
		PANEL_MIN_VALUE,	0,
		PANEL_MAX_VALUE,	100,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	gus_volume_proc,
		NULL);
#endif
	if (gus_dev >= 0)
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"gus tuning",
		PANEL_VALUE,		setting_gus_tuning,
		PANEL_MIN_VALUE,	-1000,
		PANEL_MAX_VALUE,	1000,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		10,
		PANEL_NOTIFY_PROC,	gus_tuning_proc,
		NULL);
	meter_color_choice = (Panel_item)xv_create(settings_panel, PANEL_CHOICE_STACK,
		PANEL_LABEL_STRING,	"meter color",
		PANEL_CHOICE_STRINGS,
			"expression",
			"main volume",
			"chorus depth",
			"reverberation",
			"stereo pan",
			"(reset all)",
			NULL,
		PANEL_VALUE,		setting_meter_color,
		PANEL_NOTIFY_PROC,	meter_color_proc,
		NULL);
	working_rect = (Rect *)xv_get(meter_color_choice, XV_RECT);
	(void)xv_create(settings_panel, PANEL_CHOICE_STACK,
		PANEL_LABEL_STRING,	"bar",
		PANEL_CHOICE_STRINGS,
			"channel",
			"pitch",
			"instrument",
			NULL,
		PANEL_VALUE,		0,
		XV_X,			rect_right(working_rect),
		XV_Y,			working_rect->r_top,
		PANEL_NOTIFY_PROC,	meter_column_proc,
		NULL);
	(void)xv_create(settings_panel, PANEL_SLIDER,
		PANEL_LABEL_STRING,	"meter sync",
		PANEL_VALUE,		0,
		PANEL_MIN_VALUE,	-50,
		PANEL_MAX_VALUE,	+50,
		PANEL_SLIDER_WIDTH,	90,
		PANEL_TICKS,		20,
		PANEL_NOTIFY_PROC,	meter_sync_proc,
		NULL);

	window_fit(settings_panel);
	window_fit(settings_frame);
}


static void
carousel_options_proc(Panel_item item, unsigned int value, Event *ev)
{
	recycle_carousel = value & 1;
	if (!carousel_auto && (value & 2)) {
		if (!midi_file_playing) midi_file_selected = FALSE;
	}
	carousel_auto = value & 2;
}

void
carousel_action(Menu menu, Menu_item item)
{	int r, last;
	char *path;
	char buf[MAXPATHLEN];
	char *choice = (char *)xv_get(item, MENU_STRING);
	static char *getfilename(char *);

	if (!strcmp(choice, "sort")) {
		xv_set(carousel_list, PANEL_LIST_SORT, PANEL_FORWARD, NULL);
		return;
	}
	r = (int)xv_get(carousel_list, PANEL_LIST_FIRST_SELECTED);
	if (!strcmp(choice, "clear")) {
		while ((last = xv_get(carousel_list, PANEL_LIST_NROWS))) {
			path = (char *)xv_get(carousel_list, 
				PANEL_LIST_CLIENT_DATA, last - 1, NULL);
			if (path != NULL) free(path);
			xv_set(carousel_list, PANEL_LIST_DELETE, last - 1, NULL);
		}
		return;
	}
	if (r<0) {
		message("please select a file");
		return;
	}
	path = (char *)xv_get(carousel_list, 
		PANEL_LIST_CLIENT_DATA, r, NULL);
	if (path == NULL) {
		xv_set(carousel_list, PANEL_LIST_DELETE, 0, NULL);
		return;
	}
	if (!strcmp(choice, "delete")) {
		free(path);
		xv_set(carousel_list, PANEL_LIST_DELETE, r, NULL);
		return;
	}
	last = xv_get(carousel_list, PANEL_LIST_NROWS);
	if (!last) return;
	strcpy(buf, (char *)xv_get(carousel_list, PANEL_LIST_STRING, r));
	if (!strcmp(choice, "bottom")) {
		if (last < 2) return;
		if (r == last - 1) return;
		xv_set(carousel_list,
			PANEL_LIST_DELETE, r,
			PANEL_LIST_INSERT, last - 1,
			PANEL_LIST_STRING, last - 1, buf,
			PANEL_LIST_CLIENT_DATA, last - 1, path,
			NULL);
		return;
	}
	if (!strcmp(choice, "top")) {
		if (last < 2) return;
		if (r == 0) return;
		xv_set(carousel_list,
			PANEL_LIST_DELETE, r,
			PANEL_LIST_INSERT, 0,
			PANEL_LIST_STRING, 0, buf,
			PANEL_LIST_CLIENT_DATA, 0, path,
			NULL);
		return;
	}
	if (!strcmp(choice, "add *")) {
		int i = 0, j;
		DIR *dirp;
		struct direct *dp;
		struct stat s_buf;
		char *p;
		char n_buf[MAXPATHLEN];
		char *dir_n = strcpy(malloc(strlen(path)+1), path);
		for (i = strlen(dir_n)-1; i > 0 && dir_n[i] != '/'; i--)
			dir_n[i] = '\0';
		if (dir_n[i] != '/') return;
		dir_n[i] = '\0';
		if (!(dirp = opendir(dir_n))) return;
		while ((dp = readdir(dirp))) {

			if (!strcmp(dp->d_name, ".")) continue;
			if (!strcmp(dp->d_name, "..")) continue;
			p = getfilename(dp->d_name);
			if (!strcmp(p, buf)) {
				free(p);
				continue;
			}

			(void)sprintf(n_buf, "%s/%s", dir_n, dp->d_name);
			if (stat(n_buf, &s_buf) == -1 ||
				!(s_buf.st_mode & S_IREAD) ||
				(s_buf.st_mode & S_IFDIR)) {
				free(p);
				continue;
			}
#ifdef CHECK_EACH_MID
			if (it_is_not_midi(n_buf)) {
				free(p);
				continue;
			}
#endif

			xv_set(carousel_list,
				PANEL_LIST_INSERT, last - 1,
				PANEL_LIST_STRING, last - 1, p,
				PANEL_LIST_CLIENT_DATA, last - 1,
					strcpy(malloc(strlen(n_buf)+1), n_buf),
				NULL);
			free(p);
			last++;
		}
		closedir(dirp);
		free(dir_n);
		i = 10 + xv_get(carousel_list, XV_WIDTH);
		j = xv_get(carousel_frame, XV_WIDTH);
		if (j < i) xv_set(carousel_frame, XV_WIDTH, i, NULL);
		return;
	}
}

#define COPS_ON_BUTTON
static void
setup_carousel()
{
	Menu carousel_menu;
	Panel_item toggle_item;

	carousel_frame = (Frame)xv_create(frame, FRAME,
		XV_WIDTH,	168,
		XV_HEIGHT,	280,
		FRAME_LABEL,	"carousel",
		NULL);
	carousel_panel = (Panel)xv_create(carousel_frame, PANEL,
		NULL);

	carousel_list = (Panel_item)xv_create(carousel_panel, PANEL_LIST,
		PANEL_CHOOSE_ONE,	TRUE,
		PANEL_LIST_DISPLAY_ROWS,	6,
		PANEL_LIST_WIDTH,	-1,
		NULL);
	toggle_item = (Panel_item)xv_create(carousel_panel, PANEL_TOGGLE,
		PANEL_NEXT_ROW,		-1,
		PANEL_CHOICE_STRINGS,	"recycle", "auto", NULL,
		PANEL_NOTIFY_PROC,	carousel_options_proc,
		PANEL_VALUE,		2,
		NULL);
	working_rect = (Rect *)xv_get(toggle_item, XV_RECT);
	carousel_menu = (Menu)xv_create(XV_NULL, MENU_CHOICE_MENU,
		MENU_STRINGS, "top", "bottom", "delete", "add *", "sort", "clear", NULL,
		MENU_NOTIFY_PROC,	carousel_action,
		NULL);
#ifdef COPS_ON_BUTTON
	(void)xv_create(carousel_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"ops",
		PANEL_ITEM_MENU,	carousel_menu,
		XV_X,			6+rect_right(working_rect),
		XV_Y,			2+working_rect->r_top,
		NULL);
#else
	xv_set(carousel_list, PANEL_ITEM_MENU, carousel_menu, NULL);
#endif
	window_fit(carousel_panel);
	window_fit(carousel_frame);
}

#define METER_PANEL_HEIGHT 25
#define METER_WIN_HEIGHT 180
#define METER_WIDTH 180
#define MH_DIVISOR 7
#define MC_NUMERATOR 3
#define MH_NUMERATOR 4

/*#define CTRL_HEIGHT 75*/

#include <sspkg/grip.h>

static Drawline ctrl_line[NUM_CHANS];
static Drawline area_bot_line;
static int curr_ctrl_height[NUM_CHANS];
static int curr_meter_height = METER_WIN_HEIGHT - METER_PANEL_HEIGHT;
static int curr_meter_width = METER_WIDTH;
static Grip ctrl_grip[NUM_CHANS];
static unsigned char plunger_color[NUM_CHANS];

static void
update_ctrl(int chan, int value)
{
	int v = value, h;

	if (user_ctrl_height[setting_meter_color][chan]) {
		if (plunger_color[chan] == 16) return;
		xv_set(ctrl_line[chan], RECTOBJ_FG, 16, NULL);
		plunger_color[chan] = 16;
		v = user_ctrl_height[setting_meter_color][chan];
	}
	else if (plunger_color[chan] != 4) {
		xv_set(ctrl_line[chan], RECTOBJ_FG, 4, NULL);
		plunger_color[chan] = 4;
	}


	if (value < 0) v = curr_ctrl_height[chan];
	else if (curr_ctrl_height[chan] == v) return;

	h = (MH_NUMERATOR*curr_meter_height)/MH_DIVISOR +
		((128-v) * ((MC_NUMERATOR*curr_meter_height)/(MH_DIVISOR+2)) ) / 128;
	xv_set(ctrl_grip[chan], XV_Y, h - curr_meter_width / 32,
		NULL);
	xv_set(ctrl_line[chan], DRAWLINE_Y, 1, h,
		NULL);
	curr_ctrl_height[chan] = v;
}


static void
user_update_ctrl(int chan, int value)
{
	int v = value, h;

	if (value < 0) v = user_ctrl_height[setting_meter_color][chan];
	else if (user_ctrl_height[setting_meter_color][chan] == value) return;
	h = (MH_NUMERATOR*curr_meter_height)/MH_DIVISOR +
		((128-v) * ((MC_NUMERATOR*curr_meter_height)/(MH_DIVISOR+2)) ) / 128;
	xv_set(ctrl_grip[chan], XV_Y, h - curr_meter_width / 32,
		NULL);
	xv_set(ctrl_line[chan], DRAWLINE_Y, 1, h,
		NULL);
	user_ctrl_height[setting_meter_color][chan] = v;
	curr_ctrl_height[chan] = v;
	if (value == 0 && setting_meter_color == USER_PAN) v = -1;
	switch (setting_meter_color) {
		case USER_EXPRESSION: expression[chan] = v; break;
		case USER_MAIN_VOLUME: main_volume[chan] = v; break;
		case USER_CHORUS_DEPTH: chorus_depth[chan] = v; break;
		case USER_REVERBERATION: reverberation[chan] = v; break;
		case USER_PAN:
		if (!setting_pstereo) break;
		ext_pan[chan] = v; break;
	}
#ifdef USE_SHM
	switch (setting_meter_color) {
		case USER_EXPRESSION: shm_expression[chan] = v; break;
		case USER_MAIN_VOLUME: shm_main_volume[chan] = v; break;
		case USER_CHORUS_DEPTH: shm_chorus_depth[chan] = v; break;
		case USER_REVERBERATION: shm_reverberation[chan] = v; break;
		case USER_PAN:
		if (!setting_pstereo) break;
		shm_ext_pan[chan] = v; break;
	}
#endif
}

static void
reset_controllers()
{
	int chan, temp;
	temp = setting_meter_color;
	for (setting_meter_color = 0; setting_meter_color < USER_NUM_CTLS;
			setting_meter_color++)
		for (chan = 0; chan < NUM_CHANS; chan++) user_update_ctrl(chan, 0);
	setting_meter_color = temp;
}
static void
clr_reset_controllers(Panel_item it, Event *ev)
{
	reset_controllers();
}
static void
set_controllers_proc(Panel_item it, Event *ev)
{
	int chan, ctl;

	for (ctl = 0; ctl < USER_NUM_CTLS; ctl++)
	for (chan = 0; chan < NUM_CHANS; chan++)
	user_ctrl_height[setting_meter_color][chan] = curr_ctrl_height[chan];
}
static void
wte_controllers_proc(Panel_item it, Event *ev)
{
	FILE *f;
	int ctl, chan, val;
	static char *p_name[USER_NUM_CTLS] = { "Expression", "Main Volume",
		"Chorus Depth", "Reverb", "Pan" };
	static int p_num[USER_NUM_CTLS] = { 11, 7, 93, 91, 10 };
	if ( (f = fopen("xmptemp.ctl", "w")) == NULL) {
		message("can't open xmptemp.ctl");
		return;
	}
	fprintf(f,"Track start\n");
	fprintf(f,"     0 Meta Text, type=0x03 (Sequence/Track Name)  leng=21\n");
	fprintf(f,"     Text = \"xmp controller values\"\n");

	for (ctl = 0; ctl < USER_NUM_CTLS; ctl++)
	for (chan = 0; chan < NUM_CHANS; chan++)
	if ((val=user_ctrl_height[ctl][chan])) {
	fprintf(f,"     0 Parameter, chan=%d %s[%d] ", chan+1, p_name[ctl], p_num[ctl]);
		switch(ctl) {
		case USER_EXPRESSION:
			fprintf(f,"=%d\n", val);
			break;
		case USER_MAIN_VOLUME:
			fprintf(f,"msb=%d\n", val);
			break;
		case USER_CHORUS_DEPTH:
		case USER_REVERBERATION:
			fprintf(f,"on[=%d]\n", val);
			break;
		case USER_PAN:
			fprintf(f,"%d[=%d]\n", 64-val, val);
			break;
		}
	}
	fprintf(f, "     0 Meta event, end of track\n");
	fprintf(f, "Track end\n");
	fclose(f);
}

static int
ctrl_move_proc(
	Xv_window       paint_window,
	Event          *event,
	Canvas_shell    canvas_shell,
	Grip            grip,
	short          *new_x,
	short          *new_y)
{
	int chan, h, value;

	for (chan = 0; chan < NUM_CHANS; chan++)
		if (grip == ctrl_grip[chan]) break;

	h = *new_y + curr_meter_width / 32 -
		(MH_NUMERATOR*curr_meter_height) / MH_DIVISOR;
	value = 128 - (h*128*(MH_DIVISOR+2)) / (MC_NUMERATOR*curr_meter_height);

	if (chan >= NUM_CHANS || value < 0 || value > 127) return(FALSE);

	if (value < 4 && user_ctrl_height[setting_meter_color][chan] > value) value = 0;
	if (value > 123 && user_ctrl_height[setting_meter_color][chan] < value) value = 127;

	if (!value && plunger_color[chan] != 4) {
		xv_set(ctrl_line[chan], RECTOBJ_FG, 4, NULL);
		plunger_color[chan] = 4;
	}
	if (value && plunger_color[chan] != 16) {
		xv_set(ctrl_line[chan], RECTOBJ_FG, 16, NULL);
		plunger_color[chan] = 16;
	}

	user_update_ctrl(chan, value);
	return(TRUE);
}


static void
size_ctrls(int width, int height)
{
	int chan, area_bot;
	int x1 = ((80+500/2) * width) / 10000;

	for (chan = 0; chan < NUM_CHANS; chan++) {
	    xv_set(ctrl_grip[chan], 
		XV_WIDTH,	width / 16,
		XV_HEIGHT,	width / 16,
		XV_X, x1 + (chan * width) / 16 - width / 32,
		NULL);
	    xv_set(ctrl_line[chan], 
		DRAWLINE_X, 0, x1 + (chan * width) / 16,
		DRAWLINE_Y, 0, height - 5,
		DRAWLINE_X, 1, x1 + (chan * width) / 16,
		NULL);
	    user_update_ctrl(chan, -1);
	}
	area_bot = (MH_NUMERATOR*height)/MH_DIVISOR - width/28;
	xv_set(area_bot_line,
		DRAWLINE_Y, 0, area_bot,
		DRAWLINE_X, 1, width,
		DRAWLINE_Y, 1, area_bot,
		NULL);
}

static void
draw_ctrls()
{
	int chan;

	for (chan = 0; chan < NUM_CHANS; chan++) {
		ctrl_grip[chan] = xv_create(meter_shell, GRIP,
			RECTOBJ_FG,	0,
			GRIP_SLIDE_X, FALSE,
			GRIP_MOVE_PROC, ctrl_move_proc,
			NULL);
		ctrl_line[chan] = (Drawline)xv_create(meter_shell, DRAWLINE,
			RECTOBJ_FG,	4,
			DRAWLINE_ARROW_STYLE, 1, ARROW_SIMPLE, 
			DRAWLINE_ARROW_ANGLE, 1, 180 * 64, 
			DRAWLINE_ARROW_LENGTH, 1, 4, 
			NULL);
		plunger_color[chan] = 4;
	}

	area_bot_line = (Drawline)xv_create(meter_shell, DRAWLINE,
		RECTOBJ_FG,	16,
		DRAWLINE_X, 0, 0,
		NULL);
}


static void
meter_resize(Canvas_shell shell, int width, int height)
{

	curr_meter_height = height - METER_PANEL_HEIGHT;
	curr_meter_width = width;

	xv_set(meter_area,
		XV_WIDTH, width,
		XV_HEIGHT,
			(MH_NUMERATOR*curr_meter_height)/MH_DIVISOR - width/32,
		NULL);

	size_ctrls(curr_meter_width, curr_meter_height);
	xv_set(meter_panel,
		XV_Y, curr_meter_height,
		XV_WIDTH, width + 5,
		XV_HEIGHT, METER_PANEL_HEIGHT + 5,
		NULL);
	xv_set(meter_wte_button,
		XV_X, curr_meter_width - xv_get(meter_wte_button, XV_WIDTH),
		NULL);
	xv_set(meter_clr_button,
		XV_X, curr_meter_width - xv_get(meter_clr_button, XV_WIDTH) -
			xv_get(meter_wte_button, XV_WIDTH),
		NULL);
	xv_set(meter_set_button,
		XV_X, curr_meter_width - xv_get(meter_set_button, XV_WIDTH) -
			xv_get(meter_clr_button, XV_WIDTH) -
			xv_get(meter_wte_button, XV_WIDTH),
		NULL);
}

static unsigned time_expired = 0;
struct timeval tv;
struct timezone tz;
static void
time_sync()
{
	unsigned jiffies;

	gettimeofday (&tv, &tz);
	jiffies = tv.tv_sec*100 + tv.tv_usec/10000;
	if (xmp_epoch < 0) {
		xmp_epoch = 0;
		xxmp_epoch = jiffies;
	}
	time_expired = jiffies - xxmp_epoch;
}

#define RINGSIZE 64
int *talk_in, *talk_out;

struct mp_talk {
	unsigned mptime;
	unsigned char count[NUM_CHANS];
	unsigned short vel[NUM_CHANS];
	int expr[NUM_CHANS];
};

struct mp_talk **mpd;


#ifdef USE_SHM
#define TOTAL_SHM 8192

static void
setup_shm()
{
	char *shmaddr;
	key_t key;
	int shmid, i;

	key = ftok ("xmp", 1);

	/* need 2*4 +7*4 + 65*4 + 65*116 = 8 + 28 + 260 + 7520 = 7840 */
	/* ... and add 5 * 16chan * 4 == 320, 320+7840 = 8160 */
	/* + 2*4 (2 more vib. settings) = 8168 */
	/* + 4 (running_time) = 8172 */
	if ((shmid = shmget (key, TOTAL_SHM, IPC_EXCL | 01600 )) < 0) {
		perror ("shmget ");
		exit (1);
	}
	if ((shmaddr = shmat (shmid, (char *)0, SHM_RND)) == (char *) -1) {
		perror ("shmat ");
		if (shmctl (shmid, IPC_RMID, NULL))
			perror ("shmctl ");
		exit (1);
	}

	if (shmctl (shmid, IPC_RMID, NULL)) {
		perror ("shmctl ... ");
		exit (1);
	}

	talk_in = (int *)shmaddr;
	shmaddr += sizeof(int);
	talk_out = (int *)shmaddr;
	shmaddr += sizeof(int);

	shm_setting_pstereo = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_gus_tuning = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_gus_volume = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_meter_color = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_reverb = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_chorus_spread = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_vibrato_depth = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_vibrato_speed = (int *)shmaddr;
	shmaddr += sizeof(int);
	shm_setting_vibrato_sweep = (int *)shmaddr;
	shmaddr += sizeof(int);

	*shm_setting_pstereo = setting_pstereo;
	*shm_setting_gus_tuning = setting_gus_tuning;
	*shm_setting_gus_volume = setting_gus_volume;
	*shm_setting_meter_color = setting_meter_color;
	*shm_setting_reverb = setting_reverb;
	*shm_setting_chorus_spread = setting_chorus_spread;
	*shm_setting_vibrato_depth = setting_vibrato_depth;
	*shm_setting_vibrato_speed = setting_vibrato_speed;
	*shm_setting_vibrato_sweep = setting_vibrato_sweep;

	shm_running_time = (long *)shmaddr;
	shmaddr += sizeof(unsigned long);

	shm_expression = (int *)shmaddr;
	shmaddr += sizeof(expression);
	shm_main_volume = (int *)shmaddr;
	shmaddr += sizeof(main_volume);
	shm_chorus_depth = (int *)shmaddr;
	shmaddr += sizeof(chorus_depth);
	shm_reverberation = (int *)shmaddr;
	shmaddr += sizeof(reverberation);
	shm_ext_pan = (int *)shmaddr;
	shmaddr += sizeof(ext_pan);

	mpd = (struct mp_talk **)shmaddr;
	shmaddr += (RINGSIZE+1)*sizeof(struct mp_talk *);
	for (i = 0; i < RINGSIZE+1; i++)
		mpd[i] = (struct mp_talk *)(shmaddr + i*sizeof(struct mp_talk));
	if (shmaddr - (char *)talk_in + (RINGSIZE+1)*sizeof(struct mp_talk) > TOTAL_SHM) {
		fprintf(stderr, "Uh, oh.  Allocate more shared memory!\n");
		fprintf(stderr, "misc is %d\n", shmaddr - (char *)talk_in);
		fprintf(stderr, "talk struct is %d * %d = %d\n",
			RINGSIZE+1, sizeof(struct mp_talk),
			(RINGSIZE+1) * sizeof(struct mp_talk) );
	}

}
#endif

static int last_meter_reading[NUM_CHANS];
static int disp_meter_reading[NUM_CHANS];

#ifdef USE_SHM
#define FUTURE_METER 5
#else
#define FUTURE_METER 62
#endif

static void
update_meter()
{
	int chan, vol, r;
	int x1, y1, h, w, cnt = 0;
	int fill_style = 2, exp_color;
	int expired;
	static void read_one_packet();

	read_one_packet();

	if (xmp_epoch < 0) r = RINGSIZE;
	else {
		time_sync();
		expired = FUTURE_METER + time_expired - setting_meter_sync;
		if (expired < 0) return;
		while (*talk_out < *talk_in) {
			if (mpd[*talk_out % RINGSIZE]->mptime > expired + 5) break;
			(*talk_out)++;
		}
		(*talk_out)--;
		if (*talk_out < 0) {
			*talk_out = 0;
			return;
		}
		r = *talk_out % RINGSIZE;
/** (debug)
if (mpd[r]->mptime > expired)
printf("mptime %d > expired %d at *talk_out %d, *talk_in %d\n", mpd[r]->mptime, expired, 
*talk_out, *talk_in);
**/
		if (mpd[r]->mptime > expired + 5) return;
	}

	for (chan = 0; chan < NUM_CHANS; chan++) {
		update_ctrl(chan, mpd[r]->expr[chan]);
		if (!mpd[r]->count[chan]) vol = 0;
		else vol = mpd[r]->vel[chan] / mpd[r]->count[chan];
		if (vol < disp_meter_reading[chan])
			vol = (vol + disp_meter_reading[chan]) / 2;
		disp_meter_reading[chan] = vol;
		if (vol == last_meter_reading[chan]) continue;
		last_meter_reading[chan] = vol;
		cnt++;
	}
	if (!cnt) return;

	xv_set(meter_shell, CANVAS_SHELL_DELAY_REPAINT, TRUE, NULL);
	VClear(meter_area);
	VSetFillStyle(meter_area, (fill_style = 0));
	for (chan = 0; chan < NUM_CHANS; chan++) {
		vol = disp_meter_reading[chan];
		if (!vol) continue;
		if (PERCCHAN(chan) && !setting_meter_column) {
			if (fill_style != 2) VSetFillStyle(meter_area, (fill_style = 2));
		}
		else {
			if (fill_style != 0) VSetFillStyle(meter_area, (fill_style = 0));
		}
		exp_color = (mpd[r]->expr[chan] >> 3) + 1;
		if (setting_meter_color == 3) exp_color = 17 - exp_color;
		VSetColor(meter_area, exp_color);
		w = 80 + mpd[r]->count[chan] * 30 + 20 /*50*/;
		if (w < 130) w = 130;
		else if (w > 500) w = 500;
		x1 = 80 + (501 - w)/2 + (chan * 10000 + NUM_CHANS / 2)/NUM_CHANS;
		y1 = 10000 - ((vol+1) * 10000)/128;
		h = 10000 - y1;
		VFillRectangle(meter_area, x1, y1, w, h);
	}
	xv_set(meter_shell, CANVAS_SHELL_DELAY_REPAINT, FALSE, NULL);
}

#define TUNE_UP_COLORS

#ifdef TUNE_UP_COLORS
static void
show_meter_bars()
{
	int chan, w, x1, y1, h;

	for (chan = 0; chan < NUM_CHANS; chan++) {
		VSetColor(meter_area, chan+1);
		w = 500;
		x1 = 80 + (500 - w)/2 + (chan * 10000)/NUM_CHANS;
		y1 = 0;
		h = 10000 - y1;
		VFillRectangle(meter_area, x1, y1, w, h);
	}
}
#endif

static void
setup_meter()
{
	Server_image stipple_image;

#ifdef USE_SHM
	setup_shm();
#else
	int i;
	talk_in = (int *)malloc(sizeof(int));
	talk_out = (int *)malloc(sizeof(int));
	mpd = (struct mp_talk **)malloc( (RINGSIZE+1)*sizeof(struct mp_talk *) );
	for (i = 0; i < RINGSIZE+1; i++)
		mpd[i] = (struct mp_talk *)malloc( sizeof(struct mp_talk) );
#endif

	stipple_image = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, stipple_width,
		XV_HEIGHT, stipple_height,
		SERVER_IMAGE_X_BITS, stipple_bits,
		NULL);

	meter_cms = (Cms) xv_create(XV_NULL, CMS,
		CMS_SIZE, 17,
		CMS_NAMED_COLORS, "black",
			"cyan2",
			"turquoise2",
			"aquamarine2",
			"spring green",
			"green",
			"chartreuse",
			"green yellow",
			"DarkOliveGreen1",
			"yellow",
			"LightGoldenrod1",
			"gold",
			"DarkGoldenrod1",
			"orange",
			"dark orange",
			"DarkOrange1",
			"red",
			NULL,
		NULL);
	meter_area = (Drawarea)xv_create(XV_NULL, DRAWAREA, NULL);

	/*curr_meter_height = METER_HEIGHT + CTRL_HEIGHT - METER_WIDTH/32;*/
	curr_meter_height = METER_WIN_HEIGHT - METER_PANEL_HEIGHT;
	curr_meter_width = METER_WIDTH;

	meter_frame = (Frame)xv_create(frame, FRAME,
		XV_X,		420-150-30,
		XV_Y,		50+80,
		XV_WIDTH,	curr_meter_width,
		XV_HEIGHT,	METER_WIN_HEIGHT,
		FRAME_LABEL,	"meter",
		NULL);
	meter_shell = (Canvas_shell)xv_create(meter_frame, CANVAS_SHELL,
		WIN_CMS, meter_cms,
		CANVAS_RESIZE_PROC, meter_resize,
		NULL);
	xv_set(meter_shell, CANVAS_SHELL_BATCH_REPAINT, TRUE, NULL);
	xv_set(meter_area,
		XV_OWNER, meter_shell,
		XV_WIDTH, xv_get(meter_shell, XV_WIDTH),
		/*XV_HEIGHT, xv_get(meter_shell, XV_HEIGHT) - CTRL_HEIGHT,*/
		NULL);
        VSetStipple(meter_area, stipple_image);
#ifdef TUNE_UP_COLORS
	show_meter_bars();
#endif
	draw_ctrls();
	meter_panel = (Panel)xv_create(meter_frame, PANEL,
		XV_FONT, font_small,
		NULL);
	meter_ctlr_choice = xv_create(meter_panel, PANEL_CHOICE,
		PANEL_ITEM_X_GAP,	0,
		PANEL_CHOICE_STRINGS,	"e", "v", "c", "r", "p", NULL,
		PANEL_NOTIFY_PROC,	meter_color_proc,
		PANEL_VALUE,		0,
		NULL);
	working_rect = (Rect *)xv_get(meter_ctlr_choice, XV_RECT);
	meter_set_button = xv_create(meter_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"S",
		XV_Y,			2+working_rect->r_top,
		PANEL_NOTIFY_PROC,	set_controllers_proc,
		NULL);
	meter_clr_button = xv_create(meter_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"R",
		XV_Y,			2+working_rect->r_top,
		PANEL_NOTIFY_PROC,	clr_reset_controllers,
		NULL);
	meter_wte_button = xv_create(meter_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"W",
		XV_Y,			2+working_rect->r_top,
		PANEL_NOTIFY_PROC,	wte_controllers_proc,
		NULL);

	meter_resize(meter_shell, curr_meter_width, curr_meter_height + METER_PANEL_HEIGHT);
/**
printf("clr at (%d, %d); choice at (%d, %d)\n",
xv_get(meter_clr_button, XV_X),
xv_get(meter_clr_button, XV_Y),
xv_get(meter_ctlr_choice, XV_X),
xv_get(meter_ctlr_choice, XV_Y));
**/
}

#ifndef USE_SHM
static int pipe_fd;
#endif

static void read_one_packet()
{
#ifdef USE_SHM
	if (!midi_file_playing)
		*talk_in = *talk_out = 0;
	if (*talk_in < 1) return;
	if (xmp_epoch < 0) {
		time_sync();
		xmp_epoch += mpd[0]->mptime;
		time_expired += mpd[0]->mptime;
	}
	if ((xmp_time % 6) < 3 && midi_file_playing == TRUE) {
		xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_RED, NULL);
	}
	midi_file_playing = 2;
#else
	int i, r, n;

	if (total_packets < sizeof(struct mp_talk)) return;

	if (xmp_epoch < 0) {
		*talk_in = *talk_out = 0;
	}

	r = *talk_in % RINGSIZE;

	if ((i = read(pipe_fd, mpd[r]->count, sizeof(mpd[r]->count))) > 0) {
		total_packets -= i;
	}
	if ((i = read(pipe_fd, mpd[r]->vel, sizeof(mpd[r]->vel))) > 0) {
		total_packets -= i;
	}
	if ((i = read(pipe_fd, &mpd[r]->mptime, sizeof(mpd[r]->mptime))) > 0) {
		total_packets -= i;
	}
	if ((i = read(pipe_fd, mpd[r]->expr, sizeof(mpd[r]->expr))) > 0) {
		total_packets -= i;
	}
	for (n = 0; n < NUM_CHANS; n++) if (mpd[r]->expr[n] < 0) {
		mpd[r]->expr[n] = (setting_meter_color == 3)? 0 : 64;
	}
	if (*talk_in - *talk_out < RINGSIZE - 2) (*talk_in)++;
	if (xmp_epoch < 0) {
		time_sync();
		xmp_epoch += mpd[r]->mptime;
		time_expired += mpd[r]->mptime;
	}
#endif
}

#ifndef USE_SHM
static Notify_value read_mplayer(Notify_client me, int fd)
{
	int bytes;

	pipe_fd = fd;

	if (ioctl(fd, FIONREAD, &bytes) == 0) {
		total_packets = bytes;
	}
	read_one_packet();
	if ((xmp_time % 6) < 3 && midi_file_playing == TRUE) {
		xv_set(dot_play_button, PANEL_ITEM_COLOR, MAIN_COLOR_RED, NULL);
	}
	midi_file_playing = 2;
	return(NOTIFY_DONE);
}
#endif

static void
file_action_proc(Menu menu, Menu_item mi)
{
	char *fname, *path;

	fname = (char *)xv_get(mi, MENU_STRING);
	path = (char *)xv_get(mi, MENU_CLIENT_DATA);

	if (carousel_visible) {
		int last = xv_get(carousel_list, PANEL_LIST_NROWS);
		xv_set(carousel_list,
			PANEL_LIST_INSERT, last,
			PANEL_LIST_STRING, last, fname,
			PANEL_LIST_CLIENT_DATA, last, strcpy(malloc(strlen(path)+1), path),
			NULL);
		if (!midi_file_playing && carousel_auto) midi_file_selected = FALSE;
		return;
	}
	if (midi_file_playing) return;

	(void)crsltoplay(fname, path);
}

static char *
getfilename(char *path)
{
	char *p;
#ifndef TRY_REAL_FILENAME
	char *q;
	int i, len, spccount = 0;
#endif
	if ((p = rindex(path, '/'))) p++;
	else p = path;
#ifdef TRY_REAL_FILENAME
	return(strcpy(malloc(strlen(p)+1), p));
#else
	len = strlen(p);
	for (i = 1; i < len; i++)
		if (isupper(p[i])) spccount++;
	len += spccount;
	q = strcpy(malloc(len+1), p);
	while (spccount && len > 1) {
		q[len] = q[len-spccount];
		if (isupper(q[len])) {
			q[len-spccount] = ' ';
			spccount--;
		}
		len--;
	}
	return(q);
#endif
}

Menu
gen_pullright(Menu_item mi, Menu_generate op)
{
	Menu menu;
	Menu_item new;
	char buf[MAXPATHLEN];
	static Menu_item add_path_to_menu();

	if (op == MENU_DISPLAY) {
		menu = (Menu)xv_get(mi, XV_OWNER);
		sprintf(buf, "%s/%s",
			(char *)xv_get(menu, MENU_CLIENT_DATA),
			(char *)xv_get(mi, MENU_CLIENT_DATA));
		if ((menu = (Menu)xv_get(mi, MENU_PULLRIGHT))) {
			free((char *)xv_get(menu, MENU_CLIENT_DATA));
			xv_destroy(menu);
		}
#ifdef CHECK_EACH_MID
		if ((new = add_path_to_menu(buf)) != XV_NULL
			&& new != (Menu_item)-1 && new != (Menu_item)-2) {
#else
		if ((new = add_path_to_menu(buf)) != XV_NULL) {
#endif
			menu = (Menu)xv_get(new, MENU_PULLRIGHT);
			free((char *)xv_get(new, MENU_CLIENT_DATA));
			xv_destroy(new);
			return(menu);
		}
	}
	if (!(menu = (Menu)xv_get(mi, MENU_PULLRIGHT)))
		menu = (Menu)xv_create(
			XV_NULL,
			MENU,
			MENU_COLOR,	MAIN_COLOR_RED,
			MENU_STRINGS, "Couldn't build a menu.", NULL,
			NULL);
	return(menu);
}

static
Menu_item
add_path_to_menu(char *path)
{
	DIR *dirp;
	struct direct *dp;
	struct stat s_buf;
	Menu_item mi;
	Menu next_menu;
	char buf[MAXPATHLEN];
	char *pathname;
	static int recursion = 0;

	if (stat(path, &s_buf) == -1 || !(s_buf.st_mode & S_IREAD)) return(XV_NULL);

	if (s_buf.st_mode & S_IFDIR) {
		int cnt = 0;
		if (!((dirp = opendir(path)))) return(XV_NULL);
		if (recursion) {
			closedir(dirp);
			return((Menu_item)-1);
		}
		recursion++;
		next_menu = (Menu)xv_create(XV_NULL, MENU, NULL);
		while ((dp = readdir(dirp)))
			if (strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
				(void)sprintf(buf, "%s/%s", path, dp->d_name);
				mi = add_path_to_menu(buf);
#ifdef CHECK_EACH_MID
				if (mi == (Menu_item)-2) continue;
#endif
				if (mi == XV_NULL || mi == (Menu_item)-1) {
					int do_gen_pullright =
						(mi == (Menu_item)-1);
					pathname = strcpy(malloc(
						strlen(dp->d_name)+1), dp->d_name);
					mi = (Menu_item)xv_create(next_menu,
						MENUITEM,
						MENU_STRING, getfilename(dp->d_name),
						MENU_CLIENT_DATA, pathname,
						MENU_RELEASE,
						MENU_RELEASE_IMAGE,
						NULL);
					if (do_gen_pullright) xv_set(mi,
						MENU_GEN_PULLRIGHT, gen_pullright,
						NULL);
					else xv_set(mi, MENU_INACTIVE, TRUE, NULL);
				}
				xv_set(next_menu, MENU_APPEND_ITEM, mi, NULL);
				cnt++;
			}
		closedir(dirp);
		pathname = strcpy(malloc(strlen(path)+1), path);
		mi = (Menu_item)xv_create(
			XV_NULL,
			MENUITEM,
			MENU_STRING, getfilename(path),
			MENU_CLIENT_DATA, pathname,
			MENU_RELEASE,
			MENU_RELEASE_IMAGE,
			MENU_NOTIFY_PROC, file_action_proc,
			NULL);
		if (!cnt) {
			xv_destroy(next_menu);
			xv_set(mi, MENU_INACTIVE, TRUE, NULL);
		}
		else {
			xv_set(next_menu,
				MENU_TITLE_ITEM, strcpy(malloc(strlen(path)+1), path),
				MENU_CLIENT_DATA, strcpy(malloc(strlen(path)+1), path),
				NULL);
			xv_set(mi, MENU_PULLRIGHT, next_menu, NULL);
		}
		recursion--;
		return(mi);
	}
#ifdef CHECK_EACH_MID
	else if (it_is_not_midi(path)) return(-2);
#endif
	pathname = strcpy(malloc(strlen(path)+1), path);
	return((Menu_item)xv_create(
		XV_NULL,
		MENUITEM,
		MENU_STRING, getfilename(path),
		MENU_CLIENT_DATA, pathname,
		MENU_RELEASE,
		MENU_RELEASE_IMAGE,
		MENU_NOTIFY_PROC, file_action_proc,
		NULL)
	);
}



/****************************************************************************
* Variables set by command line switches
****************************************************************************/

int ad_print = false;		/* adagio output */
int vverbose = false;		/* tracing output */


/****************************************************************************
*	Routines in phasem.c
****************************************************************************/
void var_init();
void initfuncs();
void rec_init();
void card_init(char *devname);
event_type rec_final();
/*************************/

#define READ_WHOLE

/* part of interface to midifile functions */
#ifdef READ_WHOLE
static unsigned char *midi_file_contents;
static int midi_file_size;

static int filegetc()
{
    if (midi_file_size-- > 0) return (*midi_file_contents++);
    return(EOF);
}
#else
static FILE *F;
static int filegetc()
{
    return (getc(F));
}
#endif

void txt_error(char *s)
{
    fprintf(stderr, "midifile error: %s\n", s);
    _exit(0);
}

#ifndef USE_SHM
long *shm_running_time;
static long keep_running_time;
#endif

static
int do_midi(char *midi_name)
{
#ifdef READ_WHOLE
    struct stat statbuf;
    int fd;

    if (stat(midi_name, &statbuf)) {
	perror(midi_name);
	_exit(0);
    }
    midi_file_size = statbuf.st_size;
    if ( (fd = open(midi_name, O_RDONLY, 0)) == -1 ) {
	perror(midi_name);
	_exit(0);
    }
    if ( (midi_file_contents = (char *)malloc(midi_file_size)) == NULL) {
	perror("malloc");
	_exit(0);
    }
    if (read(fd, midi_file_contents, midi_file_size) != midi_file_size) {
	perror("read");
	_exit(0);
    }
    close(fd);
#ifdef READ_MODS
    Mf_file_contents = midi_file_contents;
    Mf_file_size = midi_file_size;
#endif
#endif
#ifndef USE_SHM
    shm_running_time = &keep_running_time;
#endif
    running_time = 0;
    var_init();
    no_solo = req_no_solo;
    piano_only = req_piano_only;
#ifndef READ_WHOLE
    F = fopen(midi_name, "r");
#endif
    card_init("/dev/sequencer");/* open /dev/sequencer and get info on devices */
    rec_init();
    initfuncs();		/* set calls to us from midifile functions */
    Mf_getc = filegetc;		/* tell midifile how to get bytes to process */
    mfread();			/* call midifile */
#ifdef READ_WHOLE
    free(midi_file_contents);
#else
    fclose(F);
#endif
    phase2((rec_final(true)));
    return(0);
}
