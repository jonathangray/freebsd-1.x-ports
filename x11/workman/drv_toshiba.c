/*
 * @(#)drv_toshiba.c	1.4	1/24/94
 *
 * Vendor-specific drive control routines for Toshiba XM-3401 series.
 */
static char *ident = "@(#)drv_toshiba.c	1.4 1/24/94";

#include <stdio.h>
#include <errno.h>
#include "struct.h"

#define	SCMD_TOSH_EJECT		0xc4

static int	tosh_init(), tosh_eject();
int		wm_scsi2_get_volume(), wm_scsi2_set_volume();

struct wm_drive toshiba_proto = {
	-1,			/* fd */
	"Toshiba",		/* vendor */
	"\0                ",	/* model */
	NULL,			/* aux */
	NULL,			/* daux */

	tosh_init,		/* functions... */
	gen_get_trackcount,
	gen_get_cdlen,
	gen_get_trackinfo,
	gen_get_drive_status,
	wm_scsi2_get_volume,
	wm_scsi2_set_volume,
	gen_pause,
	gen_resume,
	gen_stop,
	gen_play,
	tosh_eject,
};

/*
 * Initialize the driver.
 */
static int
tosh_init(d)
	struct wm_drive	*d;
{
	extern int	min_volume;

	min_volume = 0;
}

/*
 * Send the Toshiba code to eject the CD.
 */
static int
tosh_eject(d)
	struct wm_drive *d;
{
	return (sendscsi(d, NULL, 0, 0, SCMD_TOSH_EJECT, 1, 0,0,0,0,0,0,0,0));
}
