/* include file defining constants/macros for PM files.  Used by xvpm.c */

#define	PM_MAGICNO	0x56494557		/* Hex for VIEW */

#define	PM_A		0x8000
#define	PM_C		0x8001
#define	PM_S		0x8002
#define	PM_I		0x8004
#define PM_F		0xc004

#define PM_IOHDR_SIZE	(sizeof(pmpic)-(2*sizeof(char*)))

typedef struct {
	int	pm_id;		/* Magic number for pm format files.	*/
	int	pm_np;		/* Number of planes. Normally 1.	*/
	int	pm_nrow;	/* Number of rows. 1 - MAXNELM.		*/
	int	pm_ncol;	/* Number of columns. 1 - MAXNELM.	*/
	int	pm_nband;	/* Number of bands.			*/
	int	pm_form;	/* Pixel format.			*/
	int	pm_cmtsize;	/* Number comment bytes. Includes NULL. */
	char	*pm_image;	/* The image itself.			*/
	char	*pm_cmt;	/* Transforms performed.		*/
} pmpic;


#define pm_nelm(p)	((p)->pm_ncol * (p)->pm_nrow)
#define pm_nbelm(p)	(pm_nelm(p) * (p)->pm_nband)
#define pm_psize(p)	(pm_nbelm(p) * (((p)->pm_form)&0xff))
#define pm_isize(p)	((p)->pm_np * pm_psize(p))
#define pm_npix(p)      (pm_nbelm(p) * (p)->pm_np)

