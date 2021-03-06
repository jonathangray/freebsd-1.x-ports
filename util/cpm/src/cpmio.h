/*	cpmio.h	1.4	83/05/13	*/
#define INTSIZE 32  /* number of bits per integer on this particular machine */
extern	int fid;
extern  struct directory {
	char status;		/* status of this entry; equals 0xe5 if */
				/* free to use, otherwise contains the */
				/* user number (owner) (0 - 15)	*/
	char name[8];		/* File name, padded with blanks */
	char ext[3];		/* file name extension, padded with blanks */
	char extno;		/* extent number */
	char notused[2];	/* unused */
	char blkcnt;		/* record count, number of 128 byte records */
				/* in this extent */
	char pointers[16];	/* pointers to the individual blocks */
	} *dirbuf;

#define CPMSECSIZ 128		/* number of bytes per sector in CP/M terms */

#define blockno(i) (use16bitptrs?					\
		    (0xff & (int)fptr->c_dirp->pointers[2*(i)]) +	\
		    ((0xff & (int)fptr->c_dirp->pointers[2*(i)+1]) << 8): \
		    0xff & (int)fptr->c_dirp->pointers[i])

extern int	dflag, cflag, iflag, tflag;
extern int	blksiz;	
extern int	tracks;
extern int	maxdir;
			
extern int	seclth;
extern int	sectrk;
extern int	skew;	
extern int	restrk;		/* reserved tracks (for system) */

extern int	*bitmap, *skewtab;
extern int 	bm_size;
extern int	use16bitptrs;
