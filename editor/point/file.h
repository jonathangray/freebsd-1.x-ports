/* $Header: /a/cvs/386BSD/ports/editor/point/file.h,v 1.1 1994/02/15 22:12:42 jkh Exp $ */

/* a PIECE is a sequence of characters that is contiguous in the logical */
/* and physical files.  Originally the file is one (big) piece.  As edits */
/* are made it is divided into more pieces.  The characters in a piece */
/* are either in the original (unchanged) file or in the add file (which) */
/* contains all characters that have been added to the file in this editing */
/* session */

struct piece {
	char flags;		/* record various Boolean data */
	int file;		/* Unix file handle */
	BlockID blockID;	/* block piece comes from or NullObject */
				/* if it comes from a format string */
	Offset position;	/* start of the piece */
	Offset length;		/* length of the piece */
	struct piece *nextClone;/* next piece in the clone list */
	struct piece *nextPiece;/* next piece in the list */
	struct piece *prevPiece;/* previous piece in the list */
};

/* piece.flags constants */
#define IS_CLONED_PIECE		1
#define IS_DECORATION		2

typedef struct piece *Piece;

/* An open file consists of the original file (which is not changed during */
/* an editing session) and an add file (where all new characters go). */
/* The logical file is described in the pieces table which shows where */
/* the logically contiguous characters in the file are physically located */

struct openFile {

	/* flag to keep track of whether this is a view or not */
	int isView;
	
	/* fields to support xterm emulation */
	int screen_rows, screen_cols;
	char * screen_image;	/* screen_rows by screen_cols */

	/* the original file size */
	Offset origFileSize;

	/* the current logical file size -- changed as the file is edited */
	/* NOT the same as the size of origfile or addfile or their sum */
	Offset fileSize;

	/* the file being edited -- this is read only */
	char origName[FILENAMESIZE];
	int origHandle;
	
	/* change information to detect files changed outside of Point */
	time_t lastModified;
	time_t lastChanged;

	/* the piece list */
	Piece pieceList;
	
	/* the command history */
	struct changeItem * cmdHistory;

	/* optimization fields */
	/* loLogPiece and hiLogPiece are the low and high logical addresses */
	/* that are mapped by piece number logPiece which is the last piece */
	/* where a byte was found and is initialized to the one big piece */
	/* that is the whole file when you start editing. */
	/* The idea is that once you map a logical byte to a piece, it is */
	/* likely that the next mapping is in the same piece. */
	/* loLogBuffer and hiLogBuffer are the logical character limits of */
	/* some valid characters in a buffer pointed to by logBuffer. */
	Offset loLogPiece, hiLogPiece, loLogBuffer, hiLogBuffer;
	Piece logPiece;
	unsigned char *logBuf;

	/* some file status flags */
	char useCount;	/* the number of windows using the file */
	char flags;	/* various information (see below) */
};

/* flags */
#define BAK_MADE	1	/* a backup copy has been made */
#define READ_ONLY	2	/* file cannot be written (changeable) */
#define IS_CHANGED	4	/* file has been changed but not yet saved */
#define NO_WRITES	8	/* file can never be written (no changeable) */

/* a disk buffer (both in and out of the address space) */
struct diskBuffer {
	int handle;	/* DOS file handle */
	/* double linked hash chain links */
	struct diskBuffer *forwardHash, *backwardHash;
	int blockNumber;
	unsigned char *bufferAddress;
	char written;	/* =1 if an add file block */
};
