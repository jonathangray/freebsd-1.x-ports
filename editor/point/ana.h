/* $Header: /a/cvs/386BSD/ports/editor/point/ana.h,v 1.1 1994/02/15 22:12:41 jkh Exp $ */

#ifdef HYPERTEXT
#include <ndbm.h>
#endif

/************************************************************************/
/*  Format Characters							*/
/*  -- these are used in block and link map format strings		*/
/************************************************************************/
#define FormatMarker		'$'
#define PutBlockContents	'b'
#define PutBlockID		'i'
#define PutBlockName		'n'
#define PutAttributeName	'a'

/************************************************************************/
/*  Constants								*/
/*  -- many of these will change as the system develops			*/
/************************************************************************/
#define NAME_SIZE	80
#define MAX_LINKS	5
#define MAX_ATTRIBUTES	5
#define MAP_SIZE	7

#define NullObject 0

typedef enum {
	OBJECT_MARKER = (0x34343434)
} ObjectMarker;

/************************************************************************/
/*  A DOCUMENT is a collection of text pieces, their hypertext links,	*/
/*  -- and views (ways of looking at the text pieces).			*/
/*  A VIEW is a display of some part of the document.			*/
/*  -- A view starts with a block and includes mappings to determine	*/
/*       how to handle the blocks and links it encounters.		*/
/*  A VIEW STREAM is the stream of characters that constitute the view.	*/
/*  -- A view stream is generated starting with the initial bloc of the	*/
/*       view and expanding blocks and links as sources.		*/
/*  A LOCATION is a source and an offset into that source.		*/
/*  -- A location marks a place in a view stream			*/
/************************************************************************/

/************************************************************************/
/*  Allocation codes							*/
/************************************************************************/
typedef enum {
	NO_ALLOCATE,
	ALLOCATE
} AllocationMode;

typedef enum {
	NO_RELEASE,
	RELEASE
} ReleaseMode;

/************************************************************************/
/*  Typedefs for the ID types (all int in the end)			*/
/************************************************************************/
typedef int Offset;
typedef unsigned int ID;
typedef unsigned int BlockID;
typedef unsigned int AttributeID;
typedef unsigned int MapID;
typedef unsigned int FileID;
typedef unsigned int LinkID;
typedef unsigned int TextID;
typedef unsigned int ViewID;
typedef unsigned int DocumentID;

/* magic numbers for error checking */
typedef enum {
	AttributeMagic = 1,
	BlockMagic = 2,
	DocumentMagic = 3,
	FileMagic = 4,
	LinkMagic = 5,
	MapMagic = 6,
	TextMagic = 7,
	ViewMagic = 8
} MagicNumber;

/*
 * All objects have a `magic' number as their first field.  This is strictly
 *    for error checking and will be removed later.
 * All objects contain their ID number in the `this_one' field.
 *    This is for error checking.
 * All items have a `next' field which is used to link together all items
 *    of a particular type.  I know we need this for BlockTypes and LinkTypes
 *    and we will probably want it for all types.
 * Most items have a `name' field.  For now it is a fixed length string.
 *    Later we will make it s TextID and allows strings of any length.
 *    Later will will also add a longName.
 */

typedef struct {
	ObjectMarker marker;	/* to verify that this is an object */
	int magic, this_one;
	ID next;
	char name[NAME_SIZE];
} AnaObjectStruct, *AnaObject;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	LinkID this_one, next;
	char name[NAME_SIZE];
	/* Link specific data */
	AttributeID attribute[MAX_ATTRIBUTES];
	LinkID nextFromLink;
	BlockID from;	
	LinkID nextToLink;
	BlockID to;
		/* a link can only be on one from list and one to list */
		/* from and to fields link to the beginning of these lists */
} LinkStruct, *Link;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	BlockID this_one, next;
	char name[NAME_SIZE];
	/* Block specific data */
	AttributeID attribute[MAX_ATTRIBUTES];
	Offset hint;
	FileID file;
	int numLinks;
	LinkID firstFromLink;
	LinkID firstToLink;
		/* these are linked lists of links. The `links' in the */
		/* linked list are object IDs not memory addresses */
} BlockStruct, *Block;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	AttributeID this_one, next;
	char name[NAME_SIZE];
} AttributeStruct, *Attribute;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	FileID this_one, next;
	char name[NAME_SIZE];
} FileStruct, *File;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	TextID this_one, next;
	/* Text specific data */
	char s[NAME_SIZE];
} TextStruct, *Text;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	MapID this_one, next;
	char name[NAME_SIZE];
	/* Map specific data */
	ID domain[MAP_SIZE];
	char range[MAP_SIZE][NAME_SIZE];
	char defaultRange[NAME_SIZE];
} MapStruct, *Map;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	ViewID this_one, next;
	char name[NAME_SIZE];
	/* View specific data */
	BlockID blockID;
	MapID fromLinkMap;
	MapID toLinkMap;
	MapID blockMap;
} ViewStruct, *View;

typedef struct {
	/* things in every object */
	ObjectMarker marker;
	MagicNumber magic;
	DocumentID this_one, next;
	char name[NAME_SIZE];
	/* Document specific data */
	ID nextFreeID;
	ViewID initialView;
	/* beginning of chains for each type of object */
	BlockID firstBlock;
	AttributeID firstAttribute;
	LinkID firstLink;
	FileID firstFile;
	ViewID firstView;
	MapID firstMap;
	TextID firstText;
} DocumentStruct, *Document;

#define MARKER_SIZE	10
#define DEFAULT_MARKER	0xFF
#define BeginBlockFlag	'1'
#define EndBlockFlag	'9'

typedef struct {
	char begin_marker;	/* always 0xFF */
	char blockNumber[6];
	char comma;	/* always ',' */
	char flags;	/* various type information */
		/* '1' --> begin block */
		/* '9' --> end block */
	char end_marker;	/* always 0xFF */
} BlockMarkerStruct, *BlockMarker;

typedef struct PickListItem_tag {
	ID id;
	char * name;
	struct PickListItem_tag * next;
} PickListItem;


