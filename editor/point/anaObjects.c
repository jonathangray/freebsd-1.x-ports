/* $Header: /a/cvs/386BSD/ports/editor/point/anaObjects.c,v 1.1 1994/02/15 22:12:35 jkh Exp $ */

#ifdef HYPERTEXT
#include <sys/types.h>
#include <sys/file.h>
#include <string.h>
#include "pt.h"

typedef struct object_table_tag {
	char *name;
	int size;
} object_table_entry;

static object_table_entry ObjectTable[] = {
	{"", 0},	/* dummy so we can avoid magic numbers of 0 */
	{"Attribute", sizeof(AttributeStruct)},
	{"Block", sizeof(BlockStruct)},
	{"Document", sizeof(DocumentStruct)},
	{"File", sizeof(FileStruct)},
	{"Link", sizeof(LinkStruct)},
	{"Map", sizeof(MapStruct)},
	{"Marker", sizeof(BlockMarkerStruct)},
	{"Text", sizeof(TextStruct)},
	{"View", sizeof(ViewStruct)},
};

DBM *
OpenObjects(name)
	String name;
{
	return dbm_open( name, O_RDWR | O_CREAT, 0644 );
}

void
CloseObjects( db )
	DBM *db;
{
	dbm_close( db );
}

void
DumpDB( db )
	DBM *db;
{
	datum d_out;
	
	d_out = dbm_firstkey( db );
	while( d_out.dptr != NULL ) {
		AnaObject object = (AnaObject)d_out.dptr;
		printf("magic=%d id=%d next=%d name is %s\n",
			object->magic, object->this_one,
			object->next, object->name);
		d_out = dbm_nextkey( db );
	}
}

AnaObject
GetObject( db, magic, id, allocate )
	DBM *db;
	MagicNumber magic;
	ID id;
	AllocationMode allocate;
{
	datum d_in, d_out;
	AnaObject object;

	d_in.dptr = (char *)&id;
	d_in.dsize = sizeof(ID);
	d_out = dbm_fetch( db, d_in );
	if( d_out.dptr == NULL ) {
		printf("ndbm ERROR in Get%s can't find id=%d\n",
			ObjectTable[magic].name, id);
		dbm_clearerr( db );
		return NULL;
	}
	if( d_out.dsize != ObjectTable[magic].size ) {
		printf("ndbm ERROR in Get%s id=%d, wrong size.",
			ObjectTable[magic].name, id);
		printf(" Size is %d and it should be %d\n", d_out.dsize,
			ObjectTable[magic].size );
		return NULL;
	}
	object = (AnaObject)d_out.dptr;
	if( object->magic != magic ) {
		printf("ndbm ERROR in Get%s id=%d, wrong magic number.",
			ObjectTable[magic].name, id);
		printf("Magic number is %d and it should be %d\n",
			object->magic, magic);
		return NULL;
	}
	if( allocate ) {
		object = (AnaObject)PtMalloc( d_out.dsize, "ana object" );
		/* copy d_out.dptr into object */
		memcpy( (char *)object, (char *)d_out.dptr, d_out.dsize );
	}
	return object;
}

void
PutObject( db, magic, object, release )
	DBM *db;
	MagicNumber magic;
	AnaObject object;
	ReleaseMode release;
{
	datum d_key, d_object;
	int ret;

	if( object == NULL )
		return;
	d_key.dptr = (char *)&(object->this_one);
	d_key.dsize = sizeof(ID);
	d_object.dptr = (char *)object;
	d_object.dsize = ObjectTable[magic].size;
	ret = dbm_store( db, d_key, d_object, DBM_REPLACE);
	if( ret != 0 )
		printf("ndbm ERROR in Put%s, ret=%d\n",
			ObjectTable[magic].name, ret);
	if( release )
		PtFree( (char *)object );
}

Block
GetBlock( db, blockID, allocate )
	DBM *db;
	BlockID blockID;
	AllocationMode allocate;
{
	return (Block)GetObject( db, BlockMagic, (ID)blockID, allocate );
}

void
PutBlock( db, block, release )
	DBM *db;
	Block block;
	ReleaseMode release;
{
	PutObject( db, BlockMagic, (AnaObject)block, release );
}

Attribute
GetAttribute( db, attributeID, allocate )
	DBM *db;
	AttributeID attributeID;
	AllocationMode allocate;
{
	return (Attribute)GetObject(db,AttributeMagic,(ID)attributeID,allocate);
}

void
PutAttribute( db, attribute, release )
	DBM *db;
	Attribute attribute;
	ReleaseMode release;
{
	PutObject( db, AttributeMagic, (AnaObject)attribute, release );
}

Map
GetMap( db, mapID, allocate )
	DBM *db;
	MapID mapID;
	AllocationMode allocate;
{
	return (Map)GetObject( db, MapMagic, (ID)mapID, allocate );
}

void
PutMap( db, map, release )
	DBM *db;
	Map map;
	ReleaseMode release;
{
	PutObject( db, MapMagic, (AnaObject)map, release );
}

File
GetFile( db, fileID, allocate )
	DBM *db;
	FileID fileID;
	AllocationMode allocate;
{
	return (File)GetObject( db, FileMagic, (ID)fileID, allocate );
}

void
PutFile( db, file, release )
	DBM *db;
	File file;
	ReleaseMode release;
{
	PutObject( db, FileMagic, (AnaObject)file, release );
}

Text
GetText( db, textID, allocate )
	DBM *db;
	TextID textID;
	AllocationMode allocate;
{
	return (Text)GetObject( db, TextMagic, (ID)textID, allocate );
}

void
PutText( db, text, release )
	DBM *db;
	Text text;
	ReleaseMode release;
{
	PutObject( db, TextMagic, (AnaObject)text, release );
}

Link
GetLink( db, linkID, allocate )
	DBM *db;
	LinkID linkID;
	AllocationMode allocate;
{
	return (Link)GetObject( db, LinkMagic, (ID)linkID, allocate );
}

void
PutLink( db, link, release )
	DBM *db;
	Link link;
	ReleaseMode release;
{
	PutObject( db, LinkMagic, (AnaObject)link, release );
}

View
GetView( db, viewID, allocate )
	DBM *db;
	ViewID viewID;
	AllocationMode allocate;
{
	return (View)GetObject( db, ViewMagic, (ID)viewID, allocate );
}

void
PutView( db, view, release )
	DBM *db;
	View view;
	ReleaseMode release;
{
	PutObject( db, ViewMagic, (AnaObject)view, release );
}

Document
GetDocument( db, documentID, allocate )
	DBM *db;
	DocumentID documentID;
	AllocationMode allocate;
{
	return (Document)GetObject(db, DocumentMagic, (ID)documentID, allocate);
}

void
PutDocument( db, document, release )
	DBM *db;
	Document document;
	ReleaseMode release;
{
	PutObject( db, DocumentMagic, (AnaObject)document, release );
}

Block
CreateBlock( db, document, name, attribute, hint, file )
	DBM *db;
	Document document;
	char *name;
	AttributeID attribute;
	Offset hint;
	FileID file;
{
	Block block;
	int i;

	block = (Block)PtMalloc( sizeof(BlockStruct), "ana block" );
	block->magic = BlockMagic;
	block->this_one = (document->nextFreeID)++;
	block->next = document->firstBlock;
	document->firstBlock = block->this_one;
	strncpy( block->name, name, NAME_SIZE );
	for( i = 1; i < MAX_ATTRIBUTES; ++i )
		 block->attribute[i] = NullObject;
	block->attribute[0] = attribute;
	block->hint = hint;
	block->file = file;
	block->numLinks = 0;
	block->firstFromLink = NullObject;
	block->firstToLink = NullObject;
	PutBlock( db, block, NO_RELEASE );
	return block;
}

Attribute
CreateAttribute( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	Attribute attribute;

	attribute = (Attribute)PtMalloc( sizeof(AttributeStruct),
						"ana attribute" );
	attribute->magic = AttributeMagic;
	attribute->this_one = (document->nextFreeID)++;
	attribute->next = document->firstAttribute;
	document->firstAttribute = attribute->this_one;
	strncpy( attribute->name, name, NAME_SIZE );
	PutAttribute( db, attribute, NO_RELEASE );
	return attribute;
}

/*ARGSUSED*/
AttributeID
LookupAttributeByName( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	extern DBM *currentDB;

	Attribute attribute;
	int attributeID = document->firstAttribute;

	while( attributeID != NullObject ) {
		attribute = GetAttribute( currentDB, attributeID, NO_ALLOCATE );
		if( strcmp(name,attribute->name) == 0 )
			return attributeID;
		attributeID = attribute->next;
	}
	return NullObject;
}

Map
CreateMap( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	Map map;
	int i;

	map = (Map)PtMalloc( sizeof(MapStruct), "ana map" );
	map->magic = MapMagic;
	map->this_one = (document->nextFreeID)++;
	map->next = document->firstMap;
	document->firstMap = map->this_one;
	strncpy( map->name, name, NAME_SIZE );
	for( i = 0; i < MAP_SIZE; ++i ) {
		map->domain[i] = NullObject;
		map->range[i][0] = '\0';
	}
	PutMap( db, map, NO_RELEASE );
	return map;
}

/*ARGSUSED*/
MapID
LookupMapByName( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	extern DBM *currentDB;

	Map map;
	int mapID = document->firstMap;

	while( mapID != NullObject ) {
		map = GetMap( currentDB, mapID, NO_ALLOCATE );
		if( strcmp(name,map->name) == 0 )
			return mapID;
		mapID = map->next;
	}
	return NullObject;
}

Link
CreateLink( db, document, name, attribute, from, to )
	DBM *db;
	Document document;
	char *name;
	AttributeID attribute;
	BlockID from, to;
{
	Link link;
	int i;

	link = (Link)PtMalloc( sizeof(LinkStruct), "ana link" );
	link->magic = LinkMagic;
	link->this_one = (document->nextFreeID)++;
	link->next = document->firstLink;
	document->firstLink = link->this_one;
	strncpy( link->name, name, NAME_SIZE );
	for( i = 1; i < MAX_ATTRIBUTES; ++i )
		 link->attribute[i] = NullObject;
	link->attribute[0] = attribute;
	link->nextFromLink = NullObject;
	link->from = from;
	link->nextToLink = NullObject;
	link->to = to;
	PutLink( db, link, NO_RELEASE );
	return link;
}

File
CreateFile( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	File file;

	file = (File)PtMalloc( sizeof(FileStruct), "ana file" );
	file->magic = FileMagic;
	strncpy( file->name, name , NAME_SIZE );
	file->this_one = (document->nextFreeID)++;
	file->next = document->firstFile;
	document->firstFile = file->this_one;
	PutFile( db, file, NO_RELEASE );
	return file;
}

/*ARGSUSED*/
FileID
LookupFileByName( db, document, name )
	DBM *db;
	Document document;
	char *name;
{
	extern DBM *currentDB;

	File file;
	int fileID = document->firstFile;

	while( fileID != NullObject ) {
		file = GetFile( currentDB, fileID, NO_ALLOCATE );
		if( strcmp(name,file->name) == 0 )
			return fileID;
		fileID = file->next;
	}
	return NullObject;
}

Text
CreateText( db, document, s )
	DBM *db;
	Document document;
	char *s;
{
	Text text;

	text = (Text)PtMalloc( sizeof(TextStruct), "ana text" );
	text->magic = TextMagic;
	text->this_one = (document->nextFreeID)++;
	text->next = document->firstText;
	document->firstText = text->this_one;
	strncpy( text->s, s, NAME_SIZE );
	PutText( db, text, NO_RELEASE );
	return text;
}

View
CreateView( db, document, name, blockID, fromLinkMap, toLinkMap, blockMap )
	DBM *db;
	Document document;
	char *name;
	BlockID blockID;
	MapID fromLinkMap, toLinkMap, blockMap;
{
	View view;

	view = (View)PtMalloc( sizeof(ViewStruct), "ana view" );
	view->magic = ViewMagic;
	view->this_one = (document->nextFreeID)++;
	view->next = document->firstView;
	document->firstView = view->this_one;
	strncpy( view->name, name, NAME_SIZE );
	view->blockID = blockID;
	view->fromLinkMap = fromLinkMap;
	view->toLinkMap = toLinkMap;
	view->blockMap = blockMap;
	PutView( db, view, NO_RELEASE );
	return view;
}

Document
CreateDocument( db, name )
	DBM *db;
	char *name;
{
	Document document;

	document = (Document)PtMalloc( sizeof(DocumentStruct), "ana document" );
	document->magic = DocumentMagic;
	document->this_one = 1;
	document->next = NullObject;
	strncpy( document->name, name, NAME_SIZE );
	/* start IDs at 100 so we can use low ids for conventional */
	/* purposes, like open fileIds as IDs */
	document->nextFreeID = 100;
	document->initialView = NullObject;
	document->firstBlock = NullObject;
	document->firstAttribute = NullObject;
	document->firstLink = NullObject;
	document->firstFile = NullObject;
	document->firstView = NullObject;
	document->firstMap = NullObject;
	document->firstText = NullObject;
	PutDocument( db, document, NO_RELEASE );
	return document;
}
#endif

