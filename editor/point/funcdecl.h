/* $Header: /a/cvs/386BSD/ports/editor/point/funcdecl.h,v 1.1 1994/02/15 22:12:42 jkh Exp $ */

#ifdef HYPERTEXT
/* in anaDialogs.c */
PickListItem * GenerateIDList(
#ifdef ANSI_PROTOTYPES
	ID id_in, MagicNumber magic
#endif
);
void FreeIDList(
#ifdef ANSI_PROTOTYPES
	PickListItem * itemList
#endif
);
AttributeID PickAttribute(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
LinkID PickLink(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
BlockID PickBlock(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
FileID PickFile(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
TextID PickText(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
MapID PickMap(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);
ViewID PickView(
#ifdef ANSI_PROTOTYPES
Document, char *
#endif
);

/* in anaObjects.c */
DBM * OpenObjects(
#ifdef ANSI_PROTOTYPES
String
#endif
);
void CloseObjects(
#ifdef ANSI_PROTOTYPES
DBM *
#endif
);
void DumpDB(
#ifdef ANSI_PROTOTYPES
DBM *
#endif
);
AnaObject GetObject(
#ifdef ANSI_PROTOTYPES
DBM *, MagicNumber, ID, AllocationMode
#endif
);
void PutObject(
#ifdef ANSI_PROTOTYPES
DBM *, MagicNumber, AnaObject, ReleaseMode
#endif
);
Block GetBlock(
#ifdef ANSI_PROTOTYPES
DBM *, BlockID, AllocationMode
#endif
);
void PutBlock(
#ifdef ANSI_PROTOTYPES
DBM *, Block, ReleaseMode
#endif
);
Attribute GetAttribute(
#ifdef ANSI_PROTOTYPES
DBM *, AttributeID, AllocationMode
#endif
);
void PutAttribute(
#ifdef ANSI_PROTOTYPES
DBM *, Attribute, ReleaseMode
#endif
);
Map GetMap(
#ifdef ANSI_PROTOTYPES
DBM *, MapID, AllocationMode
#endif
);
void PutMap(
#ifdef ANSI_PROTOTYPES
DBM *, Map, ReleaseMode
#endif
);
File GetFile(
#ifdef ANSI_PROTOTYPES
DBM *, FileID, AllocationMode
#endif
);
void PutFile(
#ifdef ANSI_PROTOTYPES
DBM *, File, ReleaseMode
#endif
);
Text GetText(
#ifdef ANSI_PROTOTYPES
DBM *, TextID, AllocationMode
#endif
);
void PutText(
#ifdef ANSI_PROTOTYPES
DBM *, Text, ReleaseMode
#endif
);
Link GetLink(
#ifdef ANSI_PROTOTYPES
DBM *, LinkID, AllocationMode
#endif
);
void PutLink(
#ifdef ANSI_PROTOTYPES
DBM *, Link, ReleaseMode
#endif
);
View GetView(
#ifdef ANSI_PROTOTYPES
DBM *, ViewID, AllocationMode
#endif
);
void PutView(
#ifdef ANSI_PROTOTYPES
DBM *, View, ReleaseMode
#endif
);
Document GetDocument(
#ifdef ANSI_PROTOTYPES
DBM *, DocumentID, AllocationMode
#endif
);
void PutDocument(
#ifdef ANSI_PROTOTYPES
DBM *, Document, ReleaseMode
#endif
);
Block CreateBlock(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *, AttributeID, Offset, FileID
#endif
);
Attribute CreateAttribute(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *
#endif
);
AttributeID LookupAttributeByName(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *
#endif
);
Map CreateMap(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *
#endif
);
MapID LookupMapByName(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *
#endif
);
Link CreateLink(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *, AttributeID, BlockID, BlockID
#endif
);
File CreateFile(
#ifdef ANSI_PROTOTYPES
DBM *, Document document, char * name
#endif
);
FileID LookupFileByName(
#ifdef ANSI_PROTOTYPES
DBM * db, Document, char *
#endif
);
Text CreateText(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *
#endif
);
View CreateView(
#ifdef ANSI_PROTOTYPES
DBM *, Document, char *, BlockID, MapID, MapID, MapID
#endif
);
Document CreateDocument(
#ifdef ANSI_PROTOTYPES
DBM *, char *
#endif
);

/* in anaSources.c */
int GetRealSelection(
#ifdef ANSI_PROTOTYPES
struct openFile *, int
#endif
);
void InitHypertext(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void CloseHypertext(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void DumpPieces(
#ifdef ANSI_PROTOTYPES
struct window *w
#endif
);
void DumpRealPieces(
#ifdef ANSI_PROTOTYPES
struct window *w
#endif
);
void DumpTables(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void PrintPieceChain(
#ifdef ANSI_PROTOTYPES
char *, Piece
#endif
);
void SeparateBlockMarkers(
#ifdef ANSI_PROTOTYPES
struct window *w
#endif
);
Offset ReadBlockMarker(
#ifdef ANSI_PROTOTYPES
int fid, Offset pos, BlockID * blockID,	unsigned int * flags
#endif
);
Offset FindBlock(
#ifdef ANSI_PROTOTYPES
BlockID blockID, int fid
#endif
);
Offset SkipToEndOfBlock(
#ifdef ANSI_PROTOTYPES
int fid, Offset pos, BlockID endBlockID
#endif
);
void CreateViewPieceTable(
#ifdef ANSI_PROTOTYPES
struct window *w, struct openFile *ff
#endif
);
Offset ProcessOneBlock(
#ifdef ANSI_PROTOTYPES
BlockID blockID, struct window *, Offset offset, Piece *, Piece *
#endif
);
void CreateSpanPieces(
#ifdef ANSI_PROTOTYPES
BlockID blockID, int fid, Offset begin, Offset end, Piece *, Piece *
#endif
);
Offset CreatePieceTableForBlock(
#ifdef ANSI_PROTOTYPES
struct window *, Offset offset, Piece *, Piece *
#endif
);
void FreeOldViewPieces(
#ifdef ANSI_PROTOTYPES
	struct openFile *ff;
#endif
);
int CreateViewFile(
#ifdef ANSI_PROTOTYPES
struct window *w
#endif
);
void AddFileToDocument(
#ifdef ANSI_PROTOTYPES
struct window *w
#endif
);
int InsertBlock(
#ifdef ANSI_PROTOTYPES
unsigned int n
#endif
);
#endif

/* in browser.c */
void ReduceUseCount(
#ifdef ANSI_PROTOTYPES
FileListData *
#endif
);
void ptBrowserLetter(
#ifdef ANSI_PROTOTYPES
int, XKeyEvent *, String *, Cardinal *
#endif
);
void ChangeBrowserFontTo(
#ifdef ANSI_PROTOTYPES
BrowserData *, char *
#endif
);
void RaiseListWindow(
#ifdef ANSI_PROTOTYPES
int n, char * geometry
#endif
);
int listComp(
#ifdef ANSI_PROTOTYPES
char *, char *
#endif
);
void SetBrowserNames(
#ifdef ANSI_PROTOTYPES
BrowserData * browser
#endif
);
void NewFilelist(
#ifdef ANSI_PROTOTYPES
BrowserData *
#endif
);
void CreateNewBrowser(
#ifdef ANSI_PROTOTYPES
int, char * geometry
#endif
);
void NewOpenList(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void CreateBigBrowser(
#ifdef ANSI_PROTOTYPES
BrowserData *, char * geometry
#endif
);
void CreateFilelist(
#ifdef ANSI_PROTOTYPES
void
#endif
);

/* in buffers.c */
void unlink1(
#ifdef ANSI_PROTOTYPES
struct diskBuffer *
#endif
);
struct diskBuffer * getBuffer(
#ifdef ANSI_PROTOTYPES
int, int
#endif
);
void fidInvalid(
#ifdef ANSI_PROTOTYPES
int, int
#endif
);
int getFileByte(
#ifdef ANSI_PROTOTYPES
int, Offset
#endif
);
void ClearByteCache(
#ifdef ANSI_PROTOTYPES
void
#endif
);
int getCachedFileByte(
#ifdef ANSI_PROTOTYPES
int, Offset
#endif
);
int getSpan(
#ifdef ANSI_PROTOTYPES
int, Offset, unsigned char **, unsigned char **, int
#endif
);
void writeChar(
#ifdef ANSI_PROTOTYPES
int, Offset
#endif
);

/* in cmdTable.c */
int FindCommandInTable(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
int GetCommandNumber(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
char * CommandNumberToName(
#ifdef ANSI_PROTOTYPES
int
#endif
);
void AddPointCommands(
#ifdef ANSI_PROTOTYPES
Tcl_Interp *
#endif
);

/* in command.c */
void ptSearchLetter(
#ifdef ANSI_PROTOTYPES
int, XKeyEvent *, String *, Cardinal *
#endif
);
void InitCommands(
#ifdef ANSI_PROTOTYPES
void
#endif
);
char * command(
#ifdef ANSI_PROTOTYPES
PointCommand, char *, char *, char *, char *, char *, char *
#endif
);

/* in copymove.c */
void updateFile(
#ifdef ANSI_PROTOTYPES
int, Offset, Offset, int
#endif
);
void updateTops(
#ifdef ANSI_PROTOTYPES
int, Offset, Offset, int
#endif
);
void exchWithScrap(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void copyToScrap(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, Offset
#endif
);
void insScrap(
#ifdef ANSI_PROTOTYPES
int, int
#endif
);
void copyMove(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, Offset, struct window *, Offset, int
#endif
);
void copyPieces(
#ifdef ANSI_PROTOTYPES
Piece, struct window *, Offset, Offset, int, int
#endif
);

/* in cursor.c */
int cursor(
#ifdef ANSI_PROTOTYPES
char *, char *, int
#endif
);
void doScreenUpdate(
#ifdef ANSI_PROTOTYPES
int, int, int
#endif
);

/* dialogs.c */
int DialogBox(
#ifdef ANSI_PROTOTYPES
char *, char *, char *, char *, char *, char *, int *, int
#endif
);

/* in display.c */
void drawWindowFast(
#ifdef ANSI_PROTOTYPES
struct window *, int, int, int, int
#endif
);
void drawWindow(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void SetWindowNames(
#ifdef ANSI_PROTOTYPES
struct window * w
#endif
);
void banner(
#ifdef ANSI_PROTOTYPES
struct window *, int
#endif
);
void SetSlider(
#ifdef ANSI_PROTOTYPES
struct window *, long
#endif
);
void fillWindow(
#ifdef ANSI_PROTOTYPES
struct window *, int, int, int, int
#endif
);
void DrawString(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void CheckForSelection(
#ifdef ANSI_PROTOTYPES
void
#endif
);
Offset fillLine(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, int, int, int, int, int
#endif
);

/* in execcmd.c */
void execCmd(
#ifdef ANSI_PROTOTYPES
int
#endif
);

/* in fileio.c */
void initFileio(
#ifdef ANSI_PROTOTYPES
void
#endif
);
int getFileId(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
void saveFile(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void writeFile(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
int doWrite(
#ifdef ANSI_PROTOTYPES
int, char *
#endif
);
char * makeTempFor(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
Offset fileSize(
#ifdef ANSI_PROTOTYPES
int
#endif
);
int closeFile(
#ifdef ANSI_PROTOTYPES
int, int
#endif
);

/* findfiles.c */
char * OldFindMatchingFiles(
#ifdef ANSI_PROTOTYPES
char *, char *
#endif
);
char * FindMatchingFiles(
#ifdef ANSI_PROTOTYPES
char *, char *
#endif
);
char * makeFullPathname(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
int striccmp(
#ifdef ANSI_PROTOTYPES
char *, char *
#endif
);
struct window * findFilenameWindow(
#ifdef ANSI_PROTOTYPES
char *
#endif
);

/* in findpos.c */
Offset xyToOffset(
#ifdef ANSI_PROTOTYPES
struct window *, int, int
#endif
);
void OffsetToXY(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, int *, int *
#endif
);
int OffsetToCol(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, Offset
#endif
);

/* goto.c */
void matchChar(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void doGoSel(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void doGoto(
#ifdef ANSI_PROTOTYPES
struct window *, int, int
#endif
);

/* in help.c */
int help(
#ifdef ANSI_PROTOTYPES
int
#endif
);

/* in inschar.c */
void HandleKey(
#ifdef ANSI_PROTOTYPES
int keysym, int state, int handleBackspaces
#endif
);
void insChar(
#ifdef ANSI_PROTOTYPES
int c2, int update
#endif
);

/* in insdel.c */
void insertChar(
#ifdef ANSI_PROTOTYPES
int
#endif
);
int delChar(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void DeleteViewChars(
#ifdef ANSI_PROTOTYPES
void
#endif
);
int deleteChars(
#ifdef ANSI_PROTOTYPES
int, int, int
#endif
);

/* keyword.c */
void findKeyword(
#ifdef ANSI_PROTOTYPES
char * keyword
#endif
);

/* library.c */
struct window * FindWindowByTkName(
#ifdef ANSI_PROTOTYPES
char * name
#endif
);
BrowserData * FindBrowserByTkName(
#ifdef ANSI_PROTOTYPES
char * name
#endif
);
int ConvertGeometrySpec(
#ifdef ANSI_PROTOTYPES
char * geometry, int * x, int * y, int * width, int * height
#endif
);
int indentToShowSelection(
#ifdef ANSI_PROTOTYPES
int
#endif
);
char * ExecTclCommand(
#ifdef ANSI_PROTOTYPES
char * command, int * result
#endif
);
int LineNumberOfSelection(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void FixName(
#ifdef ANSI_PROTOTYPES
char *s
#endif
);
char *tildefyFilename(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
char * findFile(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
char * PtMalloc(
#ifdef ANSI_PROTOTYPES
int, char *
#endif
);
void PtFree(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
void GetPointerPosition(
#ifdef ANSI_PROTOTYPES
int *, int *
#endif
);
int SupplySelectionToX(
#ifdef ANSI_PROTOTYPES
ClientData clientData, int offset, char * buffer, int maxBytes
#endif
);
void AssertSelectionOwnership(
#ifdef ANSI_PROTOTYPES
void
#endif
);
int getSelection(
#ifdef ANSI_PROTOTYPES
char *, int, int
#endif
);
int makeName(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
int getBaseName(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
void justifyLines(
#ifdef ANSI_PROTOTYPES
void
#endif
);
char * noWhiteSpace(
#ifdef ANSI_PROTOTYPES
char *
#endif
);

/* in lines.c */
Offset readLine(
#ifdef ANSI_PROTOTYPES
int, Offset, char *, int
#endif
);
Offset nextLine(
#ifdef ANSI_PROTOTYPES
int, Offset, int *
#endif
);
Offset prevLine(
#ifdef ANSI_PROTOTYPES
int, Offset, int *
#endif
);

/* in point.c */
int main(
#ifdef ANSI_PROTOTYPES
unsigned int, char **
#endif
);
int point(
#ifdef ANSI_PROTOTYPES
unsigned int, char **
#endif
);

/* in pt.c */
int pt(
#ifdef ANSI_PROTOTYPES
unsigned int, char **
#endif
);

/* in makeMenus.c */
void MakeMouseMenuCursors(
#ifdef ANSI_PROTOTYPES
void
#endif
);

/* in makeWindow.c */
void EnterAWindow(
#ifdef ANSI_PROTOTYPES
int, struct window *, XEvent *
#endif
);
void VScroll(
#ifdef ANSI_PROTOTYPES
struct window * w, int how, int y, int button
#endif
);
int DoOneVScroll(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void HScroll(
#ifdef ANSI_PROTOTYPES
struct window * w, int which, int x, int button
#endif
);
void MakeWindow(
#ifdef ANSI_PROTOTYPES
struct window *, char * geometry
#endif
);

/* in mouse.c */
void InitMouse(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void Mouse(
#ifdef ANSI_PROTOTYPES
struct window * w, char * cmd, int x, int y, int clicks
#endif
);

/* in options.c */
void GetResourceOptions(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void InitOptions(
#ifdef ANSI_PROTOTYPES
void
#endif
);
char * GetPointOption(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
void SetPointOption(
#ifdef ANSI_PROTOTYPES
char *, char *
#endif
);


/* in piece.c */
Piece dupPieces(
#ifdef ANSI_PROTOTYPES
Piece
#endif
);
Piece getFreePiece(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void freePieces(
#ifdef ANSI_PROTOTYPES
Piece
#endif
);
Piece findPiece(
#ifdef ANSI_PROTOTYPES
Offset, struct openFile *, Offset *
#endif
);

/* in ptInit.c */
void ptInit(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void msg(
#ifdef ANSI_PROTOTYPES
char *, int
#endif
);

/* in regex.c */
char * re_comp(
#ifdef ANSI_PROTOTYPES
char *pat
#endif
);
int re_exec(
#ifdef ANSI_PROTOTYPES
int fid, int cp, int end_cp, int *lines_passed
#endif
);
int re_exec_reversed(
#ifdef ANSI_PROTOTYPES
int fid, int cp, int end_cp, int *lines_passed
#endif
);
int re_match(
#ifdef ANSI_PROTOTYPES
char *lp
#endif
);
void RegexReplaceAll(
#ifdef ANSI_PROTOTYPES
struct window * w, char * searchFor, char * replaceWith, int inSelection
#endif
);
int RegexReplaceOne(
#ifdef ANSI_PROTOTYPES
struct window * w, char * searchFor, char * replaceWith
#endif
);

/* in repaint.c */
void SetTextColor(
#ifdef ANSI_PROTOTYPES
struct window *w, int normal, int foreground, char * colorName
#endif
);
void CycleColors(
#ifdef ANSI_PROTOTYPES
char **, char *
#endif
);
void InitRedisplay(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void WorkspaceResized(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void repaint(
#ifdef ANSI_PROTOTYPES
struct window *, int, int, int, int
#endif
);

/* replace.c */
void replaceText(
#ifdef ANSI_PROTOTYPES
struct window *, char * fromString, int inSelection
#endif
);
void replaceTextAux(
#ifdef ANSI_PROTOTYPES
struct window *, unsigned char *, unsigned char *, int, int
#endif
);

/* in search.c */
int searchFor(
#ifdef ANSI_PROTOTYPES
struct window * w, int searchMode, char * s, int update
#endif
);

/* in select.c */
void ExtendSelection(
#ifdef ANSI_PROTOTYPES
Offset, int, int, Offset
#endif
);
void drawSelection(
#ifdef ANSI_PROTOTYPES
int
#endif
);
void DrawSection(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, int, int, int, int
#endif
);
void modeExtend(
#ifdef ANSI_PROTOTYPES
struct window *, Offset, int, int, Offset
#endif
);
Offset adjustSelMode(
#ifdef ANSI_PROTOTYPES
Offset
#endif
);
int XSelConvert(
#ifdef ANSI_PROTOTYPES
int, Atom *, Atom *, Atom *, char * *, unsigned long *, int *
#endif
);
void XSelLose(
#ifdef ANSI_PROTOTYPES
int, Atom *
#endif
);

/* shellWindow.c */
int ConnectToPty(
#ifdef ANSI_PROTOTYPES
char * arg1, char * arg2, char * arg3, char * arg4, char * arg5, char * arg6
#endif
);

/* spans.c */
Offset searchSpans(
#ifdef ANSI_PROTOTYPES
int, Offset, Offset, char *, int, int *
#endif
);
Offset searchReverseSpans(
#ifdef ANSI_PROTOTYPES
int, Offset, Offset, char *, int, int *
#endif
);
unsigned char * match1up(
#ifdef ANSI_PROTOTYPES
unsigned char *, int, int, int
#endif
);
unsigned char * match1dn(
#ifdef ANSI_PROTOTYPES
unsigned char *, int, int, int
#endif
);
unsigned char * match2dn(
#ifdef ANSI_PROTOTYPES
unsigned char *, int, int
#endif
);
int countnl(
#ifdef ANSI_PROTOTYPES
unsigned char *, int
#endif
);

/* stats.c */
void PrintStats(
#ifdef ANSI_PROTOTYPES
int fileId
#endif
);

/* tags.c */
void findCTag(
#ifdef ANSI_PROTOTYPES
char * ctag
#endif
);

/* tcl.c */
int doPtCommand(
#ifdef ANSI_PROTOTYPES
ClientData clientData, Tcl_Interp * interp, int argc, char *argv[]
#endif
);
void AddPointCommands(
#ifdef ANSI_PROTOTYPES
Tcl_Interp *
#endif
);
void ptTcl(
#ifdef ANSI_PROTOTYPES
int, XButtonEvent *, String *, Cardinal *
#endif
);

/* undoredo.c */
void initChanges(
#ifdef ANSI_PROTOTYPES
void
#endif
);
struct changeItem * GetCurrentChange(
#ifdef ANSI_PROTOTYPES
struct openFile * ff
#endif
);
struct changeItem * GetNewChange(
#ifdef ANSI_PROTOTYPES
struct openFile * ff
#endif
);
void RecordChange(
#ifdef ANSI_PROTOTYPES
struct openFile * ff, struct changeItem * new_change
#endif
);
void redo(
#ifdef ANSI_PROTOTYPES
struct openFile * ff, int count
#endif
);
void again(
#ifdef ANSI_PROTOTYPES
struct openFile * ff, int mostRecent
#endif
);
void UpdateUndoList(
#ifdef ANSI_PROTOTYPES
struct openFile * ff
#endif
);
void ShowUndos(
#ifdef ANSI_PROTOTYPES
struct openFile * ff
#endif
);
void undo(
#ifdef ANSI_PROTOTYPES
struct openFile * ff, int count
#endif
);

/* in shellWindow.c */
void OpenShellWindow(
#ifdef ANSI_PROTOTYPES
char * arg1, char * arg2, char * arg3, char * arg4, char * arg5, char * arg6
#endif
);

/* in userInput.c */
void FixName(
#ifdef ANSI_PROTOTYPES
char *
#endif
);
void CommandHandler(
#ifdef ANSI_PROTOTYPES
int, char *, char *
#endif
);
void GetKeystrokes(
#ifdef ANSI_PROTOTYPES
int, XKeyEvent *
#endif
);

/* in windows.c */
void initWindows(
#ifdef ANSI_PROTOTYPES
void
#endif
);
void MakeWindowActive(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
struct window * createWindow(
#ifdef ANSI_PROTOTYPES
struct window * w, char * filename, char * geometry
#endif
);
int closeWindow(
#ifdef ANSI_PROTOTYPES
struct window *, int
#endif
);
void topWindow(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void ZoomWindow(
#ifdef ANSI_PROTOTYPES
struct window *, int
#endif
);
struct window * GetNewFile(
#ifdef ANSI_PROTOTYPES
struct window * w, char * filename, char * geometry, int doNotAsk
#endif
);
void doNewWindow(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);
void bottomFile(
#ifdef ANSI_PROTOTYPES
struct window *
#endif
);

