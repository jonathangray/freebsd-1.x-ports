OBJS=  LYClean.o LYShowInfo.o LYEdit.o LYStrings.o \
LYMail.o HTAlert.o GridText.o LYGetFile.o \
LYMain.o LYMainLoop.o LYCurses.o LYBookmark.o LYUtils.o \
LYOptions.o LYReadCFG.o LYSearch.o LYHistory.o \
LYForms.o LYPrint.o LYrcFile.o LYDownload.o LYNews.o LYKeymap.o \
HTML.o HTFWriter.o HTInit.o DefaultStyles.o 

CFLAGS= $(MCFLAGS) -I..

all: lynx

lynx:   message $(OBJS) $(WWWLIB)
	@echo "Linking and creating Lynx executable"
	$(CC) $(CFLAGS) -o lynx  $(OBJS) $(WWWLIB) $(LIBS)
	@echo "Copying Lynx executable into this directory"
	cp lynx ..
	@echo "Welcome to Lynx!"

message:
	@echo "Compiling Lynx sources"

traversal:    $(OBJS) traversal.o $(WWWLIB)
	@echo "Making Lynx code"
	$(CC) $(CFLAGS) -o lynx  $(OBJS) traversal.o $(LIBS) $(WWWLIB)
	cp lynx ..

dbg:	$(OBJS) $(WWWLIB)
	@echo "Making Lynx code"
	$(CC) -g $(OBJS) $(CFLAGS) $(WWWLIB) $(LIBS) 

lint:   
	lint *.c  > ../lint.out

clean:
	rm -f core lynx *.[ob]

LYMain.o: ../userdefs.h 
LYMainLoop.o: ../userdefs.h
LYReadCFG.o: ../userdefs.h
HTFWriter.o: ../userdefs.h
LYGetFile.o: ../userdefs.h
LYOptions.o: ../userdefs.h
LYReadCFG.o: ../userdefs.h
LYrcFile.o: ../userdefs.h
LYMail.o: ../userdefs.h
LYUtils.o: ../userdefs.h
HTInit.o: ../userdefs.h
HTML.o: LYCharSets.c
