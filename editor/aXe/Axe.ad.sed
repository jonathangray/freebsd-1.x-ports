Axe.version:		VERSION
*axeLibDir:		AXELIBDIR

*allowShellResize:	True

EXTENSION*AxeEditor.translations:#override\n\
                        <FocusIn>:interp-focus()

*box.Command.highlightThickness:1

*showGrip:        	False

*fileName.justify:	left

*buttons*AxeCommand.borderWidth:0

/**/ Replicate AxeEditor's default font list here to snuff out HP_VUE's
/**/ server setting of *FontList for some other purpose
*FontList:		Small:6x13  Medium:8x13  Large:9x15  Huge:10x20

*buttons.columnSpacing:         2
*buttons.AxeMenuButton.borderWidth:0
*buttons*highlightThickness:    20

*moveMenu.label:	Move
*moveMenu.help:		Activate pulldown menu of move operations
*moveMenu.menuName:	mMove

*searchMenu.label:	Search
*searchMenu.help:	Activate pulldown menu of search operations
*searchMenu.menuName:	mSearch

*insertMenu.label:	Insert
*insertMenu.help:	Activate pulldown menu of insert operations
*insertMenu.menuName:	mInsert

*deleteMenu.label:	Delete
*deleteMenu.help:	Activate pulldown menu of delete operations
*deleteMenu.menuName:	mDelete

*helpMenu.label:	Help
*helpMenu.help:		Activate pulldown menu of help operations
*helpMenu.menuName:	mHelp

*miscMenu.label:	Misc
*miscMenu.help:		Activate pulldown menu of miscellaneous operations
*miscMenu.menuName:	mMisc

*fontMenu.label:	Font
*fontMenu.help:		Activate pulldown menu of font names
*fontMenu.menuName:	mFont

*windowMenu.label:	Window
*windowMenu.help:	Activate pulldown menu of window operations
*windowMenu.menuName:	mWindow

*bufferMenu.label:	Buffer
*bufferMenu.help:	Activate pulldown menu of buffer operations
*bufferMenu.menuName:	mBuffer

*showMenu.label:	Show
*showMenu.help:	Activate pulldown menu showing names of files being edited
*showMenu.menuName:	mShow
*mShow.AxeSmeBSB.justify:  left
*mShow.AxeSmeBSB.leftMargin:16
*mShow.AxeSmeBSB.rightMargin:16
*mShow*AxeSmeBSB.help:  Raise window/buffer corresponding to highlighted entry

*fileMenu.label:	File
*fileMenu.help:		Activate pulldown menu of file operations
*fileMenu.menuName:	mFile

*quitMenu.label:	Quit
*quitMenu.help:		Activate pulldown menu of quit operations
*quitMenu.menuName:	mQuit

*axeLogo.internalWidth: 0
*buttons.axeLogo.translations:#override\n<Enter>: unhighlight() update-info()
*axeLogo.menuName:	mLogo
*mLogo.translations:    #replacee \n <BtnUp>: MenuPopdown()

*AxeSimpleMenu.translations:#override \n\
                        <EnterWindow>: update-info(Reset) \n\
                        <BtnMotion>: highlight() update-info()
*AxeSmeBSB.vertSpace:	20
*AxeSmeBSB.justify:	center

*mbeg.label:		Beginning
*mbeg.help:		Move insertion point to beginning of line  [ Ctrl-a ]
*mend.label:		End
*mend.help:		Move insertion point to end of line  [ Ctrl-e ]
*mup.label:		Up
*mup.help:		Move insertion point up one page  [ Meta-v ]
*mdown.label:		Down
*mdown.help:		Move insertion point down one page  [ Ctrl-v ]
*mtop.label:		Top
*mtop.help:		Move insertion point to top of file  [ Meta-< ]
*mbot.label:		Bottom
*mbot.help:		Move insertion point to bottom of file  [ Meta-> ]

*mSearch.AxeSmeBSB.leftMargin:16
*mSearch.AxeSmeBSB.rightMargin:16

*sbtext.label:		Text
*sbtext.help:   Pop up dialogue box for backward text search  [ Ctrl-r ]
*sftext.label:		Text
*sftext.help:	Pop up dialogue box for forward text search  [ Ctrl-s ]
*sline.label:		Line
*sline.help:	Pop up numeric pad for entering the number of a line to go to
*scaret.label:		Caret
*scaret.help:		Scroll text so that insertion point is in view
*sbsel.label:		Selection
*sbsel.help:		Search backward for the current selection
*sfsel.label:		Selection
*sfsel.help:		Search forward for the current selection

*ifile.label:		File
*ifile.help:		Pop up file selector
*isel.label:		Selection
*isel.help:		Insert current selection
*ipaste.label:		Paste
*ipaste.help:		Insert text deleted using Cut  [ Ctrl-y ]
*ictrl.label:           Control codes
*ictrl.help:            Pop up table of control codes for making selection

*dword.label:		Word
*dword.help:	Delete current word, or next if between words  [ Meta-f Meta-b Meta-d ]
*dline.label:		Line
*dline.help:		Delete current line  [ Ctrl-a Ctrl-k ]
*dsel.label:		Selection
*dsel.help:		Delete selected (highlighted) text
*dcut.label:		Cut
*dcut.help:		Delete selected text for later pasting  [ Ctrl-w ]

*hgnrl.label:		General
*hgnrl.help:		Pop up general help window
*hbind.label:		Bindings
*hbind.help:		Pop up window showing default key/button bindings
*hcust.label:		Customise
*hcust.help:		Pop up window explaining how to customise aXe
*hpop.label:		Popups
*hpop.help:	        Pop up window explaining aXe's popups
*hextn.label:	        Extension
*hextn.help:            Pop up window explaining aXe's extension language
*hchng.label:           Changes
*hchng.help:            Pop up window describing the changes since the last release

*mMisc.AxeSmeBSB.leftMargin:16
*mMisc.AxeSmeBSB.rightMargin:16

*mundo.label:		Undo
*mundo.help:		Undo the last operation that changed the buffer
*mwhere.label:		Where?
*mwhere.help:	Display the current line number and position within line
*mform.label:		Format
*mform.help:		Format the current paragraph  [ Meta-q ]
*mcentre.label:		Centre
*mcentre.help:	Redraw text window with current line centred vertically  [ Ctrl-l ]
*mhcentr.label:		Centre
*mhcentr.help:		Centre current line horizontally
*mpref.label:		Preferences
*mpref.help:	Pop up dialogue for temporarily setting user preferences

*mFont.AxeSmeBSB.justify:	left
*mFont.AxeSmeBSB.leftMargin:16

*fndflt.label:		Default

*wnew.label:		New
*wnew.help:		Create a new empty editing window
*wfull.label:		New & Load
*wfull.help:		Create a new editing window and load a file into its buffer
*wclose.label:		Close
*wclose.help:		Destroy this window, with check for unsaved changes
*wclall.label:		Close All
*wclall.help:		Destroy all windows, with check for unsaved changes
*wicon.label:		Iconify All
*wicon.help:		Iconify all windows
*wdeicon.label:		Deiconify All
*wdeicon.help:		Deiconify all windows

*bempty.label:		New
*bempty.help:		Create a new empty buffer in this window
*bfull.label:		New & Load
*bfull.help:		Create a new buffer in this window and load a file into it
*bclear.label:          Clear
*bclear.help:           Clear text in this buffer making it ready for reuse
*bclose.label:		Close
*bclose.help:		Destroy this buffer, with check for unsaved changes

*fsvex.label:		Save and Close
*fsvex.help:	        Save changes in all buffers, closing where the save is successful
*fsvall.label:		Save All
*fsvall.help:		Save changes in all buffers and continue editing
*fsave.label:		Save
*fsave.help:		Save changes in this buffer and continue editing
*fsvas.label:		Save As
*fsvas.help:		Save this buffer under new name then continue editing that file
*fload.label:		Load
*fload.help:		Edit a different file in this buffer
*frvrt.label:		Reload
*frvrt.help:	        Reload file associated with this buffer, with check for unsaved changes

*qquit.label:		Close All
*qquit.help:		Destroy all windows, with check for unsaved changes
*qsvex.label:		Save and Close All
*qsvex.help:		Save changes in all buffers and close all windows if successful
*qsvclw.label:		Save and Close Window
*qsvclw.help:		Save changes in all buffers in this window and close it if successful
*qsvclb.label:		Save and Close Buffer
*qsvclb.help:		Save changes in this buffer and close it if successful


*XpTable*mbeg.label:		Begin
*XpTable*mend.label:		End
*XpTable*mup.label:		Up
*XpTable*mdown.label:		Down
*XpTable*mtop.label:		Top
*XpTable*mbot.label:		Botm

*XpTable*sbtext.label:		<-Text
*XpTable*sftext.label:		Text->
*XpTable*sline.label:		Line
*XpTable*scaret.label:		Caret
*XpTable*sbsel.label:		<-Sel'n
*XpTable*sfsel.label:		Sel'n->

*XpTable*ifile.label:		File
*XpTable*isel.label:		Sel'n
*XpTable*ipaste.label:		Paste
*XpTable*ictrl.label:		Ctrls

*XpTable*dword.label:		Word
*XpTable*dline.label:		Line
*XpTable*dsel.label:		Sel'n
*XpTable*dcut.label:		Cut

*XpTable*hgnrl.label:		Gen'l
*XpTable*hbind.label:		Bind
*XpTable*hcust.label:		Custom
*XpTable*hpop.label:		Popups
*XpTable.hextn.label:		Extend
*XpTable*hchng.label:		Chngs

*XpTable*mundo.label:		Undo
*XpTable*mwhere.label:		Where?
*XpTable*mform.label:		Format
*XpTable*mcentre.label:		Cent(V)
*XpTable*mhcentr.label:		Cent(H)
*XpTable*mpref.label:		Prefs

*XpTable*wnew.label:		New
*XpTable*wfull.label:		New+
*XpTable*wclose.label:		Close
*XpTable*wclall.label:		Quit
*XpTable*wicon.label:		Iconify
*XpTable*wdeicon.label:	        Deic'fy

*XpTable*bempty.label:		New
*XpTable*bfull.label:           New+
*XpTable*bclear.label:		Clear
*XpTable*bclose.label:		Close

*XpTable*fsvex.label:		Exit
*XpTable*fsvall.label:		Sv All
*XpTable*fsave.label:		Save
*XpTable*fsvas.label:		Sv As
*XpTable*fload.label:		Load
*XpTable*frvrt.label:		Reload

*XpTable*qquit.label:		Quit
*XpTable*qsvex.label:		Close
*XpTable*qsvclw.label:		Clos(W)
*XpTable*qsvclb.label:		Clos(B)

*ed.baseTranslations:#override\n\
     			!:<Key>): match-parens() \n\
     			!:<Key>]: match-parens() \n\
     			!:<Key>}: match-parens() \n\
     			<Btn1Up>: extend-end(PRIMARY, CUT_BUFFER0) where() \n\
     			<Btn2Up>: where() \n\
     			<Btn3Up>: extend-end(PRIMARY, CUT_BUFFER0) where()
*ed.scrollVertical:	always
*ed.wrap:               line

*miniBuffer*editType:   Edit
*miniBuffer*pieceSize:  32
*miniBuffer.displayCaret:False

*miniMenu.internalWidth:2
*miniMenu.bitmap:       menu10
*miniMenu.help:		Activate menu of minibuffer operations
*miniMenu.menuName:	mMini
*miniMenu.translations:	#override \n\
                        <Enter>: highlight() update-info() \n\
                        <Leave>: reset()

*miniMenu:              print:lpr spell nroff mail | \
                        exec:mini-commit abort:mini-abort | \
                        print<RET>:lpr\n spell<RET>:spell\n nroff<RET>:nroff\n

*mMini.print.help:      Ready minibuffer with command to print buffer/selection
*mMini.spell.help:      Ready minibuffer with command to spell check buffer/selection
*mMini.nroff.help:      Ready minibuffer with command to apply nroff to buffer/selection
*mMini.mail.help:       Ready minibuffer with command to mail buffer/selection
*mMini.exec.help:       Re-execute the command in the minibuffer
*mMini.abort.help:      Remove minibuffer accelerators
*mMini.print<RET>.help: Print the buffer/selection
*mMini.spell<RET>.help: Check spelling of buffer/selection
*mMini.nroff<RET>.help: Run buffer/selection through nroff

*FileNominator*path.leftBitmap:menu12
*FileNominator*Filter.label:filter
*FileNominator*Filter.leftBitmap:menu12
*FileNominator*PathList:$HOME

*Preference.marginWidth:       5
*Preference.marginHeight:      5
*Preference.rowSpacing:        5
*Preference.columnSpacing:     5

*Preference*Label.borderWidth: 0
*Preference.Box.orientation:   horizontal
*Preference.Box.borderWidth:   0
*Preference.Box.vSpace:        2
*Preference*Toggle.translations: #override \n\
                               <Btn1Down>,<Btn1Up>: set() notify()
*Preference*autoFill.label:    Auto Fill
*Preference*autoFill.translations:#override \n\
                               <Btn1Down>,<Btn1Up>: toggle() notify()

*Preference.lscroll.label:     Scrollbar:
*Preference.lwrap.label:       Wrap Mode:
*Preference.ledmode.label:     Edit Mode:
*Preference.ltabs.label:       Tab Every:
*Preference.lfnom.label:       Nominator:
*Preference.lapply.label:      \ Apply To:

*Confirmer*Label.borderWidth:  0
*Confirmer*Box.orientation:    horizontal
*Confirmer*Box.borderWidth:    0
*Confirmer*Box.vSpace:         2
*Confirmer*confirm.accelerators:    Ctrl<Key>a: set() notify() reset()
*Confirmer*alternative.accelerators:Ctrl<Key>b: set() notify() reset()
*Confirmer*cancel.label:       cancel (^C)
*Confirmer*cancel.accelerators:     Ctrl<Key>c: set() notify() reset()

*NumericPad*Toggle.translations:#override\n<Btn1Down>,<Btn1Up>: set() notify()
*NumericPad*text.width:	       1

*ControlCodeSelector*list.columnSpacing:15
*ControlCodeSelector*list.defaultColumns:3
*ControlCodeSelector.base:8

*fileServer.title:	       aXe
*fileServer.iconName:	       aXe

*fileServer*select.label:      Edit
*fileServer*path.label:	       Path
*fileServer*cancel.label:      Quit

*server.title:		       aXe
*server.iconName:	       aXe
*server.Height:                125

*table.layout:		       waiter 0 0 2; show 0 1 2 hH; poleaxe 0 2 hH; help 1 2 hH;
*table.rowSpacing:	       5
*table.columnSpacing:          5
*table.marginWidth:	       5
*table.marginHeight:           5 

*waiter.label:                 Edit
*waiter.width:		       80

*show.label:		       Show
*show.leftBitmap:	       menu12
*show.menuName:		       mShow

*poleaxe.label:                Quit

*help.label:                   Help
