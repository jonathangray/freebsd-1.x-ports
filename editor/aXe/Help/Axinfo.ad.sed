Axinfo.version:		VERSION
*infoPath:	        AXINFOLIBDIR

*hyperwin.title:	axinfo
*hyperwin.iconName:	axinfo
*hyperwin.translations:	#override \n\
			<Message>WM_PROTOCOLS:delete-hyper()

*view.allowVert:	True
*view.forceBars:	True

*hyper.rows:		24
*hyper.columns:		80

*Hyper.normalFont:	7x13
*Hyper.highlightFont:	7x13bold
*Hyper.margin:		4

*hpane*Command.translations: #override \n\
			<Enter>: highlight() show-info() \n\
			<Leave>: reset() clear-info()

*last.label:		Last

*map.label:		Tree

*treewin.title:		axinfo
*treewin.iconName:	axinfo-tree
*treewin.translations:	#override \n\
			<Message>WM_PROTOCOLS:delete-tree()

*Paned*showGrip:	False

*porthole.width:	450
*porthole.height:	326
*porthole.borderWidth:	0

*tree.translations:	#override \n\
			<Enter>: show-info() \n\
			<Leave>: clear-info() \n\
			<BtnDown>: start-drag() \n\
			<BtnMotion>: drag()

*treewin*Command.translations: #override \n\
			<Enter>: highlight() show-info() \n\
			<Leave>: reset() clear-info()

*quit.label:		Quit
*close.label:		Delete
*info.label:

*Confirmer*Label.borderWidth:   0
*Confirmer*Box.orientation:     horizontal
*Confirmer*Box.borderWidth:     0
*Confirmer*Box.vSpace:          2
*Confirmer*confirm.accelerators:Ctrl<Key>a: set() notify() reset()
*Confirmer*alternative.accelerators:Ctrl<Key>b: set() notify() reset()
*Confirmer*cancel.label:        cancel (^C)
*Confirmer*cancel.accelerators: Ctrl<Key>c: set() notify() reset()

*listen.translations:	#override \n\
			<PropertyNotify>AXE_AXINFO: notify() 
