proc MakeMapBox {} {
	set name .mapBox

	catch ".destroy $name"
	toplevel $name
	wm title $name "Create Map"
	wm iconname $name "Create Map"
	wm minsize $name 0 0
	
	set attrList ""

	label $name.label1 -text "Create Map"

	label $name.label2 -text "Name of map:"
	entry $name.mapname -relief sunken -exportselection no

	frame $name.alist
	scrollbar $name.alist.scrollbar -relief sunken \
		-command "$name.alist.items view"
	listbox $name.alist.items -scrollCommand "$name.alist.scrollbar set" \
		-relief sunken
	bind $name.alist.items <1> "
		set index \[%W nearest %y\]
		lappend \$attrList \$index
		$name.label3 configure -text \$attrList
	"
	pack append $name.alist \
		$name.alist.scrollbar {left fill} \
		$name.alist.items {right fill expand}

	label $name.label3 -text "Attributes Chosen"

	frame $name.buttons
	button $name.buttons.create -text "Create Attribute" -command "
		Option set returnString \$attrList
		destroy $name
	"
	button $name.buttons.cancel -text "Cancel" -command "
		Option set returnString XXXcancelXXX
		destroy $name
	"
	pack append $name.buttons \
		$name.buttons.create {left fill} \
		$name.buttons.cancel {right fill}

	pack append $name \
		$name.label1 {top fill} \
		$name.label2 {top fill} \
		$name.mapname {top fill} \
		$name.alist {top fill} \
		$name.label3 {top fill} \
		$name.buttons {top fill}
	return $name
}

proc MakePickBox {label} {
	global wcounter

	set wcounter [expr $wcounter+1]
	set name [format ".pb%05d" $wcounter]

	toplevel $name -relief raised
	wm title $name $label
	wm iconname $name $label
	wm minsize $name 0 0
	label $name.label1 -text $label

	entry $name.keyword -relief sunken -exportselection no

	frame $name.slist
	scrollbar $name.slist.scrollbar -relief sunken \
		-command "$name.slist.items view"
	listbox $name.slist.items -scrollCommand "$name.slist.scrollbar set" \
		-relief sunken
	bind $name.slist.items <1> "
		set index \[%W nearest %y\]
		Option set returnString \$index
		destroy $name
	"
	pack append $name.slist \
		$name.slist.scrollbar {left fill} \
		$name.slist.items {right fill expand}

	button $name.close -text "Cancel" -command "
		Option set returnString XXXcancelXXX
		destroy $name
	"

	pack append $name \
		$name.label1 {top fill} \
		$name.keyword {top fill} \
		$name.slist {top fill} \
		$name.close {top fill}
	return $name
}

