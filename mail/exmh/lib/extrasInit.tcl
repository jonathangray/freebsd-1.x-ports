# 
# extrasInit.tcl
#
# This has initialization code for some extra packages.
# The idea is to avoid auto_loading the whole package,
# while still allowing the package to manifest itself
# in the preferences dialog (for example).
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

proc Faces_Init {} {
    global faces
    if {$faces(dir) == {}} {
	set faces(enabled) 0
    }
    Preferences_Add "Faces" \
"Exmh will display the facesaver bitmap of the person that sent the current message (or their organization). This relies on the facesaver database, or the presence of an X-Face mail header.  To decompress X-Face headers, you need uncompface and ikon2xbm." {
	{faces(enabled) facesEnabled ON {Show face bitmap}
"Display the bitmap image from
the facesaver database."}
	{faces(xfaceProg) xfaceProg {} {X-Face pipeline}
"A pipeline to convert an X-Face: header line into
a displayable X11 bitmap.
Typically:   uncompface | ikon2xbm
This is independent of the facesaver database."}
    }
}

proc Sound_Init {} {
    global sound
    if {$sound(cmd) == {}} {
	set sound(enabled) 0
    } else {
	# Preferences_Add will set these variables to Xresource values,
	# but only if the variables are not already defined.
	# These sound variables are defined at install time,
	# so we need to unset them in order honor any per-user defaults.
	set cmd $sound(cmd) ; unset sound(cmd)
	set new $sound(newMsg) ; unset sound(newMsg)
	set err $sound(error) ; unset sound(error)
	Preferences_Add "Sound" \
"Exmh can provide audio feedback if your workstation is capable of playing audio files." \
	[list \
	    { sound(enabled) soundEnabled ON {Sound feedback}
"Enable audio feedback.  Exmh will make a sound when
new messages are incorporated into your folders
(except during startup) and when you try to change
folders without committing moves and delete operations."} \
	    { sound(multifile) soundMultiFile OFF {Play Multiple}
"If your play command can handle multiple audio files,
then set this option.  In this case Exmh can run the
audio program in the background when it needs to
play multiple sounds."} \
	    [list sound(cmd) soundCmd $cmd {Play command} \
"The command line used to play audio files.  You may want
to add flags to control the volume, for example.  The
name of the audio file is appended to this command line."] \
	    [list sound(newMsg) soundNewMsg $new {Sound for a new message} \
"The name of an audio file to play when
new messages have arrived."] \
	    [list sound(error) soundError $err {Sound for an error} \
"The name of an audio file to play when
you forget to commit pending operations."] \
]
    }
}

proc Sedit_Init {} {
    global sedit

    set sedit(init) 1
    set sedit(height) 20
    set sedit(allids) {}

    if ![info exists sedit(key,sendMsg)] {
	set sedit(key,sendMsg) <Control-Return>
    }
    Preferences_Add "Simple Editor" \
"Exmh comes with a simple built in editor called \"sedit\".
It has about 20 keybindings for basic editing operations.
You can tune these bindings with the Bind dialog that defines
bindings for the Text and Entry widget classes."  {
    {sedit(pref,replPrefix) replPrefix "> " {Reply insert prefix}
"This string is prepended to lines included from the reply message
when you use the Insert @ command."}
    {sedit(formatDefault) seditFormatMail ON {Format Mail default}
"Sedit will format mail just before it gets sent out.  This includes
chopping long lines and expanding text/enriched directives.  You
can control whethor or not this happens with the Format Mail menu
item.  This Preference setting chooses the default for that item."}
    {sedit(mhnDefault) seditAttemptMhn OFF {Attempt mhn default}
"Sedit can send your message thru mhn in order to expand its #
MIME formating directives (see the man page about mhn for details).
You can control whethor or not this happens with the Attempt mhn menu
item.  This Preference setting chooses the default for that item."}
    {sedit(lineLength)   seditLineLength 79 {Max Line Length}
"This is the length at which Format Mail chops lines.
It looks around for a word break when chopping."}
    }
}
proc Sedit_CheckPoint {} {
    # Dummy routine overridden when/if sedit.tcl is auto-loaded
}
