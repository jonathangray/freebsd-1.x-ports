#ifx11
.TH XDVI 1 "7 June 1993" "X Version 11"
#ifx10
.TH XDVI 1 "7 June 1993" "X Version 10"
#endif
.SH NAME
xdvi \- DVI Previewer for the X Window System
.SH SYNOPSIS
.B xdvi
.nh
[+[\fIpage\fP]] [\-s \fIshrink\fP] [\-S \fIdensity\fP] [\-p \fIpixels\fP] [\-l]
[\-paper \fIpapertype\fP] [\-mgs[\fIn\fP] \fIsize\fP]
[\-hushspecials] [\-hushchars] [\-hush] [\-altfont \fIfont\fP]
[\-margins \fIdimen\fP] [\-sidemargin \fIdimen\fP] [\-topmargin \fIdimen\fP]
[\-offsets \fIdimen\fP] [\-xoffset \fIdimen\fP] [\-yoffset \fIdimen\fP]
[\-keep] [\-rv] [\-fg \fIcolor\fP] [\-bg \fIcolor\fP]
[\-hl \fIcolor\fP] [\-bd \fIcolor\fP] [\-cr \fIcolor\fP] [\-bw \fIwidth\fP]
#ifx11
[\-geometry \fIgeometry\fP] [\-icongeometry \fIgeometry\fP] [\-iconic]
[\-display \fIdisplay\fP] [\-copy] [\-thorough]
#endif
#ifgrey
[\-nogrey] [\-gamma \fIgamma\fP]
#endif
#ifbuttons
[\-expert]
#endif
[\-version]
#ifx10
[\-geometry \fIgeometry\fP | =\fIgeometry\fP]
[\-display \fIhost\fP:\fIdisplay\fP | \fIhost\fP:\fIdisplay\fP]
#endif
dvi_file
.hy
.SH DESCRIPTION
.I Xdvi
is a program which runs under the X window system. It is used to preview
DVI files, such as are produced by TeX.
.PP
This program has the capability of showing the file shrunken by various
(integer) factors, and also has a ``magnifying glass'' which allows one
to see a small part of the unshrunk image momentarily.
.PP
Before displaying any page or part thereof, it checks to see if the dvi
file has changed since the last time it was displayed.  If this is the case,
then \fIxdvi\fR will reinitialize itself for the new dvi file.  For this reason,
exposing parts of the \fIxdvi\fR window while \fITeX\fR is running should be
avoided.  This feature allows you to preview many versions
of the same file while running \fIxdvi\fR only once.
#ifbuttons
.PP
In addition to using keystrokes to move within the file, \fIxdvi\fR provides
buttons on the right side of the window, which are synonymous with various
sequences of keystrokes.
#endif
.SH OPTIONS
In addition to specifying the .\fPdvi\fR file (with or without the .\fPdvi\fR),
\fIxdvi\fR supports the following command line options.
If the option begins with a
.RB ` + '
instead of a
.RB ` \- ',
#ifx11
the option is restored to its default value.  By default, these options can
be set via the resource names given in parentheses in the description of
each option.
#ifx10
the option is restored to its default value.  These options override those
set in the ``.Xdefaults'' file (via the resource names given in parentheses
in the description of each option).
#endif
.TP
.IB + page
Specifies the first page to show.  If \fI+\fR is given without a
number, the last page is assumed; the first page is the default.
.TP
.BI \-s " shrink"
(%%dot%%shrinkFactor)
Defines the initial shrink factor.  The default value is 3.
.TP
.BI \-S " density"
(%%dot%%densityPercent)
Determines the density used when shrinking bitmaps for fonts.
A higher value produces a lighter font.  The default value is 40.
.TP
.BI \-density " density"
Same as \fB-S\fR.
.TP
.BI \-p " pixels"
(%%dot%%pixelsPerInch)
Defines the size of the fonts to use, in pixels per inch.  The
default value is %%bdpi%%.
.TP
.BI \-altfont " font"
(%%dot%%altFont)
Declares a default font to use when the font in the dvi file cannot be found.
This is useful, for example, with PostScript <tm> fonts.
.TP
.B \-l
(%%dot%%listFonts)
Causes the names of the fonts used to be listed.
.TP
.B \-hushspecials
(%%dot%%hushSpecials)
Causes \fIxdvi\fR to suppress warnings about \\special strings
which it cannot process.
.TP
.B \-hushchars
(%%dot%%hushLostChars)
Causes \fIxdvi\fR to suppress warnings about references to characters which
are not defined in the font.
.TP
.B \-hush
(%%dot%%Hush)
Causes \fIxdvi\fR to suppress all suppressable warnings.
.TP
.B \-rv
(%%dot%%reverseVideo)
Causes the page to be displayed with white characters on a
black background, instead of vice versa.
.TP
.BI \-bw " width"
(%%dot%%borderWidth)
Specifies the width of the border of the window.
.TP
.BI \-borderwidth " width"
Same as \fB-bw\fR.
.TP
.BI \-fg " color"
(%%dot%%foreground)
Determines the color of the text (foreground).
.TP
.BI \-foreground " color"
Same as \fB-fg\fR.
.TP
.BI \-bg " color"
(%%dot%%background)
Determines the color of the background.
.TP
.BI \-background " color"
Same as \fB-bg\fR.
.TP
.BI \-hl " color"
(%%dot%%highlight)
Determines the color of the page border.  The default is the foreground color.
.TP
.BI \-bd " color"
(%%dot%%borderColor)
Determines the color of the window border.
.TP
.BI \-bordercolor " color"
Same as \fB-bd\fR.
.TP
.BI \-cr " color"
(%%dot%%cursorColor)
Determines the color of the cursor.  The default is the color of the page
border.
#ifx11
.TP
.B \-thorough
(.thorough)
\fIXdvi\fR will usually try to ensure that overstrike characters (\fIe.g.\fR
\\notin) are printed correctly.  On monochrome displays, this is always
possible with one logical operation, either \fIand\fR or \fIor\fR.  On
color displays, however, this may take two operations, one to set the
appropriate bits and one to clear other bits.  If this is the case, then
by default \fIxdvi\fR will instead use the \fIcopy\fR operation,
which does not handle overstriking correctly.  The ``thorough'' option
chooses the slower but more correct choice.  See also \fI-copy\fR, below.
.TP
.B \-copy
(.copy)
Always use the \fIcopy\fR operation when writing characters to the display.
This option may be necessary for correct operation on a color display, but
overstrike characters will be incorrect.
#ifgrey
If greyscale anti-aliasing is in use, the \fIcopy\fR operation will disable
the use of colorplanes and make overstrikes come out incorrectly.
#endif
#endif
.TP
.B \-keep
(%%dot%%keepPosition)
Sets a flag to indicate that \fIxdvi\fR should not move to the home position
when moving to a new page.  See also the `k' keystroke.
#ifbuttons
.TP
.B \-expert
(.expert)
Prevent the buttons from appearing.  See also the `x' keystroke.
#endif
.TP
.BI \-version
Print information on the version of \fIxdvi\fR.
.TP
.BI \-margins " dimen"
(%%dot%%Margin)
Specifies the size of both the top margin and side margin.
This should be a decimal number optionally followed by "cm", e.g., 1.5 or 3cm,
giving a measurement in inches or centimeters.  It determines
the ``home'' position of the page within the window as follows.  If the entire
page fits in the window, then the margin settings are ignored.  If, even
after removing the margins from the left, right, top, and bottom, the page
still cannot fit in the window, then the page is put in the window such that
the top and left margins are hidden, and presumably the upper left-hand corner
of the text on the page will be in the upper left-hand corner of the window.
Otherwise, the text is centered in the window.  See also `\fBM\fR' under
the KEY\%STROKES section.
.TP
.BI \-sidemargin " dimen"
(%%dot%%sideMargin)
Specifies the side margin (see above).
.TP
.BI \-topmargin " dimen"
(%%dot%%topMargin)
Specifies the top and bottom margins (see above).
.TP
.BI \-offsets " dimen"
(%%dot%%Offset)
Specifies the size of both the horizontal and vertical offsets of the
output on the page.  This should be a decimal number optionally followed by
"cm", e.g., 1.5 or 3cm, giving a measurement in inches or centimeters.
By decree
of the Stanford TeX Project, the default TeX page origin is always 1 inch
over and down from the top-left page corner, even when non-American paper
sizes are used.  Therefore, the default offsets are 1.0 inch.
.TP
.BI \-xoffset " dimen"
(%%dot%%xOffset)
Specifies the size of the horizontal offset of the output on the page
(see above).
.TP
.BI \-yoffset " dimen"
(%%dot%%yOffset)
Specifies the size of the vertical offset of the output on the page
(see above).
.TP
.BI \-paper " papertype"
(%%dot%%paper)
Specifies the size of the printed page.  This may be of the form
\fIw\fRx\fIh\fR (or \fIw\fRx\fIh\fRcm), where \fIw\fR is the width in
inches (or cm) and \fIh\fR is the height in inches (or cm), respectively.
There are also synonyms which may be used:  us (8.5x11), usr (11x8.5),
legal (8.5x14), foolscap (13.5x17), as well as the ISO sizes a1-a7,
b1-b7, c1-c7, a1r-a7r (a1-a7 rotated), etc.  The default size is
%%defaultpagesize%%.
#ifgrey
.TP
.B \-nogrey
(.grey)
Turns off the use of greyscale anti-aliasing when printing shrunken bitmaps.
(In this case, the logic of the corresponding resource is the reverse:
-nogrey corresponds to grey:off; +nogrey to grey:on.)
See also the `G' keystroke.
.TP
.BI \-gamma " gamma"
(.gamma)
Controls the interpolation of colors in the greyscale anti-aliasing color
palette.  Default value is 1.0.  For 0 < \fIgamma\fR < 1, the fonts will
be lighter (more like the background), and for \fIgamma\fR > 1, the fonts
will be darker (more like the foreground).  Negative values behave the
same way, but use a slightly different algorithm.
#endif
.TP
#ifx11
.BI "\-mgs[n]" " size"
(.magnifierSize[n])
Specifies the size of the window to be used for the ``magnifying glass''
for Button \fIn\fR.  See the MOUSE ACTIONS section.  Defaults are 200,
350, 600, 900, and 1200.
.TP
.BI \-mgs " size"
Same as \fB-mgs1\fR.
#ifx10
.BI \-mgs " size"
(magnifierSize1)
Specifies the size of the window to be used for the ``magnifying glass''
for the left button.  See the MOUSE ACTIONS section.  Default is 200.
.TP
.BI \-mgs1 " size"
Same as \fB-mgs\fR.
.TP
.BI \-mgs2 " size"
(magnifierSize2)
Specifies the ``magnifying glass'' size for the middle
button.  Default is 350.
.TP
.BI \-mgs3 " size"
(magnifierSize3)
Specifies the ``magnifying glass'' size for the right
button.  Default is 600.
#endif
.TP
#ifx11
.BI \-geometry " geometry"
(*geometry)
Specifies the initial geometry of the window.
.TP
.BI \-icongeometry " geometry
(%%dot%%iconGeometry)
Specifies the initial position for the icon.
.TP
.B \-iconic
(%%dot%%iconic)
Causes the XDVI window to start in the iconic state.  The default is to
start with the window open.
#ifx10
.BI = geometry
(geometry)
Specifies the initial geometry of the window.
.TP
.BI \-geometry " geometry"
Same as above.
#endif
.TP
.BI \-display " host:display"
Specifies the host and screen to be used for displaying the dvi file.
This is normally obtained from the environment variable ``DISPLAY.''
.SH KEYSTROKES
\fBXdvi\fR recognizes the following keystrokes when typed in its window.
Each may optionally be preceded by a (positive or negative) number, whose
interpretation will depend on the particular keystroke.
#ifx11
Also, the "Home", "Prior", "Next", and arrow cursor keys are synonyms for
`^', `b', `f', `l', `r', `u', and `d' keys, respectively.
#endif
.TP
.B q
Quits the program.  Control-C and control-D will do this, too.
.TP
.B n
Moves to the next page (or to the \fBn\fRth next page if a number is given).
Synonyms are `\fBf\fR', Space, Return, and Line Feed.
.TP
.B p
Moves to the previous page (or back \fBn\fR pages).  Synonyms are
`\fBb\fR', control-H, and Delete.
.TP
.B g
Moves to the page with the given number.  Initially, the first page is assumed
to be page number 1, but this can be changed with the `\fBP\fR' keystroke,
below.  If no page number is given, then it goes to the last page.
.TP
.B P
``This is page number \fBn\fR.''  This can be used to make the `\fBg\fR'
keystroke refer to actual page numbers instead of absolute page numbers.
.TP
.B Control-L
Redisplays the current page.
.TP
.B ^
Move to the ``home'' position of the page.  This is normally the upper
left-hand corner of the page, depending on the margins as described in
the \-\fBmargins\fR option, above.
.TP
.B u
Moves up two thirds of a window-full.
.TP
.B d
Moves down two thirds of a window-full.
.TP
.B l
Moves left two thirds of a window-full.
.TP
.B r
Moves right two thirds of a window-full.
.TP
.B c
Moves the page so that the point currently beneath the cursor is moved to
the middle of the window.  It also (gasp!) warps the cursor to the same place.
.TP
.B M
Sets the margins so that the point currently under the cursor is the upper
left-hand corner of the text in the page.  Note that this command itself does
not move the image at all.  For details on how the margins are used, see
the \-\fBmargins\fR option.
.TP
.B s
Changes the shrink factor to the given number.  If no number is given, the
smallest factor that makes the entire page fit in the window will be used.
(Margins are ignored in this computation.)
.TP
.B S
Sets the density factor to be used when shrinking bitmaps.  This should
be a number between 0 and 100; higher numbers produce lighter characters.
.TP
.B R
Forces the dvi file to be reread.  This allows you to preview many versions
of the same file while running \fIxdvi\fR only once.
.TP
.B k
Normally when \fIxdvi\fR switches pages, it moves to the home position as well.
The `k' keystroke toggles a `keep-position' flag which, when set, will keep
the same position when moving between pages.  Also `0k' and `1k' clear and
set this flag, respectively.  See also the \fB\-keep\fR option.
#ifbuttons
.TP
.B x
Toggles expert mode (in which the buttons do not appear).  Also `0x' and `1x'
clear and reset this mode, respectively.  See also the \fB\-expert\fR option.
#endif
#ifgrey
.TP
.B G
This key toggles the use of greyscale anti-aliasing for displaying shrunken
bitmaps.  In addition, the key sequences `0G' and `1G' clear and
set this flag, respectively.  See also the \fB\-nogrey\fR option.
#endif
.SH MOUSE ACTIONS
If the shrink factor is set to any number other than one, then clicking
any mouse button will pop up a ``magnifying glass'' which shows the unshrunk
image in the vicinity of the mouse click.  This subwindow disappears when
the mouse button is released.  Different mouse buttons produce different sized
windows, as indicated by the \fB\-mgs\fR option.  Moving the cursor
while holding the button down will move the magnifying glass.
.PP
Also, the scrollbars (if present) behave in the standard way:  pushing Button 2
in a scrollbar moves the top or left edge of the scrollbar to that point
and optionally drags it;
pushing Button 1 moves the image up or right by an amount equal to the distance
from the button press to the upper left-hand corner of the window; pushing
Button 3 moves the image down or left by the same amount.
.SH ENVIRONMENT
Uses the environment variable ``DISPLAY'' to specify which bit map display
terminal to use.
.PP
The environment variable ``XDVIFONTS'' determines the path(s) searched for
fonts in the following manner.  The string consists of one or more strings
separated by colons.  In each such string, the substring ``%f'' is
changed to the font name; ``%d'' is changed to the magnification; and
``%p'' is changed to the font file format (``gf'', ``pk'', or ``pxl'').
If no ``%f'' appears in the string, then the string ``/%f.%d%p'' is added on
the end.  For example, if the string is ``/usr/local/tex/fonts'' and the font is
cmr10 at 300dpi, then it searches for /usr/local/tex/fonts/cmr10.300gf,
/usr/local/tex/fonts/cmr10.300pk, and /usr/local/tex/fonts/cmr10.1500pxl,
in that order.  An extra colon anywhere in the ``XDVIFONTS'' variable
causes the system default paths to be tried at that point.  If the font is not
found in the desired size, then \fIxdvi\fR will
#ifmakepk
invoke Metafont to create the font in the correct size.  Failing that, it will
#endif
try to find the nearest size.
If the font cannot be found at all, then \fIxdvi\fR will try to vary the point
size of the font (within a certain range), and if this fails, then it will
use the font specified as the alternate font (cf. \fB-altfont\fR).
#iftexfonts
.PP
For compatibility with TeX, you may also use ``TEXFONTS'' in place of
``XDVIFONTS'', although in that case the variable should not include
any ``%'' specifiers.  The reason for recognizing TEXFONTS is that
certain versions of TeX also support the convention regarding an extra
colon in the font path; therefore, users who create their own fonts can
put both their .tfm and raster files in the same directory and do
``setenv TEXFONTS :MFdir'' or ``setenv TEXFONTS MFdir:'' in order to
get both TeX and \fIxdvi\fR to search their directory in addition to
the system standard directories.  The XDVIFONTS variable overrides the
TEXFONTS variable, so that on those sites where TEXFONTS must be set
explicitly, and therefore this feature is not useful, the XDVIFONTS may
be set to an empty string (\fIi.e.,\fR ``setenv XDVIFONTS'') to cause
\fIxdvi\fR to ignore TEXFONTS.
.PP
\fIxdvi\fR also recognizes the PKFONTS variable, which is checked after
XDVIFONTS but before TEXFONTS.
#endif
#ifmakepk
.PP
The script used to create fonts may be controlled by the environment
variable ``XDVIMAKEPK.''  Usually this variable would be set to the name of
the script.  In that case the script is called with the following options:
(1) the font name, (2) the requested resolution in dots per inch,
(3) the base resolution in dots per inch, and (4) a (possibly more accurate)
indication of the magnification using magsteps (if possible).
Optionally, the variable may include specifiers ``%n,'' ``%d,'' ``%b,''
and ``%m'' to indicate each of the above arguments, respectively.
This is compatible with the font creation mechanism used in dvips.
By default, XDVIMAKEPK equals %%mkpk%%.
#endif
#ifsubdir
.PP
You can also enable recursive searching in the font path by using the ``*''
and ``**'' specifiers.  At this point in the path, \fIxdvi\fR will recursively
search subdirectories of the given directory in order to find font files.
A single asterisk limits the search to one level; a double asterisk will search
through an arbitrary number of levels.  Also, the variable TEXFONTS_SUBDIR
can be used to give a colon-separated list of directories to recursively search.
This is equivalent to including the specifiers with a ``*'' after each; the
usual conventions regarding extra colons applies here, too, relative to a
default subdirectory path.  Asterisks may not be preceded by a ``%'' specifier
in any path component.
#endif
.PP
The ``XDVISIZES'' variable may be set to indicate which sizes of fonts are
available.  It should consist of a list of numbers separated by colons.  If
the list begins with a colon, the system default sizes are used, as well.
Sizes are expressed in dots per inch; decimals may be used for ``pxl'' files:
for example, a 300 dots per inch file magnified by half a step comes out to
1643 dots per five inches, which should be encoded as 328.6.  The current
default set of sizes is %%DEFAULT_FONT_SIZES%%.  \fIxdvi\fR will also try the
actual size of the font before trying any of the given sizes.
.PP
Virtual fonts are also supported, although \fIxdvi\fR does not have any
built-in fonts to which they can refer.  The search path for .vf files
can be specified with the ``XDVIVFS'' environment variable in a similar
manner to that for the ``XDVIFONTS'' variable.
#iftexfonts
\fIxdvi\fR will also check the VFFONTS variable if the XDVIFONTS variable
is not set.
#endif
Virtual fonts are searched for immediately after looking for the font
as a normal font in the exact size specified.
.SH FILES
.PD 0
.TP 40
%%DEFAULT_FONT_PATH%%   Font pixel files.
%%DEFAULT_VF_PATH%%   Virtual font files.
.PD
.SH "SEE ALSO"
.BR X (1).
.SH AUTHORS
Eric Cooper, CMU, did a version for direct output to a QVSS.
Modified for X by
Bob Scheifler, MIT Laboratory for Computer Science.
Modified for X11 by Mark Eichin, MIT SIPB.
Additional enhancements by many others.
