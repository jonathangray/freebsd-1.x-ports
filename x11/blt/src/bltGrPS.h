/*
 * bltGrPS.h --
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

static char postScriptDefinitions[] =
{
    "\n\n\
200 dict begin\n\n\
/BgColorProc 0 def	% Background color procedure (for symbols)\n\
/BorderProc 0 def	% Border outline procedure (for symbols)\n\
/StippleProc 0 def	% Stipple procedure (for bar segments)\n\
/DashesProc 0 def	% Dashes procedure (for line segments)\n\n\
\n\
/encoding {ISOLatin1Encoding} def\n\
systemdict /encodefont known {\n\
    /realsetfont /setfont load def\n\
    /setfont { encoding encodefont realsetfont } def\n\
} if\n\n\
/Stroke { gsave stroke grestore } def\n\n\
/Fill { gsave fill grestore } def\n\n\
/SetFont { 	% Stack: pointSize fontName\n\
    findfont exch scalefont setfont\n\
} def\n\n\
/SetDashes {		% Stack: numDashes\n\
    dup 0 eq { pop [] 0 setdash } { 1 array astore 0 setdash } ifelse\n\
} def\n\n\
/Box {			% Stack: x y width height\n\
    newpath\n\
        exch 4 2 roll moveto\n\
        dup 0 rlineto\n\
        exch 0 exch rlineto\n\
        neg 0 rlineto\n\
    closepath\n\
} def\n\n\
/SetFgColor {		% Stack: red green blue\n\
    CL 0 eq { pop pop pop 0 0 0 } if\n\
    setrgbcolor\n\
    CL 1 eq { currentgray setgray } if\n\
} def\n\n\
/SetBgColor {		% Stack: red green blue\n\
    CL 0 eq { pop pop pop 1 1 1 } if\n\
    setrgbcolor\n\
    CL 1 eq { currentgray setgray } if\n\
} def\n\n\
% The next two definitions are taken from \"$tk_library/prolog.ps\"\n\n\
% desiredSize EvenPixels closestSize\n\
%\n\
% The procedure below is used for stippling.  Given the optimal size\n\
% of a dot in a stipple pattern in the current user coordinate system,\n\
% compute the closest size that is an exact multiple of the device's\n\
% pixel size.  This allows stipple patterns to be displayed without\n\
% aliasing effects.\n\n\
/EvenPixels {\n\
    % Compute exact number of device pixels per stipple dot.\n\
    dup 0 matrix currentmatrix dtransform\n\
    dup mul exch dup mul add sqrt\n\n\
    % Round to an integer, make sure the number is at least 1, and compute\n\
    % user coord distance corresponding to this.\n\
    dup round dup 1 lt {pop 1} if\n\
    exch div mul\n\
} bind def\n\n\
% width height string filled StippleFill --\n\
%\n\
% Given a path and other graphics information already set up, this\n\
% procedure will fill the current path in a stippled fashion.  \"String\"\n\
% contains a proper image description of the stipple pattern and\n\
% \"width\" and \"height\" give its dimensions.  If \"filled\" is true then\n\
% it means that the area to be stippled is gotten by filling the\n\
% current path (e.g. the interior of a polygon); if it's false, the\n\
% area is gotten by stroking the current path (e.g. a wide line).\n\
% Each stipple dot is assumed to be about one unit across in the\n\
% current user coordinate system.\n\n\
/StippleFill {\n\
    % Turn the path into a clip region that we can then cover with\n\
    % lots of images corresponding to the stipple pattern.  Warning:\n\
    % some Postscript interpreters get errors during strokepath for\n\
    % dashed lines.  If this happens, turn off dashes and try again.\n\n\
    gsave\n\
    {eoclip}\n\
    {{strokepath} stopped {grestore gsave [] 0 setdash strokepath} if clip}\n\
    ifelse\n\n\
    % Change the scaling so that one user unit in user coordinates\n\
    % corresponds to the size of one stipple dot.\n\
    1 EvenPixels dup scale\n\n\
    % Compute the bounding box occupied by the path (which is now\n\
    % the clipping region), and round the lower coordinates down\n\
    % to the nearest starting point for the stipple pattern.\n\n\
    pathbbox\n\
    4 2 roll\n\
    5 index div cvi 5 index mul 4 1 roll\n\
    6 index div cvi 6 index mul 3 2 roll\n\n\
    % Stack now: width height string y1 y2 x1 x2\n\
    % Below is a doubly-nested for loop to iterate across this area\n\
    % in units of the stipple pattern size, going up columns then\n\
    % across rows, blasting out a stipple-pattern-sized rectangle at\n\
    % each position\n\n\
    6 index exch {\n\
	2 index 5 index 3 index {\n\
	    % Stack now: width height string y1 y2 x y\n\n\
	    gsave\n\
	    1 index exch translate\n\
	    5 index 5 index true matrix {3 index} imagemask\n\
	    grestore\n\
	} for\n\
	pop\n\
    } for\n\
    pop pop pop pop pop\n\
    grestore\n\
    newpath\n\
} bind def\n\n\
/DrawSegment {	% Stack: x1 y1 x2 y2\n\
    newpath 4 2 roll moveto lineto stroke\n\
} def\n\n\
/DrawText {		% Stack: ?bgColorProc? boolean centerX centerY\n\
			% 	 strWidth strHeight baseline theta str\n\
    gsave\n\
	7 -2 roll translate	% Translate to center of bounding box\n\
	exch neg rotate		% Rotate by theta\n\
	exch 4 2 roll\n\
	2 copy 2 copy 2 copy\n\n\
	% If needed, draw the background area, setting the bg color\n\n\
	-0.5 mul exch -0.5 mul exch 4 -2 roll Box\n\
        7 -1 roll { gsave 7 -1 roll exec fill grestore } if\n\n\
	% Move to the text string starting position\n\n\
	-.5  mul 5 -1 roll add exch -.5 mul exch moveto\n\
	pop exch dup dup 4 2 roll\n\n\
        % Adjust character widths to get desired overall string width\n\
        % adjust X = (desired width - real width) / #chars\n\n\
 	stringwidth pop sub exch length div 0 3 -1 roll\n\n\
	% Flip back the scale so that the string is not drawn in reverse\n\n\
	1 -1 scale\n\
	ashow\n\
    grestore\n\
} def\n\n\
/DrawBitmap {		% Stack: ?bgColorProc? boolean centerX centerY\n\
			%	 width height theta imageStr\n\
    gsave\n\
	6 -2 roll translate	% Translate to center of bounding box\n\
	4 1 roll neg rotate	% Rotate by theta\n\n\
	% Find upperleft corner of bounding box\n\n\
	2 copy -.5 mul exch -.5 mul exch translate\n\
	2 copy scale		% Make pixel unit scale\n\
        newpath\n\
            0 0 moveto 0 1 lineto 1 1 lineto 1 0 lineto\n\
        closepath\n\n\
	% Fill rectangle with background color\n\n\
	4 -1 roll { gsave 4 -1 roll exec fill grestore } if\n\n\
	% Paint the image string into the unit rectangle\n\n\
	2 copy true 3 -1 roll 0 0 5 -1 roll 0 0 6 array astore 5 -1 roll\n\
        imagemask\n\
    grestore\n\
}def\n\n\
% Symbols:\n\n\
% Skinny-cross\n\
/Sc {			% Stack: x y symbolSize\n\
    gsave\n\
	3 -2 roll translate 45 rotate\n\
	0 0 3 -1 roll Sp\n\
    grestore\n\
} def\n\n\
% Skinny-plus\n\
/Sp {			% Stack: x y symbolSize\n\
    gsave\n\
	3 -2 roll translate\n\
	2 idiv  	% Stack: radius\n\
	dup 2 copy	% Stack: radius radius radius radius\n\
	newpath neg 0 moveto 0 lineto\n\
	gsave BgColorProc fill grestore stroke\n\
	newpath neg 0 exch moveto 0 exch lineto\n\
	gsave BgColorProc fill grestore stroke\n\
    grestore\n\
} def\n\n\
% Cross\n\
/Cr {			% Stack: x y symbolSize\n\
    gsave\n\
	3 -2 roll translate 45 rotate\n\
	0 0 3 -1 roll Pl\n\
    grestore\n\
} def\n\n\
% Plus\n\
/Pl {			% Stack: x y symbolSize\n\
    gsave\n\
	3 -2 roll translate\n\
	dup 2 idiv  	% Stack: size radius\n\
	exch 6 idiv 	% Stack: radius delta\n\n\
	%\n\
	%          2   3	The plus/cross symbol is a\n\
	%			closed polygon of 12 points.\n\
	%      0   1   4    5	The diagram to the left\n\
	%           x,y		represents the positions of\n\
	%     11  10   7    6	the points which are computed\n\
	%			below.\n\
	%          9   8\n\
	%\n\n\
	newpath\n\
	    2 copy exch neg exch neg moveto dup neg dup lineto\n\
	    2 copy neg exch neg lineto 2 copy exch neg lineto\n\
	    dup dup neg lineto 2 copy neg lineto 2 copy lineto\n\
	    dup dup lineto 2 copy exch lineto 2 copy neg exch lineto\n\
	    dup dup neg exch lineto exch neg exch lineto\n\
	closepath\n\
	Fill BorderProc\n\
    grestore\n\
} def\n\n\
% Circle\n\
/Ci {			% Stack: x y symbolSize\n\
    3 copy pop\n\
    moveto newpath\n\
        2 div 0 360 arc\n\
    closepath Fill BorderProc\n\
} def\n\n\
% Square\n\
/Sq {			% Stack: x y symbolSize\n\
    dup dup 2 div dup\n\
    6 -1 roll exch sub exch\n\
    5 -1 roll exch sub 4 -2 roll Box\n\
    Fill BorderProc\n\
} def\n\n\
% Line\n\
/Li {			% Stack: x y symbolSize\n\
    3 1 roll exch 3 -1 roll 2 div 3 copy\n\
    newpath\n\
        sub exch moveto add exch lineto\n\
    stroke\n\
} def\n\n\
% Diamond\n\
/Di {			% Stack: x y symbolSize\n\
    gsave\n\
	3 1 roll translate 45 rotate 0 0 3 -1 roll Sq\n\
    grestore\n\
} def\n\n\n\
%%BeginSetup\n\
gsave			% Save the graphics state\n\n\
% Default line style parameters\n\n\
1 setlinewidth		% width\n\
1 setlinejoin		% join\n\
0 setlinecap		% cap\n\
0 SetDashes		% dashes\n\n\
% Adjust coordinate system to use X11 coordinates\n\n\
0 792 translate\n\
1 -1 scale\n\n\
% User defined page layout\n\n"
};
