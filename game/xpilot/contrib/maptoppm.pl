#!/usr/local/bin/perl -- # -*-Perl-*-
#
# From: greg@hansen.ncd.com (Greg Renda)
# Newsgroups: alt.games.xpilot
# Date: 25 Sep 1993 02:02:57 GMT
# Organization: Network Computing Devices Inc., Mt. View, CA, USA
# 
# Here's a perl script to convert map files to ppm format.  This lets
# you browse maps without having to run xpilot or the map editor.  Of
# course you'll need the pbm utilities as well.  I use it with the
# following simple script:
# 
# 	maptoppm < $1 | pnmenlarge 2 | pnmtoxwd | xwud
# 
# The colors are as follows:
# 
# 	space - black
# 	walls - gray
# 	fuel - red
# 	cannon - white
# 	bases - blue
# 
# 	left gravity - green
# 	right gravity - green
# 	positive gravity - cyan
# 	negative gravity - magenta
# 
# 	two-way wormhole - yellow
# 	in wormhole - orange
# 	out wormhole - pink
# 
# Enjoy!
# 
# -Greg
# 

$black='0 0 0';
$white='255 255 255';
$gray='128 128 128';

$red='255 0 0';
$green='0 255 0';
$blue='0 0 255';

$yellow='255 255 0';
$magenta='255 0 255';
$cyan='0 255 255';

$orange='255 165 0';
$pink='255 192 203';

$wall='xswqa';
$space=' ';
$fuel='#';
$cannon='rdfc';
$base='_*';
$gravLeft='<';
$gravRight='>';
$gravPlus='+';
$gravMinus='\-';
$gravity=$gravLeft.$gravRight.$gravPlus.$gravMinus;

$wormbi='@';
$wormin='(';
$wormout=')';
$wormhole=$wormbi.$wormin.$wormout;

$symbols=$wall.$fuel.$cannon.$gravity.$wormhole.$base;

while (<>)
{
    ($lower = $_) =~ tr/[A-Z]/[a-z]/;

    if ($lower =~ /^mapwidth/)
    {
	s/\s//g;
	($dummy, $width) = split(/:/);
	next;
    }

    if ($lower =~ /^mapheight/)
    {
	s/\s//g;
	($dummy, $height) = split(/:/);
	next;
    }

    if (!$data)
    {
	if ($data = $lower =~ /^mapdata/)
	{
	    s/\s//g;
	    ($dummy, $dummy, $end) = split(/:/);
	    print "P3\n$width $height\n255\n"
	}
	next;
    }

    last if /^$end/;

    chop;

    s/[^$symbols]/ /g;		# get rid characters we don't care about

    s/[$space]/$black /g;
    s/[$wall]/$gray /g;
    s/[$fuel]/$red /g;
    s/[$cannon]/$white /g;
    s/[$base]/$blue /g;

    s/[$gravLeft]/$green /g;
    s/[$gravRight]/$green /g;
    s/[$gravPlus]/$cyan /g;
    s/[$gravMinus]/$magenta /g;

    s/[$wormbi]/$yellow /g;
    s/[$wormin]/$orange /g;
    s/[$wormout]/$pink /g;

    print "$_\n";
}
