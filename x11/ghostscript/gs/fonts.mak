#    Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Makefile for Ghostscript fonts.
# For more information about Ghostscript fonts, consult the Fontmap file.

AFM=fonts/afm
BDF=fonts/bdf
FONTS=fonts/gsf
PFA=fonts/pfa

GSBDF=gs386

fonts: AvantGarde Bookman CharterBT Courier Cyrillic Helvetica \
	NewCenturySchlbk Palatino Symbol TimesRoman \
	Ugly Utopia ZapfChancery ZapfDingbats

# ----------------------------------------------------------------

# Each Ghostscript font has a uniqueID (an integer).  This is used
# to identify distinct fonts within the Ghostscript font machinery.
# since some PostScript programs assume that adding a small integer
# to a uniqueID produces a new, distinct, unused uniqueID,
# the uniqueID values in this file are all multiples of 10.
# To avoid some conflicts with Adobe's numbering scheme, the uniqueID
# values in this file for Ghostscript's own fonts all lie between
# 4000000 and 4999999.  (The uniqueID is used only when converting the font.)
# The algorithm for computing the UniqueID is given below.

# A UniqueID for a Ghostscript font looks like:
#
# 4TTWVE0
#
# where TT is a two-digit number representing the typeface,
# W represents the weight (normal, bold, ...),
# V represents the variant (normal, italic, oblique, ...), and
# E represents the expansion (normal, condensed, ...).
# This scheme will not work forever.  As soon there are more 99
# typefaces, or more than 9 weights or variants, we will have to do
# something else. But it suffices for the near future.
#
# The filename for a font is constructed in a somewhat similar way:
#
# FTTWVVVE.gsf
#
# where F is the foundry, TT a two-letter abbreviation for the
# typeface, and W, V, and E the weight, variant, and expansion.  Since a
# font can have multiple variants, we allocate three letters to that
# (for example, Lucida Regular Sans Typewriter Italic).  If a font has
# four variants, you're on your own.  If a font does have multiple
# variants, it's best to add the expansion letter `r', so that it is
# clear which letters are variants and which the expansion.
#
# This scheme is very close to the one proposed in `Filenames for
# fonts', published in the first 1990 issue of TUGboat (the
# journal of the TeX Users Group).
#
# In the following tables, we made no attempt to be exhaustive.
# Instead, we have simply allocated entries for those things that we needed
# for the fonts that we are actually distributing.
#
#
# foundries:
# ----------------------------------------------------------------
# b = Bitstream
# n = IBM
# p = Adobe (`p' for PostScript)
#
#
#
# typefaces:
# id   name			  filename prefix
# ----------------------------------------------------------------
# 08 = Avant Garde		= pag		(Adobe)
# 11 = Bookman			= pbk		(Adobe)
# 01 = CharterBT		= bch		(Bitstream)
# 02 = Courier			= ncr		(IBM)
# 03 = Helvetica		= phv		(Adobe)
# 04 = New Century Schoolbook	= pnc		(Adobe)
# 09 = Palatino			= ppl		(Adobe)
# 05 = Symbol			= psy		(Adobe)
# 06 = Times			= ptm		(Adobe)
# 00 = Ugly			= ugly		(public domain)
# --   Utopia			= put		(Adobe)
# 07 = Zapf Chancery		= zc		(public domain)
# 10 = Zapf Dingbats		= pzd		(Adobe)
#
# 90 = Hershey Gothic English	= hrge
# 91 = Hershey Gothic Italian	= hrit
# 92 = Hershey Gothic German	= hrgr
# 93 = Hershey Greek		= hrgk
# 94 = Hershey Plain		= hrpl
# 95 = Hershey Script		= hrsc
# 96 = Hershey Symbol		= hrsy
#
#
# weights:
# 0 = normal			= r
# 1 = bold			= b
# 2 = book			= k
# 3 = demi			= d
# 4 = light			= l
#
#
# variants:
# 0 = normal			= r (omitted when the weight is normal)
# 1 = italic			= i
# 2 = oblique			= o
#
#
# expansions:
# 0 = normal			= r (omitted when the weight and variant
#                                    are normal)
# 1 = narrow			= n
#
#

# ---------------------------------------------------------------- #
#                                                                  #
#            Converting BDF fonts to .gsf (Type 1) fonts           #
#                                                                  #
# ---------------------------------------------------------------- #

# The bdftops conversion program takes the following arguments:
#
#	 bdftops xx.bdf [yy1.afm ...] zz.gsf fontname uniqueID [encodingname]
#
# These arguments have the following meanings:
#
#	xx.bdf - the input bitmap file, a BDF file
#	yy*.afm - the AFM files giving the metrics (optional)
#	zz.gsf - the output file
#	fontname - the font name
#	uniqueID - the uniqueID, as described above
#	encodingname - the encoding for the font
#
# Currently, the only defined encodings are StandardEncoding,
# ISOLatin1Encoding, and SymbolEncoding. If the encoding is omitted,
# StandardEncoding is assumed.

# ----------------------------------------------------------------

FBEGIN=echogs -w _temp_ -n -x 5b28	# open bracket, open parenthesis
P2=-x 2928				# close and open parenthesis
FNEXT=echogs -a _temp_ -n
FLAST=echogs -a _temp_
FEND=-x 295d /ARGUMENTS exch def	# close parenthesis, close bracket
RUNBDF=$(GSBDF) -dNODISPLAY _temp_ bdftops.ps quit.ps

# ---------------- Avant Garde ----------------

AvantGarde: $(FONTS)/pagk.gsf $(FONTS)/pagko.gsf $(FONTS)/pagd.gsf \
	$(FONTS)/pagdo.gsf

$(FONTS)/pagk.gsf: $(BDF)/avt18.bdf $(AFM)/av-k.afm
	$(FBEGIN) $(BDF)/avt18.bdf $(P2) $(AFM)/av-k.afm $(P2)
	$(FNEXT) $(FONTS)/pagk.gsf $(P2)
	$(FLAST) AvantGarde-Book $(P2) 4082000 $(FEND)
	$(RUNBDF)

$(FONTS)/pagko.gsf: $(BDF)/pagko.bdf $(AFM)/av-ko.afm
	$(FBEGIN) $(BDF)/pagko.bdf $(P2) $(AFM)/av-ko.afm $(P2)
	$(FNEXT) $(FONTS)/pagko.gsf $(P2)
	$(FLAST) AvantGarde-BookOblique $(P2) 4082200 $(FEND)
	$(RUNBDF)

$(FONTS)/pagd.gsf: $(BDF)/pagd.bdf $(AFM)/av-d.afm
	$(FBEGIN) $(BDF)/pagd.bdf $(P2) $(AFM)/av-d.afm $(P2)
	$(FNEXT) $(FONTS)/pagd.gsf $(P2)
	$(FLAST) AvantGarde-Demi $(P2) 4083000 $(FEND)
	$(RUNBDF)

$(FONTS)/pagdo.gsf: $(BDF)/pagdo.bdf $(AFM)/av-do.afm
	$(FBEGIN) $(BDF)/pagdo.bdf $(P2) $(AFM)/av-do.afm $(P2)
	$(FNEXT) $(FONTS)/pagdo.gsf $(P2)
	$(FLAST) AvantGarde-DemiOblique $(P2) 4083200 $(FEND)
	$(RUNBDF)

# ---------------- Bookman ----------------

Bookman: $(FONTS)/pbkl.gsf $(FONTS)/pbkli.gsf $(FONTS)/pbkd.gsf \
	$(FONTS)/pbkdi.gsf

$(FONTS)/pbkl.gsf: $(BDF)/pbkl.bdf $(AFM)/book-l.afm
	$(FBEGIN) $(BDF)/pbkl.bdf $(P2) $(AFM)/book-l.afm $(P2)
	$(FNEXT) $(FONTS)/pbkl.gsf $(P2)
	$(FLAST) Bookman-Light $(P2) 4114000 $(FEND)
	$(RUNBDF)

$(FONTS)/pbkli.gsf: $(BDF)/pbkli.bdf $(AFM)/book-li.afm
	$(FBEGIN) $(BDF)/pbkli.bdf $(P2) $(AFM)/book-li.afm $(P2)
	$(FNEXT) $(FONTS)/pbkli.gsf $(P2)
	$(FLAST) Bookman-LightItalic $(P2) 4114100 $(FEND)
	$(RUNBDF)

$(FONTS)/pbkd.gsf: $(BDF)/pbkd.bdf $(AFM)/book-d.afm
	$(FBEGIN) $(BDF)/pbkd.bdf $(P2) $(AFM)/book-d.afm $(P2)
	$(FNEXT) $(FONTS)/pbkd.gsf $(P2)
	$(FLAST) Bookman-Demi $(P2) 4113000 $(FEND)
	$(RUNBDF)

$(FONTS)/pbkdi.gsf: $(BDF)/pbkdi.bdf $(AFM)/book-di.afm
	$(FBEGIN) $(BDF)/pbkdi.bdf $(P2) $(AFM)/book-di.afm $(P2)
	$(FNEXT) $(FONTS)/pbkdi.gsf $(P2)
	$(FLAST) Bookman-DemiItalic $(P2) 4113100 $(FEND)
	$(RUNBDF)

# ---------------- Charter ----------------

# These are the fonts contributed by Bitstream to X11R5.

CharterBT: $(FONTS)/bchr.gsf $(FONTS)/bchri.gsf $(FONTS)/bchb.gsf \
	$(FONTS)/bchbi.gsf

# Old Charter, no longer used.
#$(FONTS)/bchr.gsf: $(BDF)/charR24.bdf	# 4010000

# Old Charter-Italic, no longer used.
#$(FONTS)/bchri.gsf: $(BDF)/charI24.bdf	# 4010100

# Old Charter-Bold, no longer used.
#$(FONTS)/bchb.gsf: $(BDF)/charB24.bdf	# 4011000
		
# Old Charter-BoldItalic, no longer used.
#$(FONTS)/bchbi.gsf: $(BDF)/charBI24.bdf	# 4011100

# ---------------- Courier ----------------

# Ghostscript has two sets of Courier fonts, a lower-quality set derived
# from the X11R4 bitmaps, and a higher-quality set contributed by IBM
# to X11R5 in Type 1 form.  The former are no longer used.

Courier: $(FONTS)/ncrr.gsf $(FONTS)/ncrri.gsf $(FONTS)/ncrb.gsf \
	$(FONTS)/ncrbi.gsf

# Old Courier, longer used.
#$(FONTS)/pcrr.gsf: $(BDF)/courR24.bdf $(AFM)/cour.afm	# 4020000

# Old Courier-Oblique, no longer used.
#$(FONTS)/pcrro.gsf: $(BDF)/courO24.bdf $(AFM)/cour-o.afm	# 4020200

# Old Courier-Bold, no longer used.
#$(FONTS)/_pcrb.gsf: $(BDF)/courB24.bdf $(AFM)/cour-b.afm	# 4021000

# Old Courier-BoldOblique, no longer used.
#$(FONTS)/pcrbo.gsf: $(BDF)/courBO24.bdf $(AFM)/cour-bo.afm	# 4021200

# ---------------- Cyrillic ----------------

# These are shareware fonts of questionable quality.

Cyrillic: $(FONTS)/cyr.gsf $(FONTS)/cyri.gsf

# ---------------- Helvetica ----------------

Helvetica: $(FONTS)/phvr.gsf $(FONTS)/phvro.gsf $(FONTS)/phvrrn.gsf \
	 $(FONTS)/phvb.gsf $(FONTS)/phvbo.gsf

$(FONTS)/phvr.gsf: $(BDF)/helvR24.bdf $(AFM)/helv.afm
	$(FBEGIN) $(BDF)/helvR24.bdf $(P2) $(AFM)/helv.afm $(P2)
	$(FNEXT) $(FONTS)/phvr.gsf $(P2)
	$(FLAST) Helvetica $(P2) 4030000 $(FEND)
	$(RUNBDF)

$(FONTS)/phvro.gsf: $(BDF)/helvO24.bdf $(AFM)/helv-o.afm
	$(FBEGIN) $(BDF)/helvO24.bdf $(P2) $(AFM)/helv-o.afm $(P2)
	$(FNEXT) $(FONTS)/phvro.gsf $(P2)
	$(FLAST) Helvetica-Oblique $(P2) 4030200 $(FEND)
	$(RUNBDF)

$(FONTS)/phvrrn.gsf: $(BDF)/hvmrc14.bdf $(AFM)/helv-n.afm
	$(FBEGIN) $(BDF)/hvmrc14.bdf $(P2) $(AFM)/helv-n.afm $(P2)
	$(FNEXT) $(FONTS)/phvrrn.gsf $(P2)
	$(FLAST) Helvetica-Narrow $(P2) 4030310 $(FEND)
	$(RUNBDF)

$(FONTS)/phvb.gsf: $(BDF)/helvB24.bdf $(AFM)/helv-b.afm
	$(FBEGIN) $(BDF)/helvB24.bdf $(P2) $(AFM)/helv-b.afm $(P2)
	$(FNEXT) $(FONTS)/phvb.gsf $(P2)
	$(FLAST) Helvetica-Bold $(P2) 4031000 $(FEND)
	$(RUNBDF)

$(FONTS)/phvbo.gsf: $(BDF)/helvBO24.bdf $(AFM)/helv-bo.afm
	$(FBEGIN) $(BDF)/helvBO24.bdf $(P2) $(AFM)/helv-bo.afm $(P2)
	$(FNEXT) $(FONTS)/phvbo.gsf $(P2)
	$(FLAST) Helvetica-BoldOblique $(P2) 4031200 $(FEND)
	$(RUNBDF)

# ---------------- New Century Schoolbook ----------------

NewCenturySchlbk: $(FONTS)/pncr.gsf $(FONTS)/pncrri.gsf $(FONTS)/pncb.gsf \
	$(FONTS)/pncbi.gsf

$(FONTS)/pncr.gsf: $(BDF)/ncenR24.bdf $(AFM)/ncs-r.afm
	$(FBEGIN) $(BDF)/ncenR24.bdf $(P2) $(AFM)/ncs-r.afm $(P2)
	$(FNEXT) $(FONTS)/pncr.gsf $(P2)
	$(FLAST) NewCenturySchlbk-Roman $(P2) 4040000 $(FEND)
	$(RUNBDF)

$(FONTS)/pncrri.gsf: $(BDF)/ncenI24.bdf $(AFM)/ncs-i.afm
	$(FBEGIN) $(BDF)/ncenI24.bdf $(P2) $(AFM)/ncs-i.afm $(P2)
	$(FNEXT) $(FONTS)/pncrri.gsf $(P2)
	$(FLAST) NewCenturySchlbk-Italic $(P2) 4040100 $(FEND)
	$(RUNBDF)

$(FONTS)/pncb.gsf: $(BDF)/ncenB24.bdf $(AFM)/ncs-b.afm
	$(FBEGIN) $(BDF)/ncenB24.bdf $(P2) $(AFM)/ncs-b.afm $(P2)
	$(FNEXT) $(FONTS)/pncb.gsf $(P2)
	$(FLAST) NewCenturySchlbk-Bold $(P2) 4041000 $(FEND)
	$(RUNBDF)

$(FONTS)/pncbi.gsf: $(BDF)/ncenBI24.bdf $(AFM)/ncs-bi.afm
	$(FBEGIN) $(BDF)/ncenBI24.bdf $(P2) $(AFM)/ncs-bi.afm $(P2)
	$(FNEXT) $(FONTS)/pncbi.gsf $(P2)
	$(FLAST) NewCenturySchlbk-BoldItalic $(P2) 4041100 $(FEND)
	$(RUNBDF)

# ---------------- Palatino ----------------

Palatino: $(FONTS)/pplr.gsf $(FONTS)/pplri.gsf $(FONTS)/pplb.gsf \
	$(FONTS)/pplbi.gsf

$(FONTS)/pplr.gsf: $(BDF)/pal18.bdf $(AFM)/pal-r.afm
	$(FBEGIN) $(BDF)/pal18.bdf $(P2) $(AFM)/pal-r.afm $(P2)
	$(FNEXT) $(FONTS)/pplr.gsf $(P2)
	$(FLAST) Palatino-Roman $(P2) 4090000 $(FEND)
	$(RUNBDF)

$(FONTS)/pplri.gsf: $(BDF)/pplri.bdf $(AFM)/pal-i.afm
	$(FBEGIN) $(BDF)/pplri.bdf $(P2) $(AFM)/pal-i.afm $(P2)
	$(FNEXT) $(FONTS)/pplri.gsf $(P2)
	$(FLAST) Palatino-Italic $(P2) 4090100 $(FEND)
	$(RUNBDF)

$(FONTS)/pplb.gsf: $(BDF)/pplb.bdf $(AFM)/pal-b.afm
	$(FBEGIN) $(BDF)/pplb.bdf $(P2) $(AFM)/pal-b.afm $(P2)
	$(FNEXT) $(FONTS)/pplb.gsf $(P2)
	$(FLAST) Palatino-Bold $(P2) 4091000 $(FEND)
	$(RUNBDF)

$(FONTS)/pplbi.gsf: $(BDF)/pplbi.bdf $(AFM)/pal-bi.afm
	$(FBEGIN) $(BDF)/pplbi.bdf $(P2) $(AFM)/pal-bi.afm $(P2)
	$(FNEXT) $(FONTS)/pplbi.gsf $(P2)
	$(FLAST) Palatino-BoldItalic $(P2) 4091100 $(FEND)
	$(RUNBDF)

# ---------------- Symbol ----------------

Symbol: $(FONTS)/psyr.gsf

$(FONTS)/psyr.gsf: $(BDF)/symb24.bdf $(AFM)/symbol.afm
	$(FBEGIN) $(BDF)/symb24.bdf $(P2) $(AFM)/symbol.afm $(P2)
	$(FNEXT) $(FONTS)/psyr.gsf $(P2)
	$(FLAST) Symbol 4050000 $(P2) SymbolEncoding $(FEND)
	$(RUNBDF)

# ---------------- Times Roman ----------------

TimesRoman: $(FONTS)/ptmr.gsf $(FONTS)/ptmri.gsf $(FONTS)/ptmb.gsf \
	$(FONTS)/ptmbi.gsf

$(FONTS)/ptmr.gsf: $(BDF)/timR24.bdf $(AFM)/times-r.afm
	$(FBEGIN) $(BDF)/timR24.bdf $(P2) $(AFM)/times-r.afm $(P2)
	$(FNEXT) $(FONTS)/ptmr.gsf $(P2)
	$(FLAST) Times-Roman $(P2) 4060000 $(FEND)
	$(RUNBDF)

$(FONTS)/ptmri.gsf: $(BDF)/timI24.bdf $(AFM)/times-i.afm
	$(FBEGIN) $(BDF)/timI24.bdf $(P2) $(AFM)/times-i.afm $(P2)
	$(FNEXT) $(FONTS)/ptmri.gsf $(P2)
	$(FLAST) Times-Italic $(P2) 4060100 $(FEND)
	$(RUNBDF)

$(FONTS)/ptmb.gsf: $(BDF)/timB24.bdf $(AFM)/times-b.afm
	$(FBEGIN) $(BDF)/timB24.bdf $(P2) $(AFM)/times-b.afm $(P2)
	$(FNEXT) $(FONTS)/ptmb.gsf $(P2)
	$(FLAST) Times-Bold $(P2) 4061000 $(FEND)
	$(RUNBDF)

$(FONTS)/ptmbi.gsf: $(BDF)/timBI24.bdf $(AFM)/times-bi.afm
	$(FBEGIN) $(BDF)/timBI24.bdf $(P2) $(AFM)/times-bi.afm $(P2)
	$(FNEXT) $(FONTS)/ptmbi.gsf $(P2)
	$(FLAST) Times-BoldItalic $(P2) 4061100 $(FEND)
	$(RUNBDF)

# ---------------- Ugly ----------------

# This font, and only this font, is stored in the main executable
# directory for Ghostscript, not the fonts directory.

Ugly: uglyr.gsf

uglyr.gsf: ugly10.bdf
	$(FBEGIN) ugly10.bdf $(P2) uglyr.gsf $(P2)
	$(FLAST) Ugly $(P2) 4000000 $(FEND)
	$(RUNBDF)

# ---------------- Utopia ----------------
# (Already in Type 1 form.)

Utopia: $(FONTS)/putr.gsf $(FONTS)/putri.gsf $(FONTS)/putb.gsf \
	$(FONTS)/putbi.gsf

# ---------------- Zapf Chancery ----------------

ZapfChancery: $(FONTS)/zcr.gsf $(FONTS)/zcro.gsf $(FONTS)/zcb.gsf

$(FONTS)/zcr.gsf: $(BDF)/zcr24.bdf $(AFM)/zc-r.afm
	$(FBEGIN) $(BDF)/zcr24.bdf $(P2) $(AFM)/zc-r.afm $(P2)
	$(FNEXT) $(FONTS)/zcr.gsf $(P2)
	$(FLAST) ZapfChancery $(P2) 4070000 $(FEND)
	$(RUNBDF)

$(FONTS)/zcro.gsf: $(BDF)/zcro24.bdf $(AFM)/zc-mi.afm
	$(FBEGIN) $(BDF)/zcro24.bdf $(P2) $(AFM)/zc-mi.afm $(P2)
	$(FNEXT) $(FONTS)/zcro.gsf $(P2)
	$(FLAST) ZapfChancery-Oblique $(P2) 4070200 $(FEND)
	$(RUNBDF)

$(FONTS)/zcb.gsf: $(BDF)/zcb30.bdf $(AFM)/zc-b.afm
	$(FBEGIN) $(BDF)/zcb30.bdf $(P2) $(AFM)/zc-b.afm $(P2)
	$(FNEXT) $(FONTS)/zcb.gsf $(P2)
	$(FLAST) ZapfChancery-Bold $(P2) 4071000 $(FEND)
	$(RUNBDF)

# ---------------- Zapf Dingbats ----------------

ZapfDingbats: $(FONTS)/pzdr.gsf

$(FONTS)/pzdr.gsf: $(BDF)/pzdr.bdf $(AFM)/zd.afm
	$(FBEGIN) $(BDF)/pzdr.bdf $(P2) $(AFM)/zd.afm $(P2)
	$(FNEXT) $(FONTS)/pzdr.gsf $(P2)
	$(FLAST) ZapfDingbats $(P2) 4100000 $(FEND)
	$(RUNBDF)
