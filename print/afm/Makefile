#	$Header: /a/cvs/386BSD/ports/print/afm/Makefile,v 1.3 1994/02/01 02:41:21 ljo Exp $
#
# FlexFAX Facsimile Software
#
# Copyright (c) 1990, 1991, 1992, 1993 Sam Leffler
# Copyright (c) 1991, 1992, 1993 Silicon Graphics, Inc.
# 
# Permission to use, copy, modify, distribute, and sell this software and 
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
# 
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
# 
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#

#
# This directory contains Adobe font metric files for the most
# common PostScript fonts.  They were obtained from the dvips
# distribution that is available on ftp.uu.net (use
#
#     site index dvipsafm
#
# to locate them).  Note the copyright on them.  Note that the
# lptops program expects them to reside in files without a ".afm"
# suffix.
#
AFMDIR=/usr/local/lib/afm
INSTALL=install

AFMFILES=\
	AvantGarde-Book.afm \
	AvantGarde-BookOblique.afm \
	AvantGarde-Demi.afm \
	AvantGarde-DemiOblique.afm \
	Bookman-Demi.afm \
	Bookman-DemiItalic.afm \
	Bookman-Light.afm \
	Bookman-LightItalic.afm \
	Courier-Bold.afm \
	Courier-BoldOblique.afm \
	Courier-Oblique.afm \
	Courier.afm \
	Helvetica-Bold.afm \
	Helvetica-BoldOblique.afm \
	Helvetica-Narrow-Bold.afm \
	Helvetica-Narrow-BoldOblique.afm \
	Helvetica-Narrow-Oblique.afm \
	Helvetica-Narrow.afm \
	Helvetica-Oblique.afm \
	Helvetica.afm \
	NewCenturySchlbk-Bold.afm \
	NewCenturySchlbk-BoldItalic.afm \
	NewCenturySchlbk-Italic.afm \
	NewCenturySchlbk-Roman.afm \
	Palatino-Bold.afm \
	Palatino-BoldItalic.afm \
	Palatino-Italic.afm \
	Palatino-Oblique.afm \
	Palatino-Roman.afm \
	Symbol.afm \
	Times-Bold.afm \
	Times-BoldItalic.afm \
	Times-Italic.afm \
	Times-Oblique.afm \
	Times-Roman.afm \
	ZapfChancery-MediumItalic.afm \
	ZapfDingbats.afm \
	${NULL}

all:
	@echo It's ready to be installed!

clean:
	@echo Nothing needs to be cleaned!

install: ${AFMFILES}
	[ -d ${AFMDIR} ] || mkdir ${AFMDIR}
	for i in ${AFMFILES}; do \
	    ${INSTALL} -c -o bin -g bin -m 444 $$i ${AFMDIR}/`basename $$i .afm`; \
	done

pkg:	install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST afm_bin

