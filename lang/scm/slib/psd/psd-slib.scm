;;;;
;;;; $Id: psd-slib.scm,v 1.1 1994/02/16 08:44:35 hsu Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 1, or (at your option)
;;;; any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; See file COPYING in the psd distribution.
;;;;
;;;;  $Log: psd-slib.scm,v $
;;;;  Revision 1.1  1994/02/16 08:44:35  hsu
;;;;  Initial revision
;;;;
;;;; Revision 1.1  1993/10/04  14:05:23  pk
;;;; Initial revision
;;;;
;;;;  

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;
;;;; SLIB interface to load psd files.

;;; this is not portable.
(define psd:control-z (integer->char 26))

;;; If you are using slib, use the following.
(require 'debug)
(load (in-vicinity (program-vicinity) "version" (scheme-file-suffix)))
(load (in-vicinity (program-vicinity) "instrum" (scheme-file-suffix)))
(load (in-vicinity (program-vicinity) "pexpr" (scheme-file-suffix)))
(load (in-vicinity (program-vicinity) "read" (scheme-file-suffix)))
(load (in-vicinity (program-vicinity) "runtime" (scheme-file-suffix)))
(load (in-vicinity (program-vicinity) "primitives" (scheme-file-suffix)))

;;;
;;; Say hello
;;;

(psd-announce-version)
