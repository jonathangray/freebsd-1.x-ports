(* vtty-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * A simple virtual terminal built on top of the text widget.  This supports
 * an interface that is compatible with the CIO structure in CML.
 *)

signature VTTY =
  sig

    structure CIO : CONCUR_IO
    structure W : WIDGET

    type vtty

    val widgetOf : vtty -> W.widget

    val mkVtty : W.root -> {rows : int, cols : int} -> vtty
    val openVtty : vtty -> (CIO.instream * CIO.outstream)

  end (* VTTY *)
