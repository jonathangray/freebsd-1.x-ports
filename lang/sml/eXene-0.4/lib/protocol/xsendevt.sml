(* xsendevt.sml
 *
 * COPYRIGHT (c) 1992 by AT&T.  See COPYRIGHT file for details.
 *
 * Functions to encode SendEvent messages.
 *)

structure XSendEvent =
  struct
    local
      open Geometry XProtTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val & = Bits.andb
      val ++ = Bits.orb
      infix << >> & ++

      val encodeSendEvent = XRequest.encodeSendEvent
      val eventOffset = 12

      val (put8 : (string * int * int) -> unit) = System.Unsafe.cast ByteArray.update
      val put8 = fn (s, i, j) => put8(s, i+eventOffset, j)

      fun putSigned8 (buf, i, x) = if (x < 0)
	    then put8(buf, i, x + 256)
	    else put8(buf, i, x)
      fun put16 (buf, i, x) = let val a = (x >> 8) and b = (x & 255)
	    in
	      put8(buf, i, a); put8(buf, i+1, b)
	    end
      fun putSigned16 (buf, i, x) = if (x < 0)
	    then put16(buf, i, x + 65536)
	    else put16(buf, i, x)
      fun put32 (buf, i, x) = let
	    val a = ((x >> 24) & 255) and b = ((x >> 16) & 255)
	    val c = ((x >> 8) & 255) and d = (x & 255)
	    in
	      put8(buf, i, a); put8(buf, i+1, b);
	      put8(buf, i+2, c); put8(buf, i+3, d)
	    end

      fun putXId (buf, i, XID n) = put32 (buf, i, n)
      fun putXIdOption (buf, i, NONE) = put32 (buf, i, 0)
	| putXIdOption (buf, i, SOME(XID n)) =  put32 (buf, i, n)

      fun putAtom (buf, i, XAtom n) = put32 (buf, i, n)
      fun putAtomOption (buf, i, NONE) = put32 (buf, i, 0)
	| putAtomOption (buf, i, SOME(XAtom n)) = put32 (buf, i, n)

      local
	open System.Timer
	val t2_32 = TIME{sec=4294967, usec=296000}  (* 2^32 msec *)
	val t2_31 = TIME{sec=2147483, usec=648000}  (* 2^31 msec *)
	val t2_30 = TIME{sec=1073741, usec=824000}  (* 2^30 msec *)
	fun adjustTime t =
	      if earlier(t, t2_32) then t else adjustTime(sub_time(t, t2_32))
	fun highBits t = let
	      val t = adjustTime t
	      val (t, b31) = if (earlier(t, t2_31))
		      then (t, 0)
		      else (sub_time(t, t2_31), 32768)
	      in
		if (earlier(t, t2_30))
		  then (t, b31)
		  else (sub_time(t, t2_30), b31++16384)
	      end
      in
      fun putTS (buf, i, CurrentTime) = put32(buf, i, 0)
	| putTS (buf, i, TimeStamp(t as TIME{sec, usec})) = 
	    put32(buf, i, sec*1000 + usec quot 1000)
		handle Overflow => let
		  val (TIME{sec, usec}, bits31_30) = highBits t
		  val remainder = sec*1000 + usec quot 1000
		  in
		    put16(buf, i, bits31_30++(remainder >> 16));
		    put16(buf, i+2, remainder & 65535)
		  end
      end (* local *)

    (* event codes *)
      val evtKeyPressXEvt = 2
      val evtKeyReleaseXEvt = 3
      val evtButtonPressXEvt = 4
      val evtButtonReleaseXEvt = 5
      val evtdecodeMotionNotify = 6
      val evtEnterNotifyXEvt = 7
      val evtLeaveNotifyXEvt = 8
      val evtFocusInXEvt = 9
      val evtFocusOutXEvt = 10
      val evtKeymapNotify = 11
      val evtExpose = 12
      val evtGraphicsExpose = 13
      val evtNoExpose = 14
      val evtVisibilityNotify = 15
      val evtCreateNotify = 16
      val evtDestroyNotify = 17
      val evtUnmapNotify = 18
      val evtMapNotify = 19
      val evtMapRequest = 20
      val evtReparentNotify = 21
      val evtConfigureNotify = 22
      val evtConfigureRequest = 23
      val evtGravityNotify = 24
      val evtResizeRequest = 25
      val evtCirculateNotify = 26
      val evtCirculateRequest = 27
      val evtPropertyNotify = 28
      val evtSelectionClear = 29
      val evtSelectionRequest = 30
      val evtSelectionNotify = 31
      val evtColormapNotify = 32
      val evtClientMessage = 33
      val evtMappingNotify = 34

      fun putEventCode (msg, code) = put8(msg, 0, code)

    in

    fun encodeSendSelectionNotify
	{ dst, propagate, evt_mask, requestor, selection, target, property, time } = let
	  val msg = encodeSendEvent {
			dst = dst, propagate = propagate, evt_mask = evt_mask
		      }
	  in
	    putEventCode (msg, evtSelectionNotify);
	    putXId (msg, 8, requestor);
	    putAtom (msg, 12, selection);
	    putAtom (msg, 16, target);
	    putAtomOption (msg, 20, property);
	    msg
	  end

    end (* local *)
  end (* XSendEvent *)
