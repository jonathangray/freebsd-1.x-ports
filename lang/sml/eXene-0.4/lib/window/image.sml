(* image.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This structure supports images (client-side data) and creating server-side
 * pixmaps from images.
 *
 * TODO
 *   - support a left-pad
 *   - support Z format
 *)

structure Image =
  struct

    exception BadImageData

  (* Format for images, XY format.
   * Each string in the string list corresponds to a scan line in a plane.
   * The outer list corresponds to the list of planes, with plane 0 being
   * the last item in the list.
   * 
   * Multiple planes are not very useful right now, as the pixel type is
   * opaque. It seemed reasonable, however, to allow createImageFromPixmap
   * to work on all pixmaps, and the necessary changes were minimal.
   *
   *)
    datatype image = IMAGE of {
	sz : Geometry.size,
	data : string list list
      }

    local
      open Geometry XProtTypes XRequest XDisplay Display DrawTypes

      val << = Bits.lshift
      val >> = Bits.rshift
      val & = Bits.andb
      val ++ = Bits.orb
      infix << >> & ++

    (* map a row of data coded as a string to a bit representation.  The data may
     * be either encoded in hex (with a preceeding "0x") or in binary (with a
     * preceeding "0b". *)
      fun stringToData (wid, s) = (case (explode s)
	   of ("0"::"x"::r) => let
		val nbytes = ((wid + 7) >> 3)   (* # of bytes per line *)
		fun nibbleOfChar c = let
		      val x = ord c
		      in
			if ((48 <= x) andalso (x <= 57)) (* '0'..'9' *)
			  then (x - 48)
			else if ((65 <= x) andalso (x <= 70)) (* 'A'..'F' *)
			  then (x - 55)
			else if ((97 <= x) andalso (x <= 102)) (* 'a'..'f' *)
			  then (x - 87)
			  else raise BadImageData
		      end
		fun mkRow (0, [], l) = implode(rev l)
		  | mkRow (0, _, _) = raise BadImageData
		  | mkRow (i, d1::d0::r, l) =
		      mkRow(i-1, r,
			chr(((nibbleOfChar d1)<<4)++(nibbleOfChar d0))::l)
		  | mkRow _ = raise BadImageData
		in
		  mkRow (nbytes, r, [])
		end
	    | ("0"::"b"::r) => let
		fun mkRow (0, _, [], b, l) = implode(rev((chr b)::l))
		  | mkRow (_, _, [], _, _) = raise BadImageData
		  | mkRow (i, 0, l1, b, l2) = mkRow(i, 128, l1, 0, (chr b)::l2)
		  | mkRow (i, m, "0"::r, b, l) = mkRow(i-1, m>>1, r, b, l)
		  | mkRow (i, m, "1"::r, b, l) = mkRow(i-1, m>>1, r, m++b, l)
		  | mkRow _ = raise BadImageData
		in
		  mkRow(wid, 128, r, 0, [])
		end
	    | _ => raise BadImageData
	   (* end case *))

    (* reverse the bit-order of a byte *)
      fun revBits b = let
	    val revTbl = "\
		  \\000\128\064\192\032\160\096\224\
		  \\016\144\080\208\048\176\112\240\
		  \\008\136\072\200\040\168\104\232\
		  \\024\152\088\216\056\184\120\248\
		  \\004\132\068\196\036\164\100\228\
		  \\020\148\084\212\052\180\116\244\
		  \\012\140\076\204\044\172\108\236\
		  \\028\156\092\220\060\188\124\252\
		  \\002\130\066\194\034\162\098\226\
		  \\018\146\082\210\050\178\114\242\
		  \\010\138\074\202\042\170\106\234\
		  \\026\154\090\218\058\186\122\250\
		  \\006\134\070\198\038\166\102\230\
		  \\022\150\086\214\054\182\118\246\
		  \\014\142\078\206\046\174\110\238\
		  \\030\158\094\222\062\190\126\254\
		  \\001\129\065\193\033\161\097\225\
		  \\017\145\081\209\049\177\113\241\
		  \\009\137\073\201\041\169\105\233\
		  \\025\153\089\217\057\185\121\249\
		  \\005\133\069\197\037\165\101\229\
		  \\021\149\085\213\053\181\117\245\
		  \\013\141\077\205\045\173\109\237\
		  \\029\157\093\221\061\189\125\253\
		  \\003\131\067\195\035\163\099\227\
		  \\019\147\083\211\051\179\115\243\
		  \\011\139\075\203\043\171\107\235\
		  \\027\155\091\219\059\187\123\251\
		  \\007\135\071\199\039\167\103\231\
		  \\023\151\087\215\055\183\119\247\
		  \\015\143\079\207\047\175\111\239\
		  \\031\159\095\223\063\191\127\255"
	  in
	    chr(ordof(revTbl, ord b))
	  end

    (* Routines to re-order bits and bytes to the server's format (stolen from
     * XPutImage.c in Xlib).  We represent data in the following format:
     *
     *   scan-line unit = 1 byte
     *   byte-order     = MSB first (doen't matter for 1-byte scan units)
     *   bit-order      = MSB first (bit 0 is leftmost on display)
     *
     * This is the "1Mm" format of XPutImage.c in Xlib.  The relevant line in the
     * conversion table is:
     *
     *         1Mm 2Mm 4Mm 1Ml 2Ml 4Ml 1Lm 2Lm 4Lm 1Ll 2Ll 4Ll
     *   1Mm:   n   n   n   R   S   L   n   s   l   R   R   R
     *   1Ml:   R   R   R   n   s   l   R   S   L   n   n   n
     *
     *   legend:
     *		n   no changes
     *		s   reverse 8-bit units within 16-bit units
     *		l   reverse 8-bit units within 32-bit units
     *		R   reverse bits within 8-bit units
     *		S   s+R
     *		L   l+R
     *)
      fun noSwap x = x
      fun swapBits s = implode(map revBits (explode s))
      fun swap2Bytes s = let
	    fun swap [] = []
	      | swap (a::b::r) = b::a::(swap r)
	      | swap _ = (MLXError.impossible "[swap2Bytes: bad image data]")
	    in
	      implode (swap (explode s))
	    end
      fun swap4Bytes s = let
	    fun swap [] = []
	      | swap (a::b::c::d::r) = d::c::b::a::(swap r)
	      | swap _ = (MLXError.impossible "[swap4Bytes: bad image data]")
	    in
	      implode (swap (explode s))
	    end
      fun swapBitsAnd2Bytes s = let
	    fun swap [] = []
	      | swap (a::b::r) = (revBits b)::(revBits a)::(swap r)
	      | swap _ = (MLXError.impossible "[swapBitsAnd2Bytes: bad image data]")
	    in
	      implode (swap (explode s))
	    end
      fun swapBitsAnd4Bytes  s = let
	    fun swap [] = []
	      | swap (a::b::c::d::r) =
		  (revBits d)::(revBits c)::(revBits b)::(revBits a)::(swap r)
	      | swap _ = (MLXError.impossible "[swapBitsAnd4Bytes: bad image data]")
	    in
	      implode (swap (explode s))
	    end
      fun swapFunc ( Raw8, MSBFirst, MSBFirst) = noSwap
	| swapFunc (Raw16, MSBFirst, MSBFirst) = noSwap
	| swapFunc (Raw32, MSBFirst, MSBFirst) = noSwap
	| swapFunc ( Raw8, MSBFirst, LSBFirst) = swapBits
	| swapFunc (Raw16, MSBFirst, LSBFirst) = swapBitsAnd2Bytes
	| swapFunc (Raw32, MSBFirst, LSBFirst) = swapBitsAnd4Bytes
	| swapFunc ( Raw8, LSBFirst, MSBFirst) = noSwap
	| swapFunc (Raw16, LSBFirst, MSBFirst) = swap2Bytes
	| swapFunc (Raw32, LSBFirst, MSBFirst) = swap4Bytes
	| swapFunc ( Raw8, LSBFirst, LSBFirst) = swapBits
	| swapFunc (Raw16, LSBFirst, LSBFirst) = swapBits
	| swapFunc (Raw32, LSBFirst, LSBFirst) = swapBits

      fun padToBits Raw8 = 8
        | padToBits Raw16 = 16
        | padToBits Raw32 = 32

      fun roundDown (nbytes, pad) = (nbytes & (Bits.notb ((padToBits pad) - 1)))

      fun roundUp (nbytes, pad) = let
	    val bits = (padToBits pad) - 1
	    in
	      (nbytes + bits) & (Bits.notb bits)
	    end
        
    (* Pad and re-order image data as necessary to match the server's format. *)
      fun adjustImageData (XDPY dpyInfo) = let
	    val padScanLine = case (#bitmap_scanline_pad dpyInfo)
		 of Raw8 => (fn s => s)
		  | Raw16 => (
		      fn s => (if (((String.length s)&1) = 0) then s else s^"\000"))
		  | Raw32 => (
		      fn s => (case ((String.length s) & 3)
			   of 0 => s | 1 => s^"\000\000\000"
			    | 2 => s^"\000\000" | _ => s^"\000"))
	    val swapfn = swapFunc (
		    #bitmap_scanline_unit dpyInfo,
		    #image_byte_order dpyInfo,
		    #bitmap_bit_order dpyInfo)
	    in
              fn data => map (fn s => swapfn (padScanLine s)) data
	    end (* adjustImageData *)

    (* Copy rectangle from image into pixmap.
     * It wouldn't take much to generalize this to all drawables & pens.
     * Additional efficiency could be gained by having the extractRow
     * function extract rows already padded correctly for the display,
     * if possible.
     *)
      fun putImage pm {src=IMAGE{sz,data},src_rect,dst_pt} = let
          (* clip src_rect to image *)
            val src_rect' = intersection(src_rect,mkRect(originPt,sz))

            val delta = subPt(originOfRect src_rect',originOfRect src_rect)
            val depth = length data
	    val PM{id, scr, scr_depth=SCRDEPTH{draw_cmd, ...}, ...} = pm
	    val SCREEN{dpy=DPY{xdpy=xdpy as XDPY dpyInfo, ...}, ...} = scr
            val scanlinePad = #bitmap_scanline_pad dpyInfo
            val scanlineUnit = #bitmap_scanline_unit dpyInfo

          (* Minimum no. of 4-byte words needed for PutImage.
           * There should be a function in XRequest to provide this.
           *)
            val rqstSz = 6
          (* Number of image bytes per each request *)
            val available = (min(#max_req_len dpyInfo,65536) - rqstSz) << 2

            fun putImageRqst (r as RECT{x,y,wid,ht}, dst_pt) = let
		  val leftPad = x & (padToBits scanlineUnit - 1)
		  val byteOffset = (x - leftPad) >> 3
		  val numBytes = roundUp(wid + leftPad, Raw8) >> 3
		  val adjust = adjustImageData xdpy
		(* Given the list of data for a plane, extract a list of substrings
		 * corresponding to given rectangle, to the nearest byte.
		 *)
		  fun extractRect (rows : string list) = let
			fun skip (0, r) = r
			  | skip (i, _::r) = skip(i-1,r)
			  | skip (i, []) = MLXError.impossible "Image: extractRect(skip)"
			fun extractRow (0, _) = []
			  | extractRow (i, row::rest) =
			      if (byteOffset = 0 andalso numBytes = size row)
				then row::(extractRow(i-1, rest))
				else (
				  substring(row, byteOffset, numBytes))
				    :: (extractRow (i-1, rest))
			  | extractRow (i,[]) = MLXError.impossible "Image: extractRow"
			in
			  extractRow(ht,skip(y,rows))
			end (* extractRect *)
		  val xdata = map extractRect data
		  in
		    draw_cmd (DrawMaster.DMSG{
			dst = id,
			pen = Pen.defaultPen,
			oper = DrawMaster.DOP_PutImage{
			    dst_pt = dst_pt,
			    size = SIZE{wid=wid,ht=ht},
			    depth = depth,
			    lpad = leftPad,
			    format = XYPixmap,
			    data = implode (map (implode o adjust) xdata)
			  }
		      })
		  end (* putImageRqst *)

          (* decompose putImage into multiple request smaller than max. size.
           * First try to use as many rows as possible. If there is only one
           * row left and it is still too large, decompose by columns.
           *)
	    fun putSubImage (r as RECT{x,wid,ht,y}, pt as PT{x=dx,y=dy}) = let
		  val leftPad = x & (padToBits scanlineUnit - 1)
		  val bytesPerRow = (roundUp(wid + leftPad,scanlinePad) >> 3) * depth
		  in
		    if ((bytesPerRow * ht) <= available)
		      then putImageRqst (r,pt)
		    else if (ht > 1)
		      then let
			val ht' = max(1, available div bytesPerRow)
			in
			  putSubImage (RECT{x=x,y=y,wid=wid,ht=ht'}, pt);
			  putSubImage (RECT{x=x,y=y+ht',wid=wid,ht=ht-ht'},
			    PT{x=dx,y=dy+ht'})
			end
		      else let
			val wid' = roundDown(available << 3,scanlinePad) - leftPad
			in
			  putSubImage (RECT{x=x,y=y,wid=wid',ht=1},pt);
			  putSubImage (RECT{x=x+wid',y=y,wid=wid-wid',ht=1},
			    PT{x=dx+wid',y=dy})
			end
		  end
	    in
	      putSubImage (src_rect',addPt(dst_pt,delta))
	    end (* putImage *)

    in

  (* create image data from an ascii representation *)
    fun imageFromAscii (wid, []) = raise BadImageData
      | imageFromAscii (wid, p0::rest) = let
	  fun mk (n, [], l) = (n,rev l)
	    | mk (n, s::r, l) = mk(n+1, r, stringToData(wid, s) :: l)
          val (ht,plane0) = mk(0, p0, [])
          fun chk data = let 
                val (h,plane) = mk(0,data,[]) 
                in
                    if h = ht then plane else raise BadImageData
                end
	  in
	    IMAGE{sz=SIZE{wid=wid, ht=ht}, data=plane0::(map chk rest)}
	  end

  (* create a pixmap from image data. *)
    fun createPixmapFromImage scr (image as IMAGE{sz, data}) = let
          val depth = length data
	  val pm = Pixmap.createPixmap scr (sz, depth)
	  in
	    putImage pm {
              src=image, 
              src_rect = mkRect(Geometry.originPt,sz), 
              dst_pt = Geometry.originPt};
	    pm
	  end (* createPixmapFromImage *)

  (* create a pixmap from ascii data. *)
    fun createPixmapFromAsciiData scr (wid, ascii_rep) =
	  createPixmapFromImage scr (imageFromAscii(wid, ascii_rep))

  (* create an image from a pixmap.
   * This should be better integrated with the draw-master, 
   * to avoid a possible race condition, i.e., we need to be sure
   * the draw-master flush has occurred before we ask for the image.
   *)
    fun createImageFromPixmap (PM{id, sz = sz as SIZE szinfo, scr_depth, scr}) = let
	  val SCRDEPTH{depth,draw_cmd, ...} = scr_depth
	  val DPY{xdpy=XDPY dpyInfo,...} = displayOfScr scr
	  val _ = draw_cmd (DrawMaster.DMSG_Flush)
	  val AllPlanes = ~1
	  val msg = encodeGetImage { 
		  drawable = id, 
		  rect = mkRect(originPt,sz),
		  plane_mask = PLANEMASK AllPlanes, 
		  format = XYPixmap
		}
	  val {depth, data, visualid} = 
		XReply.decodeGetImageReply (
		  CML.sync (XIo.requestReply (#conn dpyInfo) msg))
	  val swapfn = swapFunc (
		#bitmap_scanline_unit dpyInfo,
		#image_byte_order dpyInfo,
		#bitmap_bit_order dpyInfo)
	  val linesPerPlane = #ht szinfo
	  val bytesPerLine = roundUp (#wid szinfo, #bitmap_scanline_pad dpyInfo) >> 3
	  val bytesPerPlane = bytesPerLine * linesPerPlane
	  fun doLine start = swapfn(substring(data,start,bytesPerLine))
	  fun mkLine (i,start) =
		if i = linesPerPlane
		  then []
		  else (doLine start)::(mkLine(i+1,start+bytesPerLine))
	  fun mkPlane (i,start) =
		if i = depth
		  then []
		  else (mkLine(0,start))::(mkPlane(i+1,start+bytesPerPlane))
	  in
	    IMAGE{sz=sz,data=mkPlane (0,0)}
	  end (* createImageFromPixmap *)

      fun createImageFromTile (TILE pm) = createImageFromPixmap pm

    end (* local *)

  end (* Image *)
