
local
  open CML EXeneBase Geometry Widget Shell BitmapEdit

  fun tester server = let
    val root = mkRoot server
    fun quit () = (delRoot root; RunCML.shutdown())
    val horzCells = 23
    val vertCells = 16
    val scr = screenOf root
    val black = blackOfScr scr
    val white = whiteOfScr scr
    val colorOf = colorOfScr scr
    val filename = "dump"

    val bme = mkBitmapEdit root {cellSize=16,horzCells=horzCells,vertCells=vertCells}
    val bm = Bitmap.mkBitmap root 
      { horzCells=horzCells,
        vertCells=vertCells,
        foregrnd=SOME black,
        backgrnd=SOME white}
    val rbm = Bitmap.mkBitmap root 
      { horzCells=horzCells,
        vertCells=vertCells,
        backgrnd=SOME black,
        foregrnd=SOME white}

    val trans = Vector.tabulate (16, 
      fn i => if i < 10 then chr(i+(ord "0")) else chr(i - 10 + (ord "A")))

    fun write () = let
      open Format Bits
      val outs = open_out filename
      val IMAGE{sz=SIZE{wid,...},data} = Bitmap.imageOf bm
      val outfn = outputc outs


      fun putLine s = let
        fun foldfn (c, l) = let
          val ms = Vector.sub(trans,rshift(ord c,4))
          val ls = Vector.sub(trans,andb(ord c,0xF))
        in
          ms::ls::l
        end
      in
        outfn (implode("\"0x"::(fold foldfn (explode s) ["\""])))
      end

      fun putPlane lines = let
        fun doLines (prefix,[]) = ()
          | doLines (prefix,[l]) = (outfn prefix; putLine l; outfn " ]")
          | doLines (prefix,l::rest) = 
              (outfn prefix; putLine l; outfn ",\n"; doLines("      ",rest))
      in
        doLines ("    [ ", lines)
      end

      fun putPlanes [] = ()
        | putPlanes [p] = (putPlane p; outfn "\n")
        | putPlanes (p::rest) = (putPlane p; outfn ",\n"; putPlanes rest)
    in
      formatf "(%d, [\n" outfn [INT wid];
      putPlanes data;
      outfn "])\n";
      close_out outs
    end 

    val view = ScrollPort.mkScrollPort {
        color = NONE,
        continuous = true,
        vsb = SOME{left=true},
        hsb = SOME{top=true},
        widget = widgetOf bme
      }
    val quitBtn = Button.mkTextCmd root {
        action = fn () => quit (),
        backgrnd = NONE,
        foregrnd = NONE,
        rounded = false,
        label = "Quit"
      }
    val writeBtn = Button.mkTextCmd root {
        action = fn () => write (),
        backgrnd = NONE,
        foregrnd = NONE,
        rounded = false,
        label = "Write"
      }
    val box = Box.mkLayout root (Box.VtCenter[
        Box.VtCenter[
          Box.Glue{min=2,nat=2,max=SOME 2},
          Box.HzCenter[
            Box.Glue{min=3,nat=20,max=SOME 20},
            Box.WBox(Shape.mkRigid (Button.widgetOf quitBtn)),
            Box.Glue{min=3,nat=20,max=SOME 20},
            Box.WBox(Shape.mkRigid (Button.widgetOf writeBtn)),
            Box.Glue{min=3,nat=20,max=SOME 20},
            Box.WBox(Frame.widgetOf(Frame.mkFrame {
              width = 1,
              color = SOME(colorOf (CMS_RGB{red=0, green=0, blue=65535})),
              widget= Bitmap.widgetOf bm
            })),
            Box.Glue{min=3,nat=20,max=SOME 20},
            Box.WBox(Frame.widgetOf(Frame.mkFrame {
              width = 1,
              color = SOME(colorOf (CMS_RGB{red=0, green=0, blue=65535})),
              widget= Bitmap.widgetOf rbm
            })),
            Box.Glue{min=3,nat=20,max=NONE}
          ],
          Box.Glue{min=2,nat=2,max=SOME 2}
        ],
        Box.WBox(Divider.mkHorzDivider root {width=1,color=NONE}),
        Box.WBox(ScrollPort.widgetOf view)
      ]) 
    val shell = Shell.mkShell (Box.widgetOf box, NONE,
      { win_name = SOME "eXene test-1", icon_name = SOME "test-1" })

    fun main () = let
      val setfn = Bitmap.setPixel bm
      val rsetfn = Bitmap.setPixel rbm
      val evt = evtOf bme
      fun loop () = let
        val change = sync evt
      in
        setfn change;
        rsetfn change;
        loop ()
      end
    in
      loop ()
    end
  in
    spawn main;
    init shell
  end
in
fun doit' (debugFlags, server) = (
      XDebug.init debugFlags;
      RunCML.doit (fn () => tester server, SOME 20))

fun doit s = doit'([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end (* local *)
