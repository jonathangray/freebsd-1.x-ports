(*
	Test driver for idlx

	( this wasn't designed scientifically; it
	 just exercises SOME of the functionality )

	@(#)idlx.run.sml	2.1 93/03/07 00:58:14
*)

import "../Xidl/idlbase.sig";
import "../Xidl/idlbase";
import "idlx";
structure idlbase = idlbase();
structure idlx=idlx(idlbase);
open idlx; open idlx.SimpleCanvas;

fun begin() = (realize(); sync());

fun ScrolledCanvas parent (name, width, height) =
let val frame = CreateWidget parent name formwidget [];
    val canvas =
    CreateWidget frame "canvas" SimpleCanvas.widget
    [("width", short width),
     ("height", short height)];
    val hbar =
    CreateWidget frame "horizontal" scrollbarwidget
    [("orientation", orient.Horizontal),
     ("length", int width),
     ("thickness", int 10)];
    val vbar =
    CreateWidget frame "vertical" scrollbarwidget
    [("orientation", orient.Vertical),
     ("length", int height),
     ("thickness", int 10)];

    val width  = ref width;
    val height = ref height;
    val offset = ref (0,0);
    val extent = ref (0,0,0,0);

    fun a / b = if b = 0 then 1000 else (a * 1000) div b;

    fun Extent _ =
    let val extent' as (x, y, w, h) = GetExtent canvas;
	val offset' as (ox', oy')   = GetOffset canvas
    in
	offset := offset';
	extent := extent';
	print (format "(%,%)(%-%, %-%)\n" [ox', oy', x, w, y, h]);
	scrollbarsetthumb (hbar, (ox'-x)/w, ! width / w);
	scrollbarsetthumb (vbar, (oy'-y)/w, ! height / h);
	()
    end;

    fun Expose _ =
    let in
      width  := getshort (canvas, "width");
      height := getshort (canvas, "height");
      Redraw canvas;
      Extent ()
    end;

    fun ScrollX (_, _, x, y) =
    (print (format "ScrollX(%,%)\n" [x, y]);
     SetX (canvas, #1(!extent) + x * #3(!extent) div !width) ; ());
    fun ScrollY (_, _, x, y) =
    (print (format "ScrollY(%,%)\n" [x, y]);
     SetY (canvas, #2(!extent) + y * #4(!extent) div !height); ());


    (*  Rubber-banding *)
    val downx = ref 0;
    val downy = ref 0;
    val lastx = ref 0;
    val lasty = ref 0;
    val gcon  = RoGC (SimpleCanvas.GetGC (canvas, 1));
    val gcoff = RoGC (SimpleCanvas.GetGC (canvas, 255));
    val visible = ref false;
    fun vanish () =
    if ! visible then
    let in
      SetXGC gcoff;
      XdrawLine (! downx, ! downy, ! lastx, ! lasty);
      visible := false
    end
    else ();
    fun down (_, _, x, y) =
    let in
      downx := x;
      downy := y;
      SetXDrawable (IdlWindow (canvas));
      visible := false
    end;
    fun up (_, _, x, y) =
    let in
	vanish ();
	DrawLine
	(canvas, 1, ! downx + #1(!offset), ! downy + #2(!offset), x - ! downx, y - ! downy);
	SimpleCanvas.Redraw canvas (* repair any visible damage *)
    end;
    fun move (_, _, x, y) =
    let in
	vanish ();
	SetXGC gcon;
	lastx := x;
	lasty := y;
	XdrawLine (! downx, ! downy, ! lastx, ! lasty);
	visible := true
    end

    fun select(_,_, x, y) =  (* select a text item *)
	(FindAndSelect(canvas, x, y);())
in
    canvas ABOVE hbar;
    canvas BESIDE vbar;
    AugmentTranslations frame [("<Expose>", Expose)];
    OverrideTranslations canvas
    [("<Btn1Down>",   down),
     ("<Btn1Up>",     up),
     ("<Btn1Motion>", move),
     ("<Btn2Down>",   select)
    ];
    extentcallback canvas Extent;
    offsetcallback canvas Extent;
    whenscrolled hbar ScrollX;
    whenscrolled vbar ScrollY;
    {widget = frame, canvas = canvas, width = width, height = height}
end;


local
    fun show s = (fn _ => outputc std_err (s ^ "\n"));
    fun showlabel (_, widget, _, _) =
    outputc std_err (getstring (widget, "label") ^ "\n");
    fun showstate name state =
    (outputc std_err (name ^ " is " ^ (if state then "on\n" else "off\n")))
in
    type togglecallback = bool->unit;

    val showstate : string -> togglecallback = showstate;
    val showlabel : callback                 = showlabel;
    val show      : string -> callback       = show
end;



exception Quit;

fun run1 () =
let val TOP = idlx [];
    val top =
    CreateWidget TOP "outer" formwidget [("orientation", orient.Vertical)];

    val QUIT = NewCommand top ("QUIT", [], (fn _ => raise Quit));

    local
	open Menu
    in
	val menu1 =
	New top
	("menu1", [],
	 [Command ("menu1", showlabel),
	  Command ("menu2", showlabel),
	  Line,
	  Command ("menu3", showlabel)]);

	val menu2 =
	New top
	("menu2", [],
	 [Command ("menu1", show "menu1"),
	  Command ("menu2", show "menu2")]);

	val menu3 =
	New top
	("menu3", [],
	 [Command ("menu1", show "menu1"),
	  Command ("menu2", show "menu2")])
    end;

    val radio1 =
    Radio top true []
    [("radio11", true,  showstate "11"),
     ("radio12", false, showstate "12")];

    val radio2 =
    Radio top false []
    [("radio21", true,  showstate "21"),
     ("radio22", false, showstate "22")];

    val label1 = CreateWidget top "label1" labelwidget [];

    val label2 = CreateWidget top "label2" labelwidget [];

    val label3 = CreateWidget top "label3" labelwidget [];

    val text =
    CreateWidget top "text" textwidget
    [("type", text.AsciiString),
     ("editType", text.Edit),
     ("width", short 250)];

    val d1 = ScrolledCanvas top ("foo", 100, 80);

    val d2 = ScrolledCanvas top ("baz", 200, 40);

in
    label1         ABOVE
    label2         ABOVE
    label3         ABOVE
    #widget QUIT   ABOVE
    #widget menu1  ABOVE
    #widget menu2  ABOVE
    #widget menu3  ABOVE
    #widget radio1 ABOVE
    #widget radio2 ABOVE
    text           ABOVE
    #widget d1     ABOVE
    #widget d2;
    realize(); sync();
    DrawString  (#canvas d1, 1, 1, "Here is canvas 1", 10, 40);
    DrawLine (#canvas d1, 1, 10, 40, 90, 10);
    DrawString  (#canvas d2, 1, 1, "Here is canvas 2", 50, 20);
    let
	fun For (m, n) p = if m>n then () else (p m; For (m+1, n) p);

	fun tile m n =
	    For (0, m)
	    (fn x =>
		For (0, n)
		  (fn y =>  DrawBox(#canvas d1, 1, x*100, y*100, 100, 100)));

	val c6 = CreateFontCursor 6;
	val c10 = CreateFontCursor 10;

    in  DefineCursor(label1, c10);
	DefineCursor(label2, c6);
	tile 5 5
    end
end;


fun main _ = run(run1()) handle Quit=>();
