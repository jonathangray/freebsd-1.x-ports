(*
	Test driver for idlserver

	( this wasn't designed scientifically; it
	 just exercises SOME of the functionality )

	@(#)idlserver.run.sml	2.1 93/03/07 23:55:53
*)

import "../Xidl/idlbase.sig";
import "../Xidl/idlbase";
import "idlserver";
structure idlbase = idlbase();
structure idlserver=idlserver(idlbase);
open idlserver;
fun ts()   = (realize(); sync());

fun makedrawing parent (name, width, height) =
let val frame = CreateWidget parent name formwidget [];
    val canvas = CreateWidget frame "canvas" simplecanvaswidget
	[("width",  short width),
	 ("height", short height)];
    val hbar = CreateWidget frame  "horizontal" scrollbarwidget
	[("orientation", orient.Horizontal),
	 ("length",      int width),
	 ("thickness",   int 10)];
    val vbar = CreateWidget frame "vertical" scrollbarwidget
	[("orientation", orient.Vertical),
	 ("length",      int height),
	 ("thickness",   int 10)];
    val ww = ref width;
    val hh = ref height;
    fun extent _ =
    let val (x, y, w, h) = GetXYWH canvas
	val (ox, oy)     = GetXY canvas
	fun a / b = if b=0 then 1000 else (a*1000) div b
    in
	print(format "(%,%)(%-%, %-%)\n" [ox, oy, x, w, y, h]);
	scrollbarsetthumb (hbar, 0, !ww/w);
	scrollbarsetthumb (vbar, 0, !hh/h);
	()
    end
    fun expose _ =
    (   ww := getshort(canvas, "width");
	hh := getshort(canvas, "height");
	SimpleCanvasRedraw canvas;
	extent()
    )

in
    canvas /// hbar;
    canvas ||| vbar;
    scrollbarsetthumb (hbar, 0, 1000);
    scrollbarsetthumb (vbar, 0, 1000);
    AugmentTranslations  frame  [ ("<Expose>", expose) ];
    extentcallback canvas extent;
    offsetcallback canvas extent;
    {widget=frame, canvas=canvas, width=ww, height=hh}
end;


local
	fun show s =
	    (fn _ => outputc std_err (s^"\n"));

	fun showlabel(_, widget, _, _) =
	    outputc std_err (getstring(widget, "label")^"\n")

	fun showstate name state =
	    (outputc std_err (name^" is "^(if state then "on\n" else "off\n")))
in
	type togglecallback = bool -> unit;
	val showstate : string -> togglecallback = showstate;
	val showlabel : callback                 = showlabel;
	val show      : string -> callback       = show;
end



val TOP     = idlserver[];

val top     = CreateWidget TOP "outer" formwidget
	      [("orientation", orient.Vertical)];

val QUIT    = NewCommand   top ("QUIT", [], (fn _ => raise Interrupt));

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


val radio1  = Radio top true  []
	      [("radio11", true,  showstate "11"),
	       ("radio12", false, showstate "12")];
val radio2  = Radio top false []
	      [("radio21", true,  showstate "21"),
	       ("radio22", false, showstate "22")];


val label1  = CreateWidget top ("label1", labelwidget, []);
val label2  = CreateWidget top ("label2", labelwidget, []);
val label3  = CreateWidget top ("label3", labelwidget, []);
val text    = CreateWidget top
	      "text"  textwidget
		       [("type",        text.AsciiString),
			("editType",    text.Edit),
			("width",       short 250)
		       ];

val d1 = makedrawing top ("foo", 100, 80);
val d2 = makedrawing top ("baz", 200, 40);

#widget QUIT
     /// #widget menu1  /// #widget menu2  /// #widget menu3
     /// #widget radio1 /// #widget radio2 /// text /// #widget d1 /// #widget d2;

ts();
DrawStr (#canvas d1, 1, 1, "Here is canvas 1", 10, 40);
DrawLine(#canvas d1, 1, 10, 40, 90, 10);
DrawStr(#canvas d2, 1, 1, "Here is canvas 2", 50, 20);
(*
	val c6 = CreateFontCursor 6;
	val c10 = CreateFontCursor 10;
	DefineCursor(label1, c10);
	DefineCursor(label2, c6);
*)

fun For (m, n) p = if m>n then () else (p m; For (m+1, n) p);

fun tile m n =
    For (0, m)
    (fn x =>
	For (0, n)
	  (fn y =>  DrawBox(#canvas d1, 1, x*10, y*10, 10, 10)));

DrawBox(#canvas d1, 0, 0,0, 5000, 5000);
tile 50 50;
run();
