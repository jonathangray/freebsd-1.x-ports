(* widget.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Definitions for basic graphical objects.
 *)

signature WIDGET =
  sig

    structure CML : CONCUR_ML
    structure G : GEOMETRY
    structure EXB : EXENE_BASE
    structure Interact : INTERACT
    structure EXW : EXENE_WIN

    datatype valign = VCenter | VTop | VBottom
    datatype halign = HCenter | HRight | HLeft

    datatype wstate = Active of bool | Inactive of bool

    exception BadIncrement

    datatype dim = DIM of {
        base    : int,
        incr    : int,
        min     : int,
        nat     : int,
        max     : int option
      }

    (* type bounds = { x_dim : dim, y_dim : dim } *)
    type bounds
    val mkBounds : { x_dim : dim, y_dim : dim } -> bounds

    val fixDim : int -> dim
    val natDim : dim -> int
    val minDim : dim -> int
    val maxDim : dim -> int option
    val fixBounds : (int * int) -> bounds
    val compatibleDim : dim * int -> bool
    val compatibleSize : bounds * G.size -> bool

    type root

    val mkRoot : string -> root
    val delRoot : root -> unit
    val sameRoot : root * root -> bool
    val displayOf : root -> EXB.display
    val screenOf : root -> EXB.screen

    type widget

    exception AlreadyRealized

    (* type realize_fn = {
     *      env : Interact.in_env,
     *      win : EXB.window,
     *      sz : G.size
     *   } -> unit
     *)
    type realize_fn

    val mkWidget : {
        root : root,
        boundsOf : unit -> bounds,
        realize : realize_fn
      } -> widget
    val rootOf : widget -> root
    val boundsOf : widget -> bounds
    val realizeFn : widget -> realize_fn
    val sameWidget : widget * widget -> bool
    val natSize : widget -> G.size
    val boundsFn : widget -> unit -> bounds
    val okaySize : widget * G.size -> bool

    val filterMouse : widget -> 
      (widget * 
        ((Interact.mouse_msg Interact.addr_msg CML.event * Interact.mouse_msg Interact.addr_msg CML.chan) CML.event))

    val filterKey : widget -> 
      (widget * 
        ((Interact.kbd_msg Interact.addr_msg CML.event * Interact.kbd_msg Interact.addr_msg CML.chan) CML.event))

    val filterCmd : widget -> 
      (widget * 
        ((Interact.cmd_in Interact.addr_msg CML.event * Interact.cmd_in Interact.addr_msg CML.chan) CML.event))

    val ignoreMouse : widget -> widget
    val ignoreKey : widget -> widget

    val wrapCreate : (EXB.window * G.rect) -> EXB.window

    val wrapQueue : '1a CML.event -> '1a CML.event

  end (* WIDGET *)

structure Widget : WIDGET =
  struct

    structure CML = CML
    structure G = Geometry
    structure EXB = EXeneBase
    structure Interact = Interact
    structure EXW = EXeneWin

    open Geometry EXeneBase Interact EXeneWin

    datatype valign = VCenter | VTop | VBottom
    datatype halign = HCenter | HRight | HLeft

    datatype wstate = Active of bool | Inactive of bool

    exception BadIncrement

    datatype dim = DIM of {
      base    : int,
      incr    : int,
      min     : int,
      nat     : int,
      max     : int option
      }

    type bounds = { x_dim : dim, y_dim : dim }
    fun mkBounds x = x

    fun fixDim x = DIM {base = x, incr = 1, min = 0, nat = 0, max = SOME 0}
    fun natDim (DIM{base,incr,nat,...}) = base + incr*nat
    fun minDim (DIM{base,incr,min,...}) = base + incr*min
    fun maxDim (DIM{base,incr,max=NONE,...}) = NONE
      | maxDim (DIM{base,incr,max=SOME max,...}) = SOME(base + incr*max)

    fun fixBounds (x,y) = {x_dim = fixDim x, y_dim = fixDim y}

    fun compatibleDim (dim,v) =
          (minDim dim <= v) andalso
            case maxDim dim of
              NONE => true
            | SOME max => v <= max

    fun compatibleSize ({x_dim,y_dim} : bounds, SIZE{wid,ht}) =
          compatibleDim(x_dim,wid) andalso compatibleDim(y_dim,ht)

  (* Root object, corresponding to display/screen pair.
   *  server = ""          => "unix:0.0"
   *         = ":d"        => "unix:d.0"
   *         = "host:d"    => "host:d.0"
   *         = "host:d.s"  => "host:d.s"
   * where host is an internet address (e.g., "128.84.254.97") or "unix".
   *
   * At present, screen is always the default screen.
   *)
    datatype root = Root of {
      id : unit ref,
      scr : screen,
      idGen : unit -> int
    }

    fun mkRoot server = let
          val scr = defaultScreenOf (openDisplay server)
          val idChan = CML.channel ()
          fun idLoop i = (CML.send(idChan,i);idLoop(i+1))
          in
            CML.spawn (fn () => idLoop 0);
            Root {id = ref (), scr = scr, idGen = fn () => CML.accept idChan}
          end

    fun screenOf (Root {scr,...}) = scr
    fun displayOf (Root {scr,...}) = displayOfScr scr
    fun delRoot root = closeDisplay (displayOf root)
    fun sameRoot (Root {id,...},Root{id=id',...}) = id = id'

    type realize_fn = {
         env : Interact.in_env,
         win : EXB.window,
         sz : G.size
      } -> unit

    exception AlreadyRealized

    datatype widget = Widget of {
	root : root,
        id : int,
	bounds_of : unit -> bounds,
        realized : unit CML.cond_var,
	realize : realize_fn
      }

    fun mkWidget {root=root as Root{idGen,...}, boundsOf,realize} =
          Widget {
	    root = root,
            realized = CML.condVar (),
            id = idGen(),
	    bounds_of = boundsOf,
	    realize = realize
          }
    fun rootOf (Widget{root,...}) = root
    fun boundsOf (Widget{bounds_of,...}) = bounds_of ()
    fun boundsFn (Widget{bounds_of,...}) = bounds_of
    fun realizeFn (Widget{realize,realized,...}) arg = (
          (CML.writeVar(realized,())) handle _ => raise AlreadyRealized; 
          realize arg)
    fun sameWidget (Widget{id,root,...},Widget{id=id',root=root',...}) =
          id = id' andalso sameRoot(root,root')
    fun natSize (Widget{bounds_of,...}) = let
          val {x_dim,y_dim} = bounds_of ()
          in
            SIZE{wid = natDim x_dim, ht = natDim y_dim}
          end
    fun okaySize (widget, sz) = compatibleSize(boundsOf widget, sz)

    fun wrapCreate (pwin, rect) = let
	  val SIZE{wid,ht} = sizeOfRect rect
	  in
	    if (wid <= 0) orelse (ht <= 0) 
              then LibBase.badArg{
                     module="Widget",
                     func="wrapCreate",
                     msg="invalid size"
                   }
              else ();
	    createSimpleSubwin pwin {
	        geom = WGEOM{pos=originOfRect rect, sz=sizeOfRect rect, border=0},
	        backgrnd = NONE,
	        border = NONE  (* not used *)
	      }
	  end

    fun filterWidget (selfn,repfn) (Widget{root,realize,bounds_of,...}) = let
      open CML
      val realizeCh = channel ()
      fun realize' {win,env,sz} = let
        val evt = selfn env
        val echan = channel ()
        val env' = repfn (env, receive echan)
        in
          send(realizeCh, (evt,echan));
          realize{win=win,sz=sz,env=env'}
        end
      in
	(mkWidget {
          root = root,
	  boundsOf = bounds_of,
	  realize = realize'
        },
        receive realizeCh)
      end
    val filterMouse = filterWidget (fn (InEnv{m,...}) => m, replaceMouse)
    val filterKey = filterWidget (fn (InEnv{k,...}) => k, replaceKey)
    val filterCmd = filterWidget (fn (InEnv{ci,...}) => ci, replaceCI)

    fun ignoreWidget (selfn,repfn) (Widget{root,realize,bounds_of,...}) = let
      open CML
      fun realize' {win,env,sz} = let
        fun loop evt () = (sync evt; loop evt ())
        val env' = repfn (env, readVarEvt (condVar()))
        in
          spawn (loop (selfn env));
          realize{win=win,sz=sz,env=env'}
        end
      in
	mkWidget {
          root = root,
	  boundsOf = bounds_of,
	  realize = realize'
        }
      end
    val ignoreMouse = ignoreWidget (fn (InEnv{m,...}) => m, replaceMouse)
    val ignoreKey = ignoreWidget (fn (InEnv{k,...}) => k, replaceKey)

    fun wrapQueue ine = let
          val outchan = CML.channel()
          fun loop ([],[]) = loop([CML.sync ine],[])
            | loop ([],l) = loop(rev l,[])
            | loop (l as e::tl,rest) = 
                loop (CML.select [
                  CML.wrap(CML.transmit(outchan,e),fn () => (tl,rest)),
                  CML.wrap(ine,fn e => (l,e::rest))
                ])
          in
            CML.spawn(fn () => loop ([],[]));
            CML.receive outchan
          end

  end (* Widget *)
