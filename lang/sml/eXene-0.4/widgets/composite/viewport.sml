(* viewport.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Viewport widget, for panning over a child widget.
 *
 * TODO:
 *   Allow child window to vary within bounds.
 *   Parameterize by child (granularity, specific scroll function)
 *)

structure Viewport : VIEWPORT = struct

  structure W = Widget
  structure CML = CML

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

  datatype req_msg
    = DoRealize of {
          env : in_env,
          win : window,
          sz : size
        }
    | Get
    | Set of { horz : int option, vert : int option }

  type geometry = { rect : rect, childSz : size }

  datatype reply_msg
    = Okay
    | Geometry of geometry
    | Bad

  datatype viewport = VP of {
      widget : widget,
      evtc : geometry event,
      reqc : req_msg chan,
      repc : reply_msg chan
    }

  fun naturalSz {x_dim,y_dim} = (natDim x_dim,natDim y_dim)

    (* Compute child's initial size given view's size and origin *)
  fun childRect (SIZE{wid,ht},RECT{x,y,...},csz) = let
    val (cwid,cht) = naturalSz csz
  in
    RECT{x=0,y=0,wid=max(cwid,x+wid),ht=max(cht,y+ht)}
  end

  fun viewBnds cbnds = let
    fun looseDim v = DIM{base=0,incr=1,min=1,nat=v,max=NONE}
    fun bnds () = let
      val {x_dim,y_dim} = cbnds ()
    in
      {x_dim= looseDim (natDim x_dim), y_dim = looseDim (natDim y_dim)}
    end
  in
    bnds
  end

    (* Adjust view's rectangle constrained by child *)
  fun newOrigin ({horz,vert},{rect=RECT{x,y,wid,ht},childSz=SIZE s}) = let
    val x = case horz of SOME h => h | _ => x
    val y = case vert of SOME v => v | _ => y
  in
    if x + wid <= #wid s andalso y + ht <= #ht s then
      SOME(RECT{x=x,y=y,wid=wid,ht=ht})
    else NONE
  end

    (* Adjust child given view's new size *)
  fun newSize (RECT{wid,ht,...},{rect=RECT{x,y,...},childSz=SIZE s}) = let
    val rect' = RECT{wid=wid,ht=ht,x=x,y=y}
  in
    {rect=rect',childSz=SIZE{wid=max(x+wid, #wid s),ht=max(y+ht, #ht s)}}
  end

    (* Handle child's request for resizing : unimplemented *)
  fun doResizeReq g = g

  fun filter (inevt, outchan) = let
    val timeOut = timeout(TIME{sec=0,usec=30000})
    val filterCnt = 10
    fun optSend (i,v) = if i <> filterCnt then send(outchan,v) else ()

    fun main () =
      case sync inevt of
        v as Set _ => (send(outchan,v); counter(filterCnt,v))
      | Get => (send(outchan, Get); main ())
      | _ => main ()
    and counter (0,v) = (send(outchan,v); counter(filterCnt,v))
      | counter (arg as (i,v)) =
          select [
            wrap(timeOut, fn () => (optSend arg; main())),
            wrap(inevt, fn evt =>
              case evt of
                v' as Set _ => counter(i-1,v')
              | Get => (optSend arg; send(outchan, Get); main ())
              | _ => (optSend arg; main ())
            )
          ]
  in
    main ()
  end

  fun mkViewport widget = let
    val root = rootOf widget
    val scr = screenOf root
    val reqChan = channel () 
    val repChan = channel ()
    val evtChan = channel ()

    fun realizeView {env as InEnv{co=myco,...}, win, sz} (geom : geometry) = let
      val my_win = win
      val filtChan = channel ()
      val (my_inenv, my_outenv) = createWinEnv ()
      val InEnv{ci=myci,...} = ignoreInput my_inenv
      val r as RECT{x,y,...} = #rect geom

      val crect = childRect (sz, r, boundsOf widget)
      val cwin = wrapCreate(my_win, crect)
      val (cinenv, coutenv as OutEnv{co=childco,...}) = createWinEnv ()

      fun handleCI (CI_Resize r, g as {childSz,...}) = 
          let val g' as {rect,childSz=childSz'} = newSize(r,g) in
            if childSz <> childSz' then resizeWin cwin childSz' else ();
            g'
          end
        | handleCI (_,g) = g

      fun handleCO (CO_ResizeReq,g) = doResizeReq g
        | handleCO (CO_KillReq,g) = (destroyWin cwin; g)

      fun handleReq (Set arg,geom : geometry) =
            (case newOrigin (arg,geom) of
              NONE => (send(repChan, Bad); geom)
            | SOME (r as RECT{x,y,...}) => (
                moveWin cwin (PT{x= ~x,y= ~y});
                send(repChan, Okay);
                {rect=r,childSz= #childSz geom}
              )
            )
        | handleReq (Get, geom) = (send(repChan, Geometry geom); geom)
        | handleReq (_,geom) = geom

      fun loop geom = let
        fun doCI evt = let
          val geom' = handleCI (msgBodyOf evt,geom)
        in
          if geom = geom' then loop geom else changed geom'
        end
      in
        (select [
          wrap (myci, doCI),
          wrap (childco, fn arg => loop(handleCO (arg,geom))),
          wrap (receive filtChan, fn arg => loop(handleReq (arg, geom)))
        ])
      end
      and changed geom =
        (select [
          wrap (transmit(evtChan,geom), fn () => loop geom),
          wrap (myci, fn evt => changed(handleCI (msgBodyOf evt,geom))),
          wrap (childco, fn arg => changed(handleCO (arg,geom))),
          wrap (receive filtChan, fn arg => changed(handleReq (arg, geom)))
        ])

      val SIZE csz = sizeOfRect crect
      val SIZE msz = sz
    in
(*
      print(format "r = %d,%d,%d,%d  child = %d,%d\n"
       [Int x,Int y,Int(#wid msz),Int(#ht msz),Int(#wid csz),Int(#ht csz)]);
*)
      Router.routePair (env, my_outenv, coutenv);
      moveWin cwin (PT{x= ~x,y= ~y});
      realizeFn widget {
        env = cinenv, 
        win = cwin,
        sz = sizeOfRect crect
      };
      spawn(fn () => filter(receive reqChan, filtChan));
      mapWin cwin;
      changed {rect=mkRect(PT{x=x,y=y},sz),childSz=sizeOfRect crect}
    end

    fun initGeom () = let
      val (cwid,cht) = naturalSz(boundsOf widget)
    in
      {rect=RECT{x=0,y=0,wid=cwid,ht=cht},childSz=SIZE{wid=cwid,ht=cht}}
    end

    fun initLoop (geom : geometry) = let
      fun update ({horz,vert}) = let
        val RECT{x,y,...} = #rect geom
        val x' = case horz of SOME h => h | _ => x
        val y' = case vert of SOME v => v | _ => y
        val (cwid,cht) = naturalSz(boundsOf widget)
      in
        if x' < cwid andalso y' < cht then (
          send(repChan, Okay); 
          {rect=RECT{x=x',y=y',wid=cwid-x',ht=cht-y'},
           childSz=SIZE{wid=cwid,ht=cht}}
        )
        else (
          send(repChan, Bad); 
          geom
        )
      end
    in
      case accept reqChan of
        Set arg => initLoop (update arg)
      | Get => (send(repChan, Geometry geom); initLoop geom)
      | DoRealize arg => realizeView arg geom
    end
  in
    spawn (fn () => initLoop (initGeom ()));
    VP {
      widget = mkWidget{
        root = root, 
        boundsOf = viewBnds (boundsFn widget),
        realize = fn arg => send (reqChan, DoRealize arg)
      },
      evtc = receive evtChan,
      reqc = reqChan,
      repc = repChan
    }
  end

  fun widgetOf (VP{widget,...}) = widget

  fun chkValue (_,Okay) = ()
    | chkValue (f,_) = LibBase.badArg{
                         module="Viewport",
                         func=f,
                         msg="value causes invalid rectangle"
                       }

  fun setHorz (VP{reqc,repc,...}) arg = (
    send (reqc,Set{horz=SOME arg,vert=NONE}); 
    chkValue("setHorz",accept repc)
  )

  fun setVert (VP{reqc,repc,...}) arg = (
    send (reqc,Set{vert=SOME arg,horz=NONE}); 
    chkValue("setVert",accept repc)
  )

  fun setOrig (VP{reqc,repc,...}) (PT{x,y}) = (
    send (reqc,Set{vert=SOME y,horz=SOME x}); 
    chkValue("setOrig",accept repc)
  )

  fun getGeometry (VP{reqc,repc,...}) = (
    send (reqc,Get); 
    case (accept repc) of 
      Geometry g => g 
    | _ => raise LibBase.Impossible "Viewport.getGeometry"
  )

  fun evtOf (VP{evtc,...}) = evtc

end (* Viewport *)

