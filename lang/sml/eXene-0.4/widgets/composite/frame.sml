(* frame.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Frame widget, for putting a border around another widget
 *)

signature FRAME = 
  sig

    structure W : WIDGET

    type frame

    val mkFrame : {
          color : W.EXB.color option,
          width : int, 
          widget : W.widget
        } -> frame

    val widgetOf : frame -> W.widget
    val setColor : frame -> W.EXB.color option -> unit

  end (* FRAME *)

structure Frame : FRAME = 
  struct

    structure W = Widget

    datatype frame = Frame of {
        widget : W.widget,
        reqChan : W.EXB.color option CML.chan
      }

    fun mkFrame {color, width, widget} = let
          open CML Geometry Interact W.EXB W.EXW Drawing
          val _ = if width < 0 
                    then LibBase.badArg{module="Frame",func="mkFrame",msg="width < 0"}
                    else ()
          val reqChan = CML.channel ()
          val reqEvt = CML.receive reqChan
          val realizeVar = CML.condVar ()

          fun incBase (W.DIM{base,incr,min,nat,max},extra) =
            W.DIM{base=base+extra,incr=incr,min=min,nat=nat,max=max}

          fun size () = let
                val {x_dim, y_dim} = W.boundsOf widget
                val extra = 2*width
                in
                  {x_dim = incBase(x_dim,extra), y_dim = incBase(y_dim, extra)}
                end

          fun realizeFrame {env as InEnv{co=myco,...}, win, sz} color = let
                val (my_inenv, my_outenv) = createWinEnv ()
                val InEnv{ci=myci,...} = ignoreInput my_inenv

                fun childRect (SIZE{wid,ht}) =
                      (RECT{x=width,y=width,
                       wid=max(0,wid-2*width), ht=max(0,ht-2*width)})

                val crect = childRect sz
                val cwin = W.wrapCreate(win, crect)
                val (cinenv, coutenv) = createWinEnv ()
                val OutEnv{co=childco,...} = coutenv
                val drawable = drawableOfWin win

                fun mkFill NONE = (fn _ => clearDrawable drawable)
                  | mkFill (SOME c) = let 
                      val pen = newPen [PV_Foreground c] 
                      in 
                        fillRect drawable pen
                      end

                fun handleCI (rect,fill) =
                      fn (CI_Resize (RECT{x,y,wid,ht})) => 
                           (moveAndResizeWin cwin 
                             (childRect(SIZE{wid=wid,ht=ht}));
                           main(RECT{x=0,y=0,wid=wid,ht=ht},fill,false))
                       | (CI_Redraw _) => fill rect
                       | _ => ()
  
                and main (rect, fill, update) = let
                      fun handleCO CO_ResizeReq = sync(myco CO_ResizeReq)
                        | handleCO CO_KillReq = destroyWin cwin
                      val handleCI = handleCI (rect,fill)
  
                      fun loop () =
                        select [
                          wrap(reqEvt, fn c => main (rect,mkFill c,true)),
                          wrap (myci, loop o handleCI o msgBodyOf),
                          wrap (childco, loop o handleCO)
                        ]
                    in
                      loop(if update then fill rect else ())
                    end
                  in
                    Router.routePair (env, my_outenv, coutenv);
                    W.realizeFn widget {
                      env = cinenv, 
                      win = cwin,
                      sz = sizeOfRect crect
                    };
                    mapWin cwin;
                    main (mkRect(PT{x=0,y=0},sz),mkFill color,false)
                  end

          fun initLoop color =
                select [
                  wrap(readVarEvt realizeVar, fn arg => realizeFrame arg color),
                  wrap(receive reqChan, fn c =>initLoop c)
                ]
          in
            spawn (fn () => initLoop color);
            Frame {
              widget=W.mkWidget {
                root=W.rootOf widget, 
                boundsOf=size, 
                realize=fn arg => writeVar(realizeVar,arg)
              },
              reqChan = reqChan
             }
          end

    fun widgetOf (Frame{widget,...}) = widget
    fun setColor (Frame{reqChan,...}) color = CML.send(reqChan, color)

  end (* Frame *)

