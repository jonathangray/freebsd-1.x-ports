signature MENU_BUTTON = 
  sig
    structure W : WIDGET
    structure CML : CONCUR_ML
    structure Menu : SIMPLE_MENU

    val mkMenuButton : W.root -> (string * '1a Menu.menu)
	  -> (W.widget * '1a CML.event)
  end

structure MenuButton : MENU_BUTTON = 
  struct

    structure W = Widget
    structure CML = CML
    structure Menu = SimpleMenu

    open CML Geometry Widget Interact

    fun mkMenuButton root (label, menu) = let
      val wChan = channel () and rChan = channel ()
      val view = ButtonView.mkText root {align=HCenter,label=label,
        backgrnd=NONE,foregrnd=NONE}
      val allBttns = map MButton [1,2,3,4,5]
      val bttn = ToggleCtrl.mkToggle root (fn _ => (), Active false, view)
      fun prefn () = ToggleCtrl.setState (bttn,true)
      fun postfn () = ToggleCtrl.setState (bttn,false)
      fun query arg = (send(wChan, arg); accept rChan)

      fun where (Menu.WI{scr_pt=PT{x=sx,y=sy}, pt=PT{x,y}, time, but},SIZE{ht,...}) = 
        Menu.Absolute(PT{x=sx-x,y=sy-y+ht+1})

      val (widget, evt) = 
        SimpleMenu.buttonMenu (Toggle.widgetOf bttn, allBttns, menu, query)

      fun menuRealize {win, sz, env} = let
        val InEnv{m,ci,...} = env
        val mChan = channel() and cChan = channel ()
 
        fun handleMouse msg = (
          case msgBodyOf msg of
            MOUSE_FirstDown _ => prefn ()
          | MOUSE_LastUp _ => postfn ()
          | _ => ();
          send(mChan,msg))

        fun handleCI (msg,sz) =
          case msgBodyOf msg of
            CI_Resize (RECT{wid,ht,...}) => 
              (send(cChan,msg);SIZE{wid=wid,ht=ht})
          | _ => (send(cChan,msg);sz)

        fun loop sz = loop(select [
            wrap (m, fn evt => (handleMouse evt; sz)),
            wrap (ci, fn evt => handleCI(evt,sz)),
            wrap (receive wChan, fn msg => (send(rChan,where(msg,sz));sz))
          ])
        in
          spawn (fn () => loop sz);
          realizeFn widget {
            win=win,
            sz=sz,
            env=replaceCI(replaceMouse(env,receive mChan),receive cChan)
          }
        end

      val menuWidget = mkWidget {
          root = root,
          realize = menuRealize,
          boundsOf = boundsFn widget
        }
      in
        (menuWidget, evt)
      end
  end


