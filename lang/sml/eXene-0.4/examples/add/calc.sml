signature CALC =
  sig
    structure CML : CONCUR_ML
    structure W : WIDGET

    datatype answer = Right | Wrong
    datatype difficulty = Easy | Medium | Hard
    datatype function = Add | Subtract | Multiply
    val functionList : (function * bool) list
    val funcString : function -> string

    type calc

    val mkCalc : W.root -> calc
    val startGame : calc -> (difficulty * function) -> unit
    val reset : calc -> unit
    val widgetOf : calc -> W.widget
    val eventOf : calc -> answer CML.event

  end

structure Calc : CALC =
  struct
    structure W = Widget

    open CML Geometry W Box
    
    val fontname = 
      "-b&h-lucidatypewriter-bold-r-normal-sans-24-240-75-75-m-140-iso8859-1"
      (* "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1" *)

    datatype answer = Right | Wrong
    datatype difficulty = Easy | Medium | Hard
    fun diffString Easy = "Easy"
      | diffString Medium = "Medium"
      | diffString Hard = "Hard"
    val diffList = [Easy,Medium,Hard]
    datatype function = Add | Subtract | Multiply
    fun funcString Add = " +"
      | funcString Subtract = " -"
      | funcString Multiply = " x"
    fun funcOp Add = Integer.+
      | funcOp Subtract = Integer.-
      | funcOp Multiply = Integer.*
    val functionList = [(Add,true), (Subtract,false), (Multiply,false)]

    datatype rqst = Start of (difficulty * function) | Reset

    datatype calc = CALC of {
      widget : widget,
      reqChan : rqst chan,
      answerEvt : answer event
    }

    fun fixVert w = let
      val SIZE{ht,...} = natSize w
      val ydim = fixDim ht
      fun bndfn bounds_of = let
        val {x_dim,y_dim} = bounds_of ()
        in
          {x_dim=x_dim,y_dim=ydim}
        end
      in
        Shape.mkShape {
          widget=w, 
          bounds_fn = bndfn,
          resize_fn = fn _ => true
        }
      end

    fun getSeed () = let
      val TIME{sec,...} = System.Unsafe.CInterface.gettimeofday()
      in
        real sec
      end

    fun genVals (random,d) = let
      val maxrange = 
        case d of
          Easy => 99
        | Medium => 999
        | Hard => 9999

      fun gen () = let
        val v1 = Random.range (1,maxrange) (random())
        val v2 = Random.range (1,maxrange) (random())
        in
          if v1 < v2 then (v2,v1) else (v1, v2)
        end
      in
        gen
      end

    fun doInput (kbd,label,anschan) = let
      open Interact
      val lookup = lookupString defaultTranslation
      fun isErase c = c = "\^H"
      fun isNewline c = (c = "\^M") orelse (c = "\^J")

      fun addDigit (c,s) = let
        val s' = c^s
        in
          Label.setLabel label s';
          s'
        end

      fun erase "" = ""
        | erase s = let
          val s' = substring(s,1,size s - 1)
          in
            Label.setLabel label s'; s'
          end

      val (kbdevt,_) = sync kbd
      fun restart cv = let
        fun handleKbd(KEY_Press key,s) = (let
            val c = lookup key
            in
              if isErase c then erase s
              else if isNewline c andalso size s > 0 then (
                (writeVar(cv,StringCvt.atoi s)) handle _ => writeVar(cv,0);
                initLoop())
              else if CType.isDigit (c,0) then addDigit(c,s)
              else s
            end handle _ => s)
              
          | handleKbd(_,s) = s
            
        fun loop s = 
          select[
            wrap(receive anschan, restart),
            wrap(kbdevt, fn k => loop(handleKbd(msgBodyOf k,s)))
          ]
      in
        Label.setLabel label "";
        loop ""
      end

      and initLoop () = 
        select[
          wrap(receive anschan, restart),
          wrap(kbdevt, fn _ => initLoop())
        ]
      in
        initLoop ();
        ()
      end

    fun mkCalc root = let
      val reqChan = channel ()
      val answerChan = channel ()
      val ansChan = channel ()
      val seed = getSeed ()
      val random = Random.mkRandom seed
      val val1 = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val val2 = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val sign = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "  ",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val answer = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val layout = mkLayout root (HzCenter[
         Glue{nat=10,min=10,max=SOME 20},
         VtCenter [
           WBox (fixVert (Label.widgetOf val1)),
           HzCenter[
             WBox (Shape.mkRigid (Label.widgetOf sign)),
             WBox (fixVert (Label.widgetOf val2))
           ],
           Box.WBox (Divider.mkHorzDivider root {color=NONE,width=2}),
           WBox (fixVert (Label.widgetOf answer))
         ],
         Glue{nat=10,min=10,max=SOME 20}
        ])
      val (layout,kbd) = filterKey (widgetOf layout)

      fun resetAns ans = send(ansChan, ans)

      fun startGame (d,f) = let
        val getVals = genVals (random,d)
        val evalFn = funcOp f
        fun doReq (Start d) = startGame d
          | doReq Reset = calc ()
        fun round () = let
              val (v1,v2) = getVals ()
              val ans = condVar ()
              fun chk v = 
                if evalFn(v1,v2) = v then (send(answerChan,Right);round ()) 
                else (send(answerChan,Wrong); calc ())
              in
                Label.setLabel val1 (makestring v1);
                Label.setLabel val2 (makestring v2);
                resetAns ans;
                select [
                  wrap(receive reqChan, doReq),
                  wrap(readVarEvt ans, chk)
                ]
              end
        in
          Label.setLabel sign (funcString f);
          round ()
        end

      and calc () = let
            fun loop () = 
              case accept reqChan of
                Start d => startGame d
              | Reset => loop ()
            in
              Label.setLabel val1 "";
              Label.setLabel val2 "";
              Label.setLabel answer "";
              loop ()
            end
      in
        spawn (fn () => doInput(kbd,answer,ansChan));
        spawn calc;
        CALC{
          widget = layout,
          reqChan = reqChan,
          answerEvt = receive answerChan
        }
      end

    fun startGame (CALC{reqChan,...}) d = send(reqChan,Start d)
    fun reset (CALC{reqChan,...}) = send(reqChan,Reset)
    fun widgetOf (CALC{widget,...}) = widget
    fun eventOf (CALC{answerEvt,...}) = answerEvt

  end
