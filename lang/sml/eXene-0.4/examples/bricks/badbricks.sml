
structure BadBricks : sig val badBricks : string -> unit end =
  struct

    open CML Geometry Widget BrickUtil

    val XSize = 10
    val YSize = 30

    fun main server = let
      val root = mkRoot server
      val wall = Wall.mkWall root (XSize, YSize)
      fun doGC () = System.Unsafe.CInterface.gc 2
      fun quitGame () = (delRoot root; RunCML.shutdown())
      fun doShortRange () = Wall.setRange(wall, Short)
      fun doLongRange () =
        if cmpDifficulty (Wall.difficultyOf wall,Hard) > 0 
          then Wall.setRange(wall, Long) 
          else ()
      fun doGame d = (
        Wall.startGame (wall,d);
        (* if d > Hard then activate sensorMenu *)
        ())

      fun gameMenu() = let
        open SimpleMenu
        fun mkItem d = MenuItem(difficultyName d, fn () => doGame d)
        in
          MENU((map mkItem difficultyList)@[MenuItem("GC",doGC),MenuItem("Quit", quitGame)])
        end 

      fun sensorMenu () = let
        open SimpleMenu
        in
          MENU [
              MenuItem("Short range", doShortRange),
              MenuItem("Long range", doLongRange)
            ]
        end 

      val (gameMenuBttn, gameMenuEvt) = 
        MenuButton.mkMenuButton root ("Game", gameMenu())

      fun loop () = loop((sync gameMenuEvt) ())
      val layout = Box.mkLayout root (Box.VtCenter [
          Box.HzTop [
            Box.WBox (Shape.mkRigid gameMenuBttn), 
            Box.Glue {nat=0,min=0,max=NONE}
          ],
          Box.WBox (Divider.mkHorzDivider root {color=NONE,width=1}),
          Box.WBox (Wall.widgetOf wall)
        ])
      val shell = Shell.mkShell (Box.widgetOf layout, NONE,
        {win_name = SOME "BadBricks", icon_name = SOME "BadBricks" })

      in
        Shell.init shell;
        Wall.startGame(wall,Normal);
        loop ()
      end

    fun badBricks server = RunCML.doit(fn () => main server, SOME 20)

  end (* structure BadBricks *)

fun doit s = BadBricks.badBricks s

fun main (prog::server::_,_) = BadBricks.badBricks server
  | main _ = BadBricks.badBricks ""
