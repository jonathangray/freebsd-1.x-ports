loadEntry "Global";
loadEntry "Memo";
loadSig "MAKE";

(* MAKE SYSTEM

Created by:     Nick Rothwell, LFCS, University of Edinburgh
                nick@lfcs.ed.ac.uk
Date:           30 Oct 1990

                Modified to fit the library structure by Dave Berry,
                24 Jan 1991.

Maintenance:    Author


DESCRIPTION

   This is the main part of the make system.


RCS LOG

$Log: Make0.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.6  91/09/13  16:39:46  16:39:46  db (Dave Berry)
Added semi-colons to separate declarations in temp file.

Revision 1.5  91/02/20  16:41:31  16:41:31  db (Dave Berry)
Added uncompiled function; fixed consultDecl and consultFile to touch
all entries they find.

Revision 1.4  91/01/30  18:59:29  18:59:29  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/25  20:13:27  20:13:27  db (Dave Berry)
Prefixed local signature and functor names with MAKE_ or Make respectively.

Revision 1.2  91/01/25  20:02:44  20:02:44  db (Dave Berry)
Changed some occurrences of CoreUtils to Global, to reflect the transfer
of some functions between the two.

Revision 1.1  91/01/25  11:41:37  11:41:37  db (Dave Berry)
Initial revision


*)

functor Make0(structure CoreUtils: CORE_UTILS
	      structure Global: MAKE_GLOBAL
              structure Busy: MAKE_BUSY
              structure Patches: sig
				   val Trace: bool -> unit
				   val Use: string -> unit
                                 end
             ): MAKE =

   struct
      val version = "$Revision: 1.1 $"

      datatype Timestamp = NOTCOMPILED   |
                           COMPILED of int

      structure Memo = MakeMemo(structure CoreUtils = CoreUtils
			    structure Global = Global
			    structure Busy = Busy
			    type Arg = string
			   )

      structure StringSet =
        MonoSet (
          structure Element =
            struct
              type T = string
              fun eq s s' = (s = s')
            end
        );

      structure StringxStringSetSet =
        MonoSet (
          structure Element =
            struct
              type T = string * StringSet.Set
              fun eq (s, set) (s', set') =
                (s = s') andalso StringSet.eq set set'
            end
        );


      open Patches
      open Global

      exception FindResidence of string
                and Locate of string * string
                and Bug of string
                and Make

      val TEMPFILE = ref "%Make.tmp%"
      fun setTempfile filename = (TEMPFILE := filename)

      val LastInput = (ref ""): string ref

      fun P(s) = Busy.println("   [" ^ s ^ "]")

      fun StripNewline(str) = Global.substring(str, 0, size str - 1)

      fun Complain(s) = (Busy.println("Make: " ^ s);
                         raise Make
                        )

      fun bug(s) = (Busy.println(">>> Bug(" ^ s ^ ")");
                    raise Bug(s)
                   )

      fun InputFails() =
         Complain("I couldn't open file \"" ^ !LastInput ^ "\" for input.")

      fun Handler(Suspension: unit -> unit) =
         Suspension()
         handle
            FindResidence(x) =>
               Complain("I don't know where " ^ x ^ " lives.")   |
            Locate(t, f) =>
               Complain("I can't find " ^ t ^ " in " ^ f ^ ".")   |
            Io "open_in" => InputFails()

      val ResideState = (ref nil): (string * string) list ref

      fun resides(Tag, Filename) =
         ResideState := (Tag, Filename) :: !ResideState

      fun findResidence(Tag: string) =
         let fun findResidence'(Tag: string, (Tag', Filename) :: Rest) =
                    if Tag = Tag' then Filename
                    else findResidence'(Tag, Rest)
               | findResidence'(Tag, nil) =
                    raise FindResidence(Tag)

         in  findResidence'(Tag, !ResideState)
         end

      fun whereIs(Tag) =	(* The one which appears in MAKE signature *)
         findResidence(Tag)
         handle FindResidence(_) =>
            Complain("I don't know where " ^ Tag ^ " lives.")

      fun Open(f) =
         (LastInput := f;
          case f of "" => raise Io "open"
                 |  _ =>  open_in(f)
         )

      datatype CharClass = LETTER | DIGIT | SYMBOL | COLON | BLANK
      fun Class(Ch) =
         if Ch >= "a" andalso Ch <= "z" then LETTER
         else if Ch >= "A" andalso Ch <= "Z" then LETTER
         else if Ch = "_" then LETTER
         else if Ch = "'" then LETTER
         else if Ch = ":" then COLON
         else if Ch >= "0" andalso Ch <= "9" then DIGIT
         else if Ch <= " " then BLANK
         else SYMBOL

	(* Stuff for finding tags within files *)
      val TagEscape = "(*$"

      fun Begins(Line: string, Chars: string) =
         let val l = size(Chars)
         in  size(Line) >= l
             andalso Global.substring(Line, 0, l) = Chars
         end

      fun AnyTag(Line) = Begins(Line, TagEscape)

      fun Alphanumeric(Ch) =
         case Class(Ch) of LETTER => true   |
                           DIGIT  => true   |
                           _ => false

			(* MatchesTag: checks for line being \(\*\$<tag>,
			   *without* a following letter/digit *)
      fun MatchesTag(Line, Tag) =
         let val TagStuff = TagEscape ^ Tag
             val TagStuffLen = size(TagStuff)

         in  if size(Line) > TagStuffLen
                andalso Alphanumeric(Global.substring(Line, TagStuffLen, 1))
             then false
             else Begins(Line, TagStuff)
         end

      abstype Cache = CACHE of (string * bool ref * string list) list ref
      with
         local
            val Cache = CACHE(ref nil)

            fun Insert(tag, thing, (this as (tag', _, _)) :: rest) =
                   if tag = tag' then
                      (tag, ref true, thing) :: rest
                   else
                      this :: Insert(tag, thing, rest)
              | Insert(tag, thing, nil) = [(tag, ref true, thing)]

            fun Delete(tag, (this as (tag', _, _)) :: rest) =
                   if tag = tag' then rest
                   else this :: Delete(tag, rest)
              | Delete(_, nil) = nil

            fun SetFlag(tag, b, (tag', b', _) :: rest) =
                   if tag = tag' then b' := b
                   else SetFlag(tag, b, rest)
              | SetFlag(_, _, nil) = ()

            fun locate(tag, (tag', ref visible, thing) :: rest) =
                   if tag = tag' andalso visible then Global.SOME thing
                   else locate(tag, rest)
              | locate(_, nil) = Global.NONE

            fun UnwrapCache() = case Cache of CACHE(ref c) => c
            fun AssignCache c = case Cache of CACHE c' => (c' := c)

         in
            fun CacheInsert(tag, thing) =
               AssignCache(Insert(tag, thing, UnwrapCache()))

            fun CacheDelete tag = AssignCache(Delete(tag, UnwrapCache()))
            fun CacheHide   tag = SetFlag(tag, false, UnwrapCache())
            fun CacheReveal tag = SetFlag(tag, true, UnwrapCache())
            fun CacheLocate tag = locate(tag, UnwrapCache())
            fun WipeCache() = AssignCache nil
         end
      end

      fun FindObjectInFile(Tag: string, Filename: string) =
         let val Filestream = Open(Filename)

             fun locate(): string =
			(* Returns matching tag line. IMPORTANT, since it
			   may start a multi-line comment *)
                case Global.inputLine(Filestream) of
                   "" => (close_in(Filestream);
                          raise Locate(Tag, Filename)
                         )  |
                   s => if MatchesTag(s, Tag) then s else locate()

             fun Consume() =
                case Global.inputLine(Filestream) of
                   "" => nil   |
                   s => if AnyTag(s) then nil else s :: Consume()

             val TagLine = locate()
             val LineList = Consume()
         in
            close_in(Filestream);
            TagLine :: LineList
         end

      fun FindObject(tag, filename) =
		(* Find object 'tag', in cache if possible. If it isn't there,
		   then extract from the file and cache it for next time.
		 *)
         case CacheLocate tag of
            Global.SOME thing =>
               (P("Make: " ^ tag ^ " in cache");
                thing
               )   |
            Global.NONE =>
               let val thing = FindObjectInFile(tag, filename)
               in
                  (P("Make: " ^ tag ^ " extracted from " ^ filename);
                   CacheInsert(tag, thing);
                   thing
                  )
               end


	(* Stuff to filter out code specific to old and new ML compilers.
	   accepts lines like
		(*---OLD---*)
		xxx
		(*---NEW---*)
		yyy
		(*---------*)
	   and filters accordingly. Assign Version according to what you want.
	 *)

      datatype MLVersion = OLD | NEW
      val Version = ref NEW

      fun old_version() = (Version := OLD)
      fun new_version() = (Version := NEW)

	(* Line classification in terms of THIS and THAT (i.e. what we want
	   vs. what we don't). If Version is OLD, then THIS is ---OLD--- and
	   THAT is ---NEW---, otherwise the other way round.
	 *)

      datatype LineType = LineTHIS | LineTHAT | LineEND | LineOTHER of string
      fun ClassifyLine "(*---OLD---*)\n" =
             (case !Version of OLD => LineTHIS | NEW => LineTHAT)   |
          ClassifyLine "(*---NEW---*)\n" =
             (case !Version of OLD => LineTHAT | NEW => LineTHIS)   |
          ClassifyLine "(*---------*)\n" = LineEND   |
          ClassifyLine l = LineOTHER l

      fun FilterClassifiedLines(ls, printing) =
             case ls of
                LineTHIS :: rest => FilterClassifiedLines(rest, true)   |
                LineTHAT :: rest => RemoveClassifiedLines(rest)   |
                LineEND :: rest  => FilterClassifiedLines(rest, false)   |
                (other as LineOTHER(s)) :: rest    =>
                   (if printing then Busy.print("--> " ^ s) else ();
                    other :: FilterClassifiedLines(rest, printing)
                   )   |
                nil => nil

          and RemoveClassifiedLines(ls) =
             case ls of
                LineTHIS :: rest => FilterClassifiedLines(rest, true)   |
                LineTHAT :: rest => RemoveClassifiedLines(rest)   |
                LineEND :: rest  => FilterClassifiedLines(rest, false)   |
                LineOTHER(s) :: rest    =>
                   (Busy.print("<-- " ^ s);
                    RemoveClassifiedLines(rest)
                   )   |
                _ => bug "RemoveClassifiedLines"

      val FilterClassifiedLines = fn ls => FilterClassifiedLines(ls, false)

      exception UNCLASSIFYLINE
      fun UnclassifyLine(LineOTHER(l)) = l   |
          UnclassifyLine(_) = raise UNCLASSIFYLINE

      val FilterLines =
         (map UnclassifyLine) o FilterClassifiedLines o (map ClassifyLine)

      fun Enclose(Lines, Tag) =
         let val Header = ";\nval _ = Make.compiling_ \"" ^ Tag ^ "\";\n"
             val Trailer = ";\nval _ = Make.OK_ \"" ^ Tag ^ "\";\n"

         in  Header :: rev(Trailer :: rev(Lines))
         end

      fun FindText(Tag, Diags) =
         let val Where = findResidence(Tag)
             val Text = if Diags then Enclose(FindObject(Tag, Where), Tag)
                                 else FindObject(Tag, Where)
         in
             FilterLines(Text)
         end

      val DependencyState = (ref StringxStringSetSet.empty)

      fun FindDependencies(Tag) =
         let fun FindDependencies'(Tag, DepSet) =
            if StringxStringSetSet.isEmpty(DepSet) then
                 StringSet.empty
            else let val ((Tag', Deps), Rest) =
		       StringxStringSetSet.select(DepSet)
                 in  if Tag = Tag' then Deps
                     else FindDependencies'(Tag, Rest)
                 end

         in  FindDependencies'(Tag, !DependencyState)
         end

      fun RemoveDependencies(Tag, State) =
         if StringxStringSetSet.isEmpty(State) then StringxStringSetSet.empty
         else let val ((This as (Tag', _)), xRest) =
		    StringxStringSetSet.select(State)
              in  if Tag = Tag' then xRest
                  else StringxStringSetSet.insert This
					(RemoveDependencies(Tag, xRest))
              end

     (* Timestamp stuff *)
      local val Clock = ref 0
            val TimestampState = (ref nil): (string * Timestamp ref) list ref

		(* TimestampOf() - returns a timestamp reference, generating
		   and inserting one into the current state if none present *)
            fun TimestampOf'(Tag, existing, (Tag', t) :: Rest) =
                   if Tag = Tag' then t else TimestampOf'(Tag, existing, Rest)

              | TimestampOf'(Tag, false, nil) =	(* It's allowed not to exist *)
                   let val p as (_, t) = (Tag, ref(NOTCOMPILED))
                   in  TimestampState := p :: !TimestampState;
                       t
                   end

              | TimestampOf'(Tag, true, nil) =	(* Error: we're expecting it
						   to exist *)
                   Complain("I don't know about " ^ Tag)

      in    fun Tick() = (Clock := !Clock + 1)
            fun ThePresent() = !Clock
            fun ResetClock() = (Clock := 0)
            fun TimestampOf(Tag) = TimestampOf'(Tag, false, !TimestampState)
            fun ExistingTsOf(Tag) = TimestampOf'(Tag, true, !TimestampState)
            fun ClearAllStamps() = (TimestampState := nil)

            fun NeverCompiledTags() =
              let
                fun count((tag, ref NOTCOMPILED) :: rest) = tag :: count rest
                  | count((_, ref(COMPILED _)) :: rest) = count rest
                  | count nil = nil
              in
                count(!TimestampState)
              end
      end

      fun touch' Tag =
	(whereIs Tag;			(* Make sure we know about it. *)
	 TimestampOf(*ExistingTsOf*)(Tag) := NOTCOMPILED;
	 CacheDelete Tag
        )

      fun touch Tag =
	(touch' Tag;
	 P("Make: " ^ Tag ^ " altered")
        )

      fun OK_(Tag) =
	(Trace(false);
	 TimestampOf(Tag) := COMPILED(ThePresent());
	 CacheReveal Tag;
	 P("Make: " ^ Tag ^ " now OK")
        )

      fun allOK' tags =
         if StringSet.isEmpty tags then ()
         else
            let val (tag, rest) = StringSet.select tags
                val deps = FindDependencies tag
            in
               OK_ tag;
               allOK'(StringSet.union deps rest)
			(* Not optimal, but I can't be bothered... *)
            end

      fun allOK(tags: string list) = allOK'(StringSet.fromList tags)

      fun depends(Tag, Tag') =
         let val Deps = FindDependencies(Tag)
             val NewDeps = StringSet.insert Tag' Deps

         in  (DependencyState :=
                 StringxStringSetSet.insert (Tag, NewDeps)
                        (RemoveDependencies(Tag, !DependencyState))
             )
         end

      fun standsAlone(Tag) =
         DependencyState := RemoveDependencies(Tag, !DependencyState)

	(* Stuff to keep count of number of files/tags/dependencies *)
      local val FileCount = ref 0
            val TagCount = ref 0
            val DepCount = ref 0

            fun PrintCount(1, (word, single, _)) =
                   Busy.print("1 " ^ word ^ single)   |
                PrintCount(n, (word, _, plural)) =
                   Busy.print(CoreUtils.intToString n ^ " " ^ word ^ plural)

      in    fun ResetCounts() =
               (FileCount := 0; TagCount := 0; DepCount := 0)

            fun BumpFileCount()    = (FileCount := !FileCount + 1)
            fun BumpDependencies() = (DepCount := !DepCount + 1)
            fun BumpTagCount()     = (TagCount := !TagCount + 1)
            fun PrintCounts() =
               (Busy.print "Tag information: ";
                PrintCount(!DepCount, ("dependenc", "y", "ies"));
                Busy.print " found involving ";
                PrintCount(!TagCount, ("tag", "", "s"));
                Busy.print " in ";
                PrintCount(!FileCount, ("file", "", "s"));
                Busy.println ""
               )
      end

      fun ReadTokens(c1 :: c2 :: Rest) =
             (case (Class(c1), Class(c2)) of
                 (LETTER, LETTER) => ReadTokens((c1 ^ c2) :: Rest)   |
                 (LETTER, DIGIT)  => ReadTokens((c1 ^ c2) :: Rest)   |
                 (BLANK, _)       => ReadTokens(c2 :: Rest)   |
                 (SYMBOL, SYMBOL) => ReadTokens((c1 ^ c2) :: Rest)   |
                 _ =>                c1 :: ReadTokens(c2 :: Rest)
             )   |
          ReadTokens([c]) = if Class(c) = BLANK then nil else [c]   |
          ReadTokens(nil) = nil

      fun TokeniseText(Line :: Rest) =
             ReadTokens(explode(Line)) @ TokeniseText(Rest)   |
          TokeniseText(nil) = nil

      fun SetDependencies(_, "*)" :: _) = ()   |
          SetDependencies(Tag, t :: Rest) =
             (Busy.print(" " ^ t);
              depends(Tag, t);
              BumpDependencies();
              SetDependencies(Tag, Rest))   |
          SetDependencies(_, nil) = bug("SetDependencies")

      fun consultDecl(Tag) =
         let val _ = CacheDelete Tag
             val Text = FindObject(Tag, findResidence(Tag))

         in  case TokeniseText(Text) of
                "(*$" :: _ :: ":" :: Toks =>
                   (Busy.print("   " ^ Tag ^ " :-");
                    standsAlone(Tag);
                    SetDependencies(Tag, Toks);
		    touch' Tag;
                    Busy.println "."
                   )   |
                "(*$" :: _ :: "*)" :: _ =>
                   (Busy.println("   " ^ Tag ^ ".");
                    standsAlone(Tag);
		    touch' Tag
                   )   |
                _ => bug("consultDecl")
         end

      val consultDecl = fn Tag => Handler(fn () => consultDecl(Tag))

		(* Closing: true if a token list contains \*\) *)
      fun Closing("*)" :: Rest) = true   |
          Closing(_ :: Rest) = Closing(Rest)   |
          Closing(nil) = false

		(* DrainTokens: drain all tokens from a stream, upto and
		   including the line containing \*\) *)
      fun DrainTokens(stream) =
         case Global.inputLine(stream) of
            "" => bug("DrainTokens")   |
            L  => let val Toks = ReadTokens(explode(L))
                  in  if Closing(Toks) then Toks
                      else Toks @ DrainTokens(stream)
                  end

      fun ConsultStream(Filename, stream) =
         let val Line = Global.inputLine(stream)
         in  if Line = "" then ()
             else if AnyTag(Line) then
                let val LineTokens = ReadTokens(explode(Line))
                    val HdrTokens =
                       if Closing(LineTokens) then LineTokens
                       else LineTokens @ DrainTokens(stream)

                in  BumpTagCount();
                    case HdrTokens of
                       "(*$" :: t :: ":" :: Rest =>
                          (Busy.print("   " ^ t ^ " :-");
                           resides(t, Filename);
                           standsAlone(t);
                           SetDependencies(t, Rest);
                           Busy.println ".";
			   touch' t;
                           ConsultStream(Filename, stream)
                          )   |
                       "(*$" :: t :: "*)" :: _ =>
                          (Busy.println("   " ^ t ^ ".");
                           resides(t, Filename);
                           standsAlone(t);
			   touch' t;
                           ConsultStream(Filename, stream)
                          )   |
                       _ => Complain("Bad format in \"" ^
                                     StripNewline(Line) ^ "\".")
                end
             else ConsultStream(Filename, stream)
         end

      fun consultFile'(f) =
         let val x = Open(f)
             val _ = Busy.println("File " ^ f ^ ":")

         in  BumpFileCount();
             ConsultStream(f, x);
             close_in(x)
         end

      val consultFile' = fn f => Handler(fn () => consultFile'(f))
      fun consultFile(f) = (ResetCounts(); consultFile'(f); PrintCounts())

      fun compiling_(Tag) =
         (Trace(true);
          CacheHide Tag;
          P("Make: Compiling " ^ Tag))

      fun Flatten(L) = Global.fold (op @) L nil

      fun Collect(Xp :: XRest) =
             let fun Contained(X as (Xt, _), (Yt, _) :: YRest) =
                        Xt = Yt orelse Contained(X, YRest)   |
                     Contained(_, nil) = false

             in  if Contained(Xp, XRest) then Collect(XRest)
                                         else Xp :: Collect(XRest)
             end   |
          Collect(nil) = nil

      fun GenMakeTags forReal =
         let val F = ref(fn _ => let exception e in raise e end)
                     : (string -> (string * Timestamp) list) ref

             fun MakeTags(MyTag): (string * Timestamp) list =
                let val MyStamp = !(TimestampOf(MyTag))
                    val DepSet = FindDependencies(MyTag)
                    val DepList = StringSet.list(DepSet)
                    fun DepStamps() = map ((!) o TimestampOf) DepList
			(* only work out DepStamps if required... *)

                  (*fun StrPair(Tag, NOTCOMPILED) = Tag ^ "(nc)"   |
                        StrPair(Tag, COMPILED(t)) =
                           Tag ^ "(c@" ^ makestring(t) ^ ")"*)

                    fun StrPair(Tag, _) = Tag

                    fun app(x, "") = StrPair(x)   |
                        app(x, y)  = StrPair(x) ^ ", " ^ y

                    val SubPairs = Collect(Flatten(map (!F) DepList))
                    val (SubMakeTags, SubStamps) = CoreUtils.unzip(SubPairs)
			(* SubMakeTags are all tags to be rebuilt before Tag,
			   SubStamps are their timestamps *)

                    fun OutOfDate(t, COMPILED(t') :: tRest) =
                           t < t' orelse OutOfDate(t, tRest)   |
                        OutOfDate(t, NOTCOMPILED :: _) =
                           true   |
                        OutOfDate(t, nil) =
                           false

                    fun PrintInfo() =
                       P("Make: " ^ StrPair(MyTag, MyStamp)
                         ^ " :- " ^ Global.fold app SubPairs "")

	(* If not forReal (ie. if a makeTask), then *everything* gets made: *)
                in  case SubMakeTags of
                       nil =>	(* None of my dependencies need building,
				   but do I? *)
                          (case MyStamp of
                              NOTCOMPILED => [(MyTag, MyStamp)]   |
                              COMPILED(t) =>
                                 if not forReal then
                                    [(MyTag, MyStamp)]
                                 else if OutOfDate(t, DepStamps()) then
                                    [(MyTag, MyStamp)]
                                 else nil
                          )   |
                       Ts =>	(* Some of my dependencies need building,
				   therefore so do I *)
                          (PrintInfo();
                           (MyTag, MyStamp) :: SubPairs
                          )
                end

             val MakeTags' = Memo.memo_fn (op =) MakeTags
             val _ = (F := MakeTags')

         in  MakeTags'
         end

      fun DumpTexts(T :: TRest, Str) =
             (map (fn x => output(Str, x)) T;
              DumpTexts(TRest, Str))   |
          DumpTexts(nil, _) = ()

      fun printAlert outStream =
         output(outStream,
	    "(* This \"batch\" file has been generated from the original\n\
	    \   sources by MAKE. If you want to make alterations, make them\n\
	    \   to the originals, and execute Make.makeTask again. *)\n\n"
	 )

      fun make'(Tag, forReal, TempFile, Use): unit =
         let val MakeTags = GenMakeTags forReal
             val MTags = map fst (MakeTags(Tag))
         in  (Trace(false);
              case MTags of
                 nil => P("Make: " ^ Tag ^ " is up to date")   |
                 _ =>
                    let val TagTexts =
                           rev(map (fn x => FindText(x, forReal)) MTags)
                        val OutStream = open_out(TempFile)

                    in  (printAlert OutStream;
				(* Put a "do not edit me" in the output file. *)
                         DumpTexts(TagTexts, OutStream);
                         close_out(OutStream);
                         Tick();
                         Use TempFile
                        )
                    end
             )
         end

      val make' = fn x => Handler(fn () => make'(x))

      val LastTag = ref ""

      fun make(Tag) = (LastTag := Tag; make'(Tag, true, !TEMPFILE, Use))
      fun again() = make(!LastTag)

      fun makeTask(Tag, file) = make'(Tag, false, file, fn _ => ())

      val wipeCache = WipeCache

      fun forgetAll() = (ClearAllStamps(); ResetClock())

      val TagFile = ref ""

      fun loadFrom(file) =	(* open a top-level tag file,
				   do a consultFile on each entry *)
         let val s = Open(file)
             val _ = Busy.print("Reading from tag file " ^ file)
             val _ = TagFile := file

             fun GetNames() =
                case Global.inputLine(s) of
                   "" => nil   |
                   n  => StripNewline(n) :: GetNames()

             val TheNames = GetNames()
             val _ = Busy.println "."
             val _ = close_in(s)

         in  ResetCounts();
             map consultFile' TheNames;
             PrintCounts();
             WipeCache()
         end

      val loadFrom = fn file => Handler(fn () => loadFrom(file))

      fun reload() =
         case !TagFile of
            "" => Complain "Load from where?"    |
            f =>  loadFrom f

      fun uncompiled() =
        let
          val tags = NeverCompiledTags()
        in
          case tags
            of nil => ()
             | _ =>
                 (map (fn t => Busy.print(" " ^ t)) tags;
                  Busy.println "."
                 )
        end

   end;
