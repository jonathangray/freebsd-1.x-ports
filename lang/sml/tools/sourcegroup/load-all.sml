(* Copyright (c) 1992 by Carnegie Mellon University *)
System.Control.Print.signatures := 0;
use "date.sml";
use "local/System/interrupt.sig";
use "local/System/interrupt.sml";
use "local/System/ioStream.sig";
use "local/System/ioStream.sml";
use "tools/sepcomp.sml";
use "tools/compile.sml";

Compile.compilingEnv :=
  System.Env.filterEnv
    (!System.Env.topLevelEnvRef,
     [System.Symbol.strSymbol "Compile",
      System.Symbol.sigSymbol "COMPILE",
      System.Symbol.strSymbol "Interrupt",
      System.Symbol.sigSymbol "INTERRUPT",
      System.Symbol.strSymbol "IO_Stream",
      System.Symbol.sigSymbol "IO_STREAM"
     ]);

val load = Compile.loadSource;

load "parsing/base.sml";

load "local/System/ioStream.sig";
load "local/System/ioStream.sml";
load "local/System/execute.sig";
load "local/System/execute.sml";
load "local/System/getwd.sml";
load "local/System/pathname.sig";
load "local/System/pathname.sml";
load "local/System/dirFile.sig";
load "local/Container/listSort.sig";
load "local/Container/listSort.sml";
load "local/System/dirFile.sml";
load "local/System/fileList.sig";
load "local/Misc/stringXtra.sig";
load "local/Misc/stringXtra.sml";
load "local/System/fileList.sml";
load "local/System/interrupt.sig";
load "local/System/interrupt.sml";
load "local/System/cshellDir.sig";
load "local/System/cshellDir.sml";
load "local/System/getenv.sig";
load "local/System/getenv.sml";
load "local/Misc/stringFns.sig";
load "local/Misc/stringFns.sml";
load "local/Container/hasher.sig";
load "local/Container/hasherFilenames.sml";
load "local/Container/sortableQueue.sig";
load "local/Container/sortableQueue.sml";
load "local/Container/queue.sig";
load "local/Container/fifo2.sig";
load "local/Container/fifo2.sml";
load "local/Container/queue.sml";
load "local/Container/hasher.sml";
load "local/Container/listFns.sig";
load "local/Container/set.sig";
load "local/Container/listFns.sml";
load "local/Container/hash.sig";
load "local/Container/hash.sml";
load "local/Container/set.sml";

load "analyzer/pervasives.sml";
load "analyzer/moduleNames.sig";
load "analyzer/moduleDecls.sig";
load "analyzer/scopes.sig";
load "analyzer/traverse.sig";
load "analyzer/process.sig";
load "analyzer/parse.sig";
load "analyzer/analyze.sig";
load "analyzer/process.sml";
load "analyzer/scopes.sml";
load "analyzer/moduleNames.sml";
load "analyzer/traverse.sml";
load "analyzer/analyze.sml";
load "analyzer/parse.sml";
load "analyzer/moduleDecls.sml";
load "analyzer/link.sml";

load "src/namespaceTable.sig";
load "src/nameRefTable.sig";
load "src/modtable.sig";
load "src/modtable.sml";
load "src/data.sig";
load "src/util.sig";
load "src/group.sig";
load "src/args.sig";
load "src/args.sml";
load "src/toolInternals.sig";
load "src/sourceGroup.sig";
load "src/connections.sig";
load "src/core.sig";
load "src/create.sig";
load "src/create.sml";
load "src/data.sml";
load "src/auxiliary.sig";
load "src/core.sml";
load "src/util.sml";
load "src/toolInternals.sml";
load "src/sourceGroup.sml";
load "src/namespaceTable.sml";
load "src/nameRefTable.sml";
load "src/group.sml";
load "parsing/absyn.sig";
load "parsing/ConnOperators.sig";
load "parsing/Conn.sig";
load "src/connections.sml";
load "src/auxiliary.sml";
load "parsing/names.sig";
load "parsing/names.sml";
load "parsing/interface.sig";
load "parsing/interface.sml";
load "parsing/errormsg.sig";
load "parsing/errormsg.sml";
load "parsing/conn.grm.sig";
load "parsing/conn.lex.sml";
load "parsing/conn.grm.sml";
load "parsing/ascii.sig";
load "parsing/ascii.sml";
load "parsing/operator.sig";
load "parsing/absyn.sml";
load "parsing/ConnOperators.sml";
load "parsing/Conn.sml";
load "parsing/parsing.link";
load "src/src.link";

load "user/sourceAction.sig";
load "user/sourceAction.sml";

load "tools/Tools.sig";
load "tools/YaccTool.sml";
load "tools/SMLTool.sml";
load "tools/NoOpTool.sml";
load "tools/LexTool.sml";
load "tools/DefineTool.sml";
load "tools/tools.link";

Compile.openEnv ();
