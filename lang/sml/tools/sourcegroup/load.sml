(* Copyright (c) 1992 by Carnegie Mellon University *)
val print'sigs = !System.Control.Print.signatures;
System.Control.Print.signatures := 0;

use "../tools/sourcegroup/local/System/getwd.sml";
val cwd = GetWorkingDirectory.getwd();
System.Directory.cd "../tools/sourcegroup";

use "load-all.sml";

System.Directory.cd cwd;
System.Control.Print.signatures := print'sigs;
