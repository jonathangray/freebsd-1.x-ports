val createGroup = SourceGroup.createInEnv (!Compile.compilingEnv)

val clientGroup = createGroup [SourceGroup.Sources ["a.sml","b.sml"]]

fun m() = (SourceGroup.make clientGroup "B"; Compile.openEnv())
