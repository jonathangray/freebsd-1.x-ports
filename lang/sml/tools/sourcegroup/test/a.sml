structure A = struct

fun a name =
  let val cwd = Pathname.getwd ()
      val pathname = Pathname.absoluteName cwd name
      fun opr dir file filetype =
        if substring(file,0,1) = "." then () else
          print ((Pathname.mergePathnames [dir, file])^"\n")
  in
    DirFile.scan opr [] pathname
  end
end
