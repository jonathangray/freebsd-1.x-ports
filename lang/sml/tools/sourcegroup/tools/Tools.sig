(* Copyright (c) 1992 by Carnegie Mellon University *)

signature TOOLDEF = sig
  val toolName            :string
  val processor           :string ref
  val targetNamer         :(string -> string) ref
  val validTarget         :SourceGroup.group -> string -> string -> bool
  val loadSource          :SourceGroup.group -> string -> string -> unit
  val genTarget           :SourceGroup.group -> string -> string -> unit
  val loadTarget          :SourceGroup.group -> string -> string -> unit
  val compileSource       :SourceGroup.group -> string -> string -> unit
  val checkLoad           :SourceGroup.group -> string -> string -> unit
end

signature TOOL = sig
  val toolName          :string
  val processor         :string ref
  val targetNamer       :(string -> string) ref
  val targetNameOf      :string -> string
  val validTarget       :SourceGroup.group -> string -> string -> bool
  val loadSource        :SourceGroup.group -> string -> string -> unit
  val genTarget         :SourceGroup.group -> string -> string -> unit
  val loadTarget        :SourceGroup.group -> string -> string -> unit
  val compileSource     :SourceGroup.group -> string -> string -> unit
  val checkLoad         :SourceGroup.group -> string -> string -> unit
end
