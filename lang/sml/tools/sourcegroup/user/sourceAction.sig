(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature SOURCEACTION = sig
  val doLoadSource   :SourceGroup.Expert.sourceInfo -> unit
  val doCompile      :SourceGroup.Expert.sourceInfo -> unit
  val doForceCompile :SourceGroup.Expert.sourceInfo -> unit
  val doClean        :SourceGroup.Expert.sourceInfo -> unit
  val doNothing      :SourceGroup.Expert.sourceInfo -> unit
  val doLoadLibrary  :SourceGroup.Expert.sourceInfo -> unit
  val doPrint        :SourceGroup.Expert.sourceInfo -> unit

  val replaceExtension   :string -> string
  val appendBin          :string -> string
  val systemBinary       :string -> string -> string
  val sysBinary          :string -> string
  val architectureBinary :string -> string

  val showDemandedUses :string -> SourceGroup.group * string -> unit
  val showUses         :string -> SourceGroup.group -> unit
  val showDependencies :string -> SourceGroup.group -> unit

  val showDemandedUses' :outstream -> SourceGroup.group * string -> unit
  val showUses'         :outstream -> SourceGroup.group -> unit
  val showDependencies' :outstream -> SourceGroup.group -> unit
end
