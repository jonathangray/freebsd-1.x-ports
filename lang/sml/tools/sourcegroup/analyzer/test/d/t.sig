signature SCOPES = sig
 structure MD :MODULE_DECLS

 open A
 include MORE

 type moduleInfo
 type scope

 val init       :unit -> unit
 val topScope   :unit -> scope
 val bindScope  :scope -> scope
 val localScope :scope -> scope
 val plainScope :scope -> scope
 val popScope   :scope -> scope
 val enterRef   :X.R.moduleName * scope -> scope
 val enterDef   :MN.moduleName * scope -> scope
 val lookup     :scope -> MN.moduleName -> moduleInfo option

 val printAll   :outstream -> unit
end
