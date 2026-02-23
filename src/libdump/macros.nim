import std/[options, macros]

proc getObjectDecl*(typ: NimNode): Option[NimNode] =
  ## Looks through an object to get the declaration of it
  let impl = typ.getTypeImpl()
  case impl.kind
  of nnkObjectTy:
    some impl
  of nnkRefTy:
    some impl[0]
  of nnkSym:
    impl.getObjectDecl()
  of nnkBracketExpr:
    if impl[0].eqIdent(bindSym"typeDesc"):
      # Referencing a type, we need to check what the inner generic param is
      impl[1].getObjectDecl()
    else:
      # Generic instantiation, we want to see what the base type is
      impl[0].getObjectDecl()
  else:
    return none(NimNode)
