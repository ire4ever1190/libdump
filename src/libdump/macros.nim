import std/[options, macros, strformat]

export macros

proc getObjectDecl*(typ: NimNode): Option[NimNode] =
  ## Looks through an object to get the declaration of it
  let impl =  typ.getTypeImpl()
  case impl.kind
  of nnkObjectTy, nnkTupleTy, nnkTupleConstr:
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

proc findField*(obj: NimNode, field: string): Option[NimNode] =
  ## Tries to find `field` inside `obj`. `obj` must be an object declaration.
  ## This returns the `identDef` for the field
  case obj.kind
  of nnkObjectTy:
    obj[2].findField(field)
  of nnkRecList, nnkRecCase:
    for identDef in obj:
      let res = identDef.findField(field)
      if res.isSome():
        return res
    none(NimNode)
  of nnkIdentDefs:
    if obj[0].eqIdent(field): some(obj)
    else: none(NimNode)
  of nnkOfBranch: obj[1].findField(field)
  of nnkElse: obj[0].findField(field)
  else:
    echo obj.treeRepr
    none(NimNode)

proc canHaveSons*(inp: NimNode): bool =
  ## Checks whether a node can have sons (i.e. can be safely iterated over).
  ## This does not check if the node DOES have sons
  return inp.kind notin {nnkNone, nnkEmpty, nnkNilLit, nnkCharLit..nnkUInt64Lit, nnkFloatLit..nnkFloat64Lit, nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym}

macro hasDefaultValue*(n: typed): bool =
  ## This checks if a field has a default value.
  runnableExamples:
    type
      MyObj = object
        noDefault: string
        hasDefault = "Hello"
        alsoHasDefault: bool = false
    assert not MyObj.noDefault.hasDefaultValue
    assert MyObj.hasDefault.hasDefaultValue
    assert MyObj.alsoHasDefault.hasDefaultValue

  if n.kind != nnkDotExpr:
    error("Expected parameter to be in `Object.fieldName` format", n)
  let
    obj = n[0]
    field = n[1]
  let objectDecl = obj.getObjectDecl()
  echo objectDecl.get().treeRepr
  if objectDecl.isNone:
    error(fmt"Could not find object declaration for {obj}", obj)

  let fieldDecl = objectDecl.get().findField($field)
  if fieldDecl.isNone:
    error(fmt"Unknown field '{field}' in {obj}")
  echo fieldDecl.get().treeRepr
  return newLit fieldDecl.get()[2].kind != nnkEmpty
