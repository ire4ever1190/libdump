import std/[options, macros, strformat, sugar]

export macros

# TODO: Make nort generic on NimNode
type Navigator = proc (input: NimNode): Option[NimNode]
  ## A navigator takes a node and then returns a new node

proc idx*(num: int): Navigator =
  ## Tries to access an index into a node
  proc (input: NimNode): Option[NimNode] =
    if num in 0 ..< input.len:
      return some input[num]

proc want*(check: proc (inp: NimNode): bool): Navigator =
  ## Checks that the passed in navigator returns a certain node.
  ## Doesn't continue if the check fails
  proc (input: NimNode): Option[NimNode] =
    if check(input):
      return some input

proc repeat*(navigator: Navigator): Navigator =
  ## Chains `navigator` on itself til it fails.
  ## Returns the final result that passed, matches 0 or more times
  proc (input: NimNode): Option[NimNode] =
    var curr = input
    while true:
      let next = navigator(curr)
      if next.isNone:
        return some curr
      curr = next.get()

proc ofKind*(kind: set[NimNodeKind]): Navigator =
  want(node => node.kind in kind)

proc need*(msg: proc (inp: NimNode): string, check: proc (inp: NimNode): bool): Navigator =
  ## Checks that the passed in naviator returns a certain node.
  ## Errors the node if the check doesn't pass
  proc (input: NimNode): Option[NimNode] =
    result = want(check)(input)
    if result.isNone:
      msg(input).error(input)

proc chain*(navigators: varargs[Navigator]): Navigator =
  ## Chains a series of navigators so they run one after another
  let gators = @navigators
  proc (input: NimNode): Option[NimNode] =
    var curr = input
    for navigator in gators:
      let next = navigator(curr)
      if next.isNone: return none(NimNode)
      curr = next.get()
    some curr

proc path*(path: openArray[tuple[idx: int, kinds: set[NimNodeKind]]]): Navigator =
  ## Works its way through a series of indexs, checking each kind before progressing
  var checks: seq[Navigator]
  for (i, kind) in path:
    checks &= idx(i)
    checks &= ofKind(kind)
  return chain(checks)

proc getObjectDecl*(typ: NimNode): Option[NimNode] =
  ## Looks through an object to get the declaration of it
  let impl =  typ.getTypeImpl()
  case impl.kind
  of nnkObjectTy, nnkTupleTy, nnkTupleConstr, nnkEnumTy:
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

proc skipPast*(inp: NimNode, skip: set[NimNodeKind]): NimNode =
  ## Skips past anything in `skip` by recursing into the first child
  result = inp
  while result.kind in skip:
    result = result[0]

proc isPublic*(inp: NimNode): bool =
  ## Tells whether a node is public or not
  let name = inp.skipPast({nnkPragmaExpr, nnkIdentDefs})
  name.kind == nnkPostFix and name[0].eqIdent("*")

proc public*(inp: NimNode): NimNode =
  ## Makes a `NimNode` public.
  ## Does nothing if already public
  if inp.isPublic: return inp
  case inp.kind
  of nnkIdentDefs:
    result = inp
    result[0] = result[0].public
  of nnkIdent, nnkSym:
    result = nnkPostFix.newTree(ident"*", inp)
  else:
    "Can't make this public".error(inp)

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
