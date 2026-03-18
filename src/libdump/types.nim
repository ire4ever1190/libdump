## Utilities for working with types

import ./macros

import std/options

macro callForEachField(inp: typedesc, fun: untyped): typedesc =
  ## Calls `func` on each field creating a new object
  let decl = inp.getObjectDecl()
  if decl.isNone:
    echo inp.treeRepr
    "Can't find declaration".error(inp)
  proc rec(node: NimNode): NimNode =
    ## Recursive function to rebuild a type but with different fields
    case node.kind
    of nnkIdentDefs:
      return nnkIdentDefs.newTree(node[0], newCall(ident"mapperTmpl", node[1]), node[2])
    of nnkTupleConstr:
      # All we have are types, just map them
      result = nnkTupleConstr.newTree()
      for son in node:
        result &= newCall(ident"mapperTmpl", son)
    else:
      if node.canHaveSons:
        result = node.kind.newTree()
        for son in node:
          result &= rec(son)
      else:
        return node
  result = rec(decl.get())

template transformFields*(inputObj: typedesc, mapper: untyped): typedesc =
  ## Transforms the fields in an object to create a new type.
  ## `mapper` is passed an implicit "inp" variable which contains the type
  runnableExamples:
    type Foo = object
      a: string
      b: int

    type Transformed = transformFields(Foo, seq[inp])

    echo Transformed(a: @["hello"], b: @[1])

  # Inner template to have some kind of "callable"
  template mapperTmpl(inp {.inject.}: untyped): typedesc =
    mapper

  callForEachField(inputObj, mapperTmpl)

type OptionalFields[T] = transformFields(T, Option[T])
  ## Converts every field in the type to be optional
