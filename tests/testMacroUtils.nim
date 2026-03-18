# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import std/[unittest, options, macros]

import libdump/macros

suite "Get object decl":
  type
    Person = object
      name: string

    RefPerson = ref Person

    GenericObj[T] = object
      value: T

    Alias = GenericObj[int]

    Tuple = tuple[a: int, b: string]

  macro getObject(x: typedesc): bool =
    return newLit(getObjectDecl(x).isSome())

  test "Normal object":
    check getObject(Person)

  test "Reference type":
    check getObject(RefPerson)

  test "Generic object":
    check getObject(GenericObj[string])

  test "Generic alias":
    check getObject(Alias)

  test "Tuple":
    check getObject(Tuple)

suite "Check field default":
  type
    MyObj = object
      noDefault: string
      hasDefault = "Hello"

  test "Field with no default":
    check not MyObj.noDefault.hasDefaultValue


  test "Check field with default":
    check MyObj.hasDefault.hasDefaultValue

suite "Find field":
  type
    MyObj = object
      topLevel: string
      case kind: bool
      of false:
        insideVariant: int
      else:
        insideElseCase: float

  macro getFieldType(obj: typed, name: static[string]): typedesc =
    obj.getObjectDecl().get().findField(name).get()[1]

  test "Top level":
    check MyObj.getFieldType("topLevel") is string

  test "Discriminator":
    check MyObj.getFieldType("kind") is bool

  test "Inside variant":
    check MyObj.getFieldType("insideVariant") is int

  test "Inside else case":
    check MyObj.getFieldType("insideElseCase") is float

  test "Is case insensitive":
    check MyObj.getFieldType("top_level") is string
