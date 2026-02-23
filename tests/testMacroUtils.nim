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
