include "tests/if/B.thrift"
include "tests/if/C.thrift"
include "tests/if/D.thrift"
include "tests/if/E.thrift"

# INCLUSION HIERARCHY
#
#        A
#      /   \
#     B     C
#   /   \ /
#  D     E

struct A {
  1: B.B bThing = { "dThing": {}, "eThing": {}, "b2Thing": B.B2.X },
  2: C.C cThing,
}

const D.D dstruct = {}
