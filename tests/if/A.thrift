include "common/hs/thrift/tests/if/B.thrift"
include "common/hs/thrift/tests/if/C.thrift"
include "common/hs/thrift/tests/if/D.thrift"
include "common/hs/thrift/tests/if/E.thrift"

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
