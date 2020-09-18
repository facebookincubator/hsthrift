include "common/hs/thrift/tests/if/B.thrift"
include "common/hs/thrift/tests/if/C.thrift"

# INCLUSION HIERARCHY
#
#        A
#      /   \
#     B     C
#   /   \ /
#  D     E

struct A {
  1: B.B bThing
  2: C.C cThing
}
