include "common/hs/thrift/tests/if/D.thrift"
include "common/hs/thrift/tests/if/E.thrift"

# INCLUSION HIERARCHY
#
#        B
#      /   \
#     D     E

struct B {
  1: D.D dThing
  2: E.E eThing
}
