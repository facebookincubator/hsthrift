include "tests/if/E.thrift"

# INCLUSION HIERARCHY
#
#        C
#        |
#        E

struct C {
  1: E.E eThing
}
