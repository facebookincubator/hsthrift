package "facebook.com/hs/thrift/tests/recursive_annotation"

// recursive dependency between typedef and struct used as an annotation

@Ann{i=42}
typedef i64 T

@Ann{i=43}
const Ann x = Ann {i = 44}

struct Ann { 1: T i }
