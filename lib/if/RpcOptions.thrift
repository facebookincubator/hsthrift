namespace cpp2 "thrift.protocol"
namespace hs "Thrift.Protocol"

struct RpcOptions {
  1: i32 timeout;
  2: optional i32 priority;
  3: i32 chunkTimeout;
  4: i32 queueTimeout;
  5: optional map<string, string> headers;
} (hs.prefix = "rpc_")
