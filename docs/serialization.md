# Serialization and Deserialization

***All generated Thrift types can be serialized and deserialized in a variety of formats***

Thrift for Haskell supports 3 standard Thrift serialization formats; `TBinaryProtocol`, `TCompactProtocol`, and `TSimpleJSONProtocol`. Thrift types can be serialized and deserialized using the following functions:

## Binary Protocol

Binary is the fastest format. It is useful where latency is more important than network usage.

```name=Thrift/Protocol/Binary.hs
serializeBinary :: ThriftSerializable a => a -> ByteString
deserializeBinary :: ThriftSerializable a => ByteString -> Either String a
```

## Compact Protocol

Compact protocol uses fewer bytes than Binary, but it takes longer to serialize and deserialize.

```name=Thrift/Protocol/Compact.hs
serializeCompact :: ThriftSerializable a => a -> ByteString
deserializeCompact ::ThriftSerializable a => ByteString -> Either String a
```

## Simple JSON Protocol

JSON is very inefficient in both speed and byte size. It should only be used when the output needs to be human readable. The regular JSON output has no whitespace. Pretty JSON is nicely formatted with indentation and one field per line.

```name=Thrift/Protocol/JSON.hs
serializeJSON :: ThriftSerializable a => a -> ByteString
serializePrettyJSON :: ThriftSerializable a => a -> ByteString
deserializeJSON ::ThriftSerializable a => ByteString -> Either String a
```
