// Copyright (c) Facebook, Inc. and its affiliates.

exception NotFound {
  1: string key,
}

service HashMapService {

  void put(1: string key, 2: binary value)

  binary get(1: string key) throws (1: NotFound ex)

}
