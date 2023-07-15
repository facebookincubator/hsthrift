/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/io/async/AsyncSocket.h>
#include <thrift/lib/cpp2/async/RocketClientChannel.h>

using namespace apache::thrift;
using namespace folly;

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

extern "C" {

typedef folly::AsyncTransport::UniquePtr (*MakeTransport)(
    const folly::SocketAddress& addr,
    folly::EventBase* eb,
    size_t conn_timeout);

RocketClientChannel::Ptr* newHeaderChannel(
    const char* host_str,
    size_t host_len,
    size_t port,
    MakeTransport makeTransport,
    protocol::PROTOCOL_TYPES protocol_id,
    size_t conn_timeout,
    size_t send_timeout,
    size_t recv_timeout,
    EventBase* eb) noexcept {
  SocketAddress addr(std::string(host_str, host_len), port);

  // Construction of the socket needs to be in the event base thread
  auto f = folly::via(eb, [=, &addr] {
    auto transport = makeTransport(addr, eb, conn_timeout);
    auto chan = RocketClientChannel::newChannel(std::move(transport));
    chan->setProtocolId(protocol_id);
    chan->setTimeout(send_timeout + recv_timeout);
    chan->getTransport()->setSendTimeout(send_timeout);
    return chan;
  });
  return new RocketClientChannel::Ptr(std::move(f).get());
}

void deleteHeaderChannel(RocketClientChannel::Ptr* ch) noexcept {
  auto h = std::move(*ch);
  delete ch;
  // Destruction needs to be done in the event base thread too
  if (h != nullptr) {
    auto eb = h->getEventBase();
    eb->runInEventBaseThread([h = std::move(h)] {});
  }
}

uint16_t getRequestChannelProtocolId(
    std::unique_ptr<RequestChannel, DelayedDestruction::Destructor>*
        ch) noexcept {
  return ch->get()->getProtocolId();
}

folly::AsyncTransport::UniquePtr makeRawTransport(
    const folly::SocketAddress& addr,
    folly::EventBase* eb,
    size_t conn_timeout) {
  return AsyncSocket::newSocket(eb, addr, conn_timeout);
}
}
