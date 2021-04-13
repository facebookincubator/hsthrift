// Copyright (c) Facebook, Inc. and its affiliates.

#include <folly/io/async/AsyncSocket.h>
#include <thrift/lib/cpp2/async/HeaderClientChannel.h>

using namespace apache::thrift;
using namespace folly;

extern "C" {

HeaderClientChannel::Ptr* newHeaderChannel(
    const char* host_str,
    size_t host_len,
    size_t port,
    uint16_t protocol_id,
    size_t conn_timeout,
    size_t send_timeout,
    size_t recv_timeout,
    EventBase* eb) noexcept {
  SocketAddress addr(std::string(host_str, host_len), port);

  // Construction of the socket needs to be in the event base thread
  auto f = folly::via(eb, [=, &addr] {
    auto sock = AsyncSocket::newSocket(eb, addr, conn_timeout);
    auto chan = HeaderClientChannel::newChannel(std::move(sock));
    chan->setProtocolId(protocol_id);
    chan->setTimeout(send_timeout + recv_timeout);
    chan->getTransport()->setSendTimeout(send_timeout);
    return chan;
  });
  return new HeaderClientChannel::Ptr(std::move(f).get());
}

void deleteHeaderChannel(HeaderClientChannel::Ptr* ch) noexcept {
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
}
