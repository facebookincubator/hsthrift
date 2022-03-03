// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/HaskellProcessor.h"

#include <folly/executors/ManualExecutor.h>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace apache {
namespace thrift {

using namespace testing;

struct MockResponseChannelRequest : public ResponseChannelRequest {
  explicit MockResponseChannelRequest(size_t& d) : active(true), destroyed(d) {}
  ~MockResponseChannelRequest() override {
    ++destroyed;
  }

  bool isActive() const override {
    return active;
  }
  bool isOneway() const override {
    return false;
  }
  bool tryStartProcessing() override {
    return true;
  }
  bool includeEnvelope() const override {
    return true;
  }

  MOCK_METHOD(
      void,
      sendReply,
      (ResponsePayload&&,
       MessageChannel::SendCallback*,
       folly::Optional<uint32_t>));

  MOCK_METHOD(
      void,
      sendException,
      (ResponsePayload&&, MessageChannel::SendCallback*));

  MOCK_METHOD(void, sendErrorWrapped, (folly::exception_wrapper, std::string));

  bool active;
  size_t& destroyed;
};

struct MockThreadManager : public concurrency::ThreadManagerExecutorAdapter {
  MockThreadManager(
      std::shared_ptr<folly::ManualExecutor> exec =
          std::make_shared<folly::ManualExecutor>())
      : concurrency::ThreadManagerExecutorAdapter(exec), executor(exec) {}

  std::shared_ptr<folly::ManualExecutor> executor;
};

struct HaskellProcessorTest : public Test {
  HaskellProcessorTest()
      : processor(callback, oneways_),
        header(std::make_unique<transport::THeader>()),
        conn_context(std::make_unique<Cpp2ConnContext>()),
        request_context(std::make_unique<Cpp2RequestContext>(
            conn_context.get(),
            header.get())) {}

  class Request {
   public:
    Request()
        : pointer(new MockResponseChannelRequest(destroyed)),
          owner(true),
          destroyed(0) {}
    ~Request() {
      EXPECT_EQ(destroyed, 1);
    }

    MockResponseChannelRequest* operator->() const {
      EXPECT_TRUE(alive());
      return pointer;
    }

    MockResponseChannelRequest& operator*() const {
      EXPECT_TRUE(alive());
      return *pointer;
    }

    bool alive() const {
      return destroyed == 0;
    }

    std::unique_ptr<MockResponseChannelRequest> move() {
      EXPECT_TRUE(owner);
      return std::unique_ptr<MockResponseChannelRequest>(pointer);
    }

   private:
    MockResponseChannelRequest* pointer;
    bool owner;
    size_t destroyed;
  };

  void process(Request& req) {
    processor.processSerializedRequest(
        req.move(),
        apache::thrift::SerializedRequest(std::make_unique<folly::IOBuf>()),
        protocol::T_BINARY_PROTOCOL,
        request_context.get(),
        &event_base,
        &thread_manager);

    thread_manager.executor->drain();
  }

  static void callback(uint16_t, const uint8_t*, size_t, TResponse* resp) {
    auto result = malloc(response.size());
    std::memcpy(result, response.data(), response.size());
    resp->data = reinterpret_cast<uint8_t*>(result);
    resp->len = response.size();
  }

  static const folly::fbstring response;

  const std::unordered_set<std::string> oneways_;

  HaskellAsyncProcessor processor;

  folly::EventBase event_base;
  MockThreadManager thread_manager;

  std::shared_ptr<EventTask> task;

  std::unique_ptr<transport::THeader> header;
  std::unique_ptr<Cpp2ConnContext> conn_context;
  std::unique_ptr<Cpp2RequestContext> request_context;
};

const folly::fbstring HaskellProcessorTest::response = "Hello, world!";

TEST_F(HaskellProcessorTest, respond) {
  folly::fbstring response;
  Request req;
  EXPECT_CALL(*req, sendReply(_, _, _))
      .WillOnce(Invoke([&response](auto&& buf, auto, auto) {
        response = std::move(buf).buffer()->moveToFbString();
      }));
  EXPECT_CALL(*req, sendErrorWrapped(_, _)).Times(Exactly(0));

  process(req);
  EXPECT_TRUE(req.alive());
  event_base.loop();

  EXPECT_EQ(response, HaskellProcessorTest::response);
}

TEST_F(HaskellProcessorTest, not_active) {
  Request req;
  EXPECT_CALL(*req, sendReply(_, _, _)).Times(Exactly(0));
  EXPECT_CALL(*req, sendErrorWrapped(_, _)).Times(Exactly(0));

  req->active = false;
  process(req);
  EXPECT_TRUE(req.alive());
  event_base.loop();
}

} // namespace thrift
} // namespace apache
