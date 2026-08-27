/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cpp/CppServer.h"

#include <poll.h>
#include <unistd.h>

#include <array>
#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>

#include <gtest/gtest.h>

#include <thrift/lib/cpp/concurrency/ThreadManager.h>
#include <thrift/lib/cpp2/server/ServerFlags.h>
#include <thrift/lib/cpp2/server/ThriftServer.h>

#include "cpp/HsStruct.h"

namespace apache::thrift {
class CppServer;
}

using CreateCppServerResult = HsEither<apache::thrift::CppServer*, HsString>;

// =============================================================================
// CppServer FFI declarations
// =============================================================================

extern "C" apache::thrift::AsyncProcessorFactory* c_haskell_factory(
    apache::thrift::TCallback callback,
    apache::thrift::AsyncProcessorFactory::MethodMetadataMap&
        metadataMap) noexcept;

extern "C" CreateCppServerResult* c_create_cpp_server(
    apache::thrift::TCallback callback,
    apache::thrift::TFactory factoryFn,
    int desiredPort,
    int workers,
    const int* poolSizeOverridePriorities,
    const size_t* poolSizeOverrideSizes,
    size_t numPoolSizeOverrides,
    const apache::thrift::concurrency::PRIORITY* methodPriorities,
    const bool* methodOneways,
    const char** methodNames,
    size_t* methodNamesSizes,
    size_t methodsLength) noexcept;

extern "C" void c_destroy_cpp_server(
    apache::thrift::CppServer* server) noexcept;

extern "C" HsString* c_serve_cpp_server(
    apache::thrift::CppServer* server,
    int fd,
    int* port,
    void (*modify)(apache::thrift::ThriftServer&)) noexcept;

namespace {

using apache::thrift::AsyncProcessorFactory;
using apache::thrift::CppServer;
using apache::thrift::RpcKind;
using apache::thrift::ThriftServer;
using apache::thrift::concurrency::N_PRIORITIES;
using apache::thrift::concurrency::PRIORITY;
using apache::thrift::concurrency::PriorityThreadManager;

constexpr char kFactoryError[] = "factory rejected configuration";
constexpr char kStartupError[] = "test stopped startup";
constexpr char kInspectionComplete[] = "configuration inspection complete";

struct Observations {
  AsyncProcessorFactory::MethodMetadataMap metadata;
  size_t workers = 0;
  uint16_t port = 0;
  std::array<size_t, N_PRIORITIES> poolSizes{};
  bool priorityManager = false;
};

Observations& observations() {
  static Observations value;
  return value;
}

void noopCallback(
    uint16_t,
    const uint8_t*,
    size_t,
    apache::thrift::TResponse*) {}

AsyncProcessorFactory* recordingFactory(
    apache::thrift::TCallback callback,
    AsyncProcessorFactory::MethodMetadataMap& metadataMap) {
  observations().metadata = metadataMap;
  return c_haskell_factory(callback, metadataMap);
}

[[noreturn]]
AsyncProcessorFactory* throwingFactory(
    apache::thrift::TCallback,
    AsyncProcessorFactory::MethodMetadataMap&) {
  throw std::runtime_error(kFactoryError);
}

[[noreturn]]
void throwDuringStartup(ThriftServer&) {
  throw std::runtime_error(kStartupError);
}

[[noreturn]]
void inspectConfigurationAndStop(ThriftServer& server) {
  auto& observed = observations();
  observed.workers = server.getNumCPUWorkerThreads();
  observed.port = server.getPort();
  server.setupThreadManager();

  auto manager = std::dynamic_pointer_cast<PriorityThreadManager>(
      server.getThreadManager());
  observed.priorityManager = manager != nullptr;
  if (manager != nullptr) {
    for (int priority = 0; priority < N_PRIORITIES; ++priority) {
      observed.poolSizes[priority] =
          manager->workerCount(static_cast<PRIORITY>(priority));
    }
  }
  throw std::runtime_error(kInspectionComplete);
}

std::unique_ptr<CreateCppServerResult> createServer(
    int desiredPort = 0,
    int workers = 0,
    const int* poolSizeOverridePriorities = nullptr,
    const size_t* poolSizeOverrideSizes = nullptr,
    size_t numPoolSizeOverrides = 0,
    const PRIORITY* methodPriorities = nullptr,
    const bool* methodOneways = nullptr,
    const char** methodNames = nullptr,
    size_t* methodNameSizes = nullptr,
    size_t methodsLength = 0,
    apache::thrift::TFactory factory = recordingFactory) {
  return std::unique_ptr<CreateCppServerResult>(c_create_cpp_server(
      noopCallback,
      factory,
      desiredPort,
      workers,
      poolSizeOverridePriorities,
      poolSizeOverrideSizes,
      numPoolSizeOverrides,
      methodPriorities,
      methodOneways,
      methodNames,
      methodNameSizes,
      methodsLength));
}

using ServerPtr = std::unique_ptr<CppServer, decltype(&c_destroy_cpp_server)>;

ServerPtr takeServer(const CreateCppServerResult& result) {
  return ServerPtr(result.getLeft(), c_destroy_cpp_server);
}

TEST(CppServerTest, BuildsMethodMetadataFromFfiArrays) {
  const std::string requestName = "lookup";
  const std::string onewayName{"notify\0audit", 12};
  std::array<const char*, 2> names{requestName.data(), onewayName.data()};
  std::array<size_t, 2> nameSizes{requestName.size(), onewayName.size()};
  const std::array<PRIORITY, 2> priorities{
      apache::thrift::concurrency::NORMAL, apache::thrift::concurrency::HIGH};
  const bool oneways[] = {false, true};
  observations().metadata.clear();

  const auto result = createServer(
      0,
      0,
      nullptr,
      nullptr,
      0,
      priorities.data(),
      oneways,
      names.data(),
      nameSizes.data(),
      names.size());

  ASSERT_TRUE(result->hasLeft());
  const auto server = takeServer(*result);
  ASSERT_NE(server, nullptr);
  ASSERT_EQ(observations().metadata.size(), 2);
  const auto& request = *observations().metadata.at(requestName);
  const auto& oneway = *observations().metadata.at(onewayName);
  EXPECT_EQ(
      request.executorType,
      AsyncProcessorFactory::MethodMetadata::ExecutorType::ANY);
  EXPECT_EQ(
      request.interactionType,
      AsyncProcessorFactory::MethodMetadata::InteractionType::UNKNOWN);
  EXPECT_EQ(request.rpcKind, RpcKind::SINGLE_REQUEST_SINGLE_RESPONSE);
  EXPECT_EQ(request.priority, apache::thrift::concurrency::NORMAL);
  EXPECT_EQ(oneway.rpcKind, RpcKind::SINGLE_REQUEST_NO_RESPONSE);
  EXPECT_EQ(oneway.priority, apache::thrift::concurrency::HIGH);
}

TEST(CppServerTest, AppliesPartialPoolOverridesAndIgnoresInvalidPriorities) {
  gflags::FlagSaver flagSaver;
  FLAGS_thrift_disable_resource_pools = true;
  constexpr int kDesiredPort = 4312;
  constexpr int kWorkers = 6;
  const std::array<int, 4> overridePriorities{
      apache::thrift::concurrency::HIGH,
      apache::thrift::concurrency::BEST_EFFORT,
      -1,
      N_PRIORITIES};
  const std::array<size_t, 4> overrideSizes{3, 4, 101, 102};
  auto expectedPoolSizes = PriorityThreadManager::defaultThreadCounts(kWorkers);
  expectedPoolSizes[apache::thrift::concurrency::HIGH] = 3;
  expectedPoolSizes[apache::thrift::concurrency::BEST_EFFORT] = 4;
  observations().poolSizes.fill(0);
  observations().priorityManager = false;

  const auto result = createServer(
      kDesiredPort,
      kWorkers,
      overridePriorities.data(),
      overrideSizes.data(),
      overridePriorities.size());
  ASSERT_TRUE(result->hasLeft());
  const auto server = takeServer(*result);
  int port = -1;

  const std::unique_ptr<HsString> error(
      c_serve_cpp_server(server.get(), -1, &port, inspectConfigurationAndStop));

  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->getStr(), kInspectionComplete);
  EXPECT_EQ(observations().port, kDesiredPort);
  EXPECT_EQ(observations().workers, kWorkers);
  EXPECT_TRUE(observations().priorityManager);
  EXPECT_EQ(observations().poolSizes, expectedPoolSizes);
}

TEST(CppServerTest, FactoryFailureIsReturnedAcrossNoexceptBoundary) {
  const auto result = createServer(
      0,
      0,
      nullptr,
      nullptr,
      0,
      nullptr,
      nullptr,
      nullptr,
      nullptr,
      0,
      throwingFactory);

  ASSERT_TRUE(result->hasRight());
  EXPECT_EQ(result->getRight().getStr(), kFactoryError);
}

TEST(CppServerTest, StartupFailureClearsPortAndNotifiesWaiter) {
  int pipeFds[2] = {-1, -1};
  ASSERT_EQ(::pipe(pipeFds), 0);
  const auto result = createServer();
  ASSERT_TRUE(result->hasLeft());
  const auto server = takeServer(*result);
  int port = 1234;

  const std::unique_ptr<HsString> error(
      c_serve_cpp_server(server.get(), pipeFds[1], &port, throwDuringStartup));

  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->getStr(), kStartupError);
  EXPECT_EQ(port, 0);
  pollfd readable{pipeFds[0], POLLIN, 0};
  ASSERT_EQ(::poll(&readable, 1, 1000), 1);
  int64_t notification = 0;
  ASSERT_EQ(
      ::read(pipeFds[0], &notification, sizeof(notification)),
      sizeof(notification));
  EXPECT_EQ(notification, 1);
  EXPECT_EQ(::close(pipeFds[0]), 0);
  EXPECT_EQ(::close(pipeFds[1]), 0);
}

} // namespace
