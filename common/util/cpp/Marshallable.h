// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once
#include "cpp/Constructible.h"
#include "cpp/Destructible.h"

#ifndef HS_DEFINE_MARSHALLABLE
#define HS_DEFINE_MARSHALLABLE(Name, Type...) \
  HS_DEFINE_DEFAULT_CONSTRUCTIBLE(Name, Type) \
  HS_DEFINE_DESTRUCTIBLE(Name, Type)
#endif
