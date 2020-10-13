#pragma once
#include "common/hs/util/cpp/Constructible.h"
#include "common/hs/util/cpp/Destructible.h"

#ifndef HS_DEFINE_MARSHALLABLE
#define HS_DEFINE_MARSHALLABLE(Name, Type...) \
  HS_DEFINE_DEFAULT_CONSTRUCTIBLE(Name, Type) \
  HS_DEFINE_DESTRUCTIBLE(Name, Type)
#endif
