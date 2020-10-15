#include <common/hs/runtime/HaskellRuntime.h>
#include <gtest/gtest.h>

extern "C" {
extern int hs_regexMatchWord(const char*, int);
}

TEST(EncodingForeignTest, DefaultTest) {
  facebook::haskell::HaskellRuntime runtime_{};
  const char* w = "lunedi";
  EXPECT_TRUE(hs_regexMatchWord(w, strlen(w)));
}
