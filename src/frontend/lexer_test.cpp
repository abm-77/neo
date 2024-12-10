#include "lexer.h"
#include <gtest/gtest.h>

TEST(Lexer, add) { EXPECT_EQ(neo::lex::add(1, 2), 3); }
