#include "lexer.h"
#include <gtest/gtest.h>
#include <string>
#include <string_view>

using namespace neo::lex;

static void
EXPECT_LIST(LexerResult &result,
            std::vector<std::pair<TokenType, std::string_view>> &expected) {
  EXPECT_EQ(result.tokens.size(), expected.size());

  for (u32 i = 0; i < result.tokens.size(); i++) {
    auto token = result.tokens[i];
    auto [ex_type, ex_lexeme] = expected[i];

    EXPECT_EQ(token.type, ex_type)
        << "expected: " << token_type_to_string(ex_type)
        << " actual: " << token_type_to_string(token.type) << std::endl;

    if (ex_type == TOKEN_IDENTIFIER) {
      EXPECT_EQ(ex_lexeme, result.get_string_data(token))
          << "expected: " << ex_lexeme
          << " actual: " << result.get_string_data(token) << std::endl;
    } else if (ex_type == TOKEN_INTEGER_LITERAL) {
      EXPECT_EQ(ex_lexeme, std::to_string(result.get_int_data(token)))
          << "expected: " << ex_lexeme
          << " actual: " << result.get_int_data(token) << std::endl;
    }
  }
}

TEST(Lexer, LexImport) {
  auto L = Lexer::from_source("import fmt;");
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_IMPORT, ""},
      {TOKEN_IDENTIFIER, "fmt"},
      {TOKEN_SEMICOLON, ""},
  };
  EXPECT_LIST(result, expected);
}
TEST(Lexer, LexIdentifier) {
  auto input = "hello";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_IDENTIFIER);
  EXPECT_EQ(result.get_string_data(token), input);
}

TEST(Lexer, LexIdentifierComplex) {
  auto input = "_h3ll0_w0rld_";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_IDENTIFIER);
  EXPECT_EQ(result.get_string_data(token), input);
}

TEST(Lexer, LexIntLiteralPositive) {
  auto input = 12345;
  auto L = Lexer::from_source(std::to_string(input));
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_INTEGER_LITERAL);
  EXPECT_EQ(result.get_int_data(token), input);
}

TEST(Lexer, LexIntLiteralNegative) {
  auto input = -12345;
  auto L = Lexer::from_source(std::to_string(input));
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_INTEGER_LITERAL);
  EXPECT_EQ(result.get_int_data(token), input);
}

TEST(Lexer, LexString) {
  auto input = "\"hello\"";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_STRING_LITERAL);
  EXPECT_EQ(result.get_string_data(token), input);
}

TEST(Lexer, LexStringUnterminated) {
  auto input = "\"hello";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_STRING_LITERAL_UNTERMINATED);
}

TEST(Lexer, LexBoolLiteralTrue) {
  auto L = Lexer::from_source("true");
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_BOOL_LITERAL);
  EXPECT_EQ(result.get_bool_data(token), true);
}

TEST(Lexer, LexBoolLiteralFalse) {
  auto L = Lexer::from_source("false");
  auto result = L.lex();
  auto token = result.tokens.back();
  EXPECT_EQ(token.type, TOKEN_BOOL_LITERAL);
  EXPECT_EQ(result.get_bool_data(token), false);
}

TEST(Lexer, LexConstantDef) {
  auto L = Lexer::from_source("const a: int = 10;");
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_CONST, ""},     {TOKEN_IDENTIFIER, "a"},
      {TOKEN_COLON, ""},     {TOKEN_INT, ""},
      {TOKEN_EQUAL, ""},     {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexVariableDef) {
  auto L = Lexer::from_source("var a: int = 10;");
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_VAR, ""},       {TOKEN_IDENTIFIER, "a"},
      {TOKEN_COLON, ""},     {TOKEN_INT, ""},
      {TOKEN_EQUAL, ""},     {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexArrayDef) {
  auto L = Lexer::from_source("const a: [3]int = {1, 2, 3};");
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_CONST, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_COLON, ""},
      {TOKEN_LEFT_BRACKET, ""},
      {TOKEN_INTEGER_LITERAL, "3"},
      {TOKEN_RIGHT_BRACKET, ""},
      {TOKEN_INT, ""},
      {TOKEN_EQUAL, ""},
      {TOKEN_LEFT_BRACE, ""},
      {TOKEN_INTEGER_LITERAL, "1"},
      {TOKEN_COMMA, ""},
      {TOKEN_INTEGER_LITERAL, "2"},
      {TOKEN_COMMA, ""},
      {TOKEN_INTEGER_LITERAL, "3"},
      {TOKEN_RIGHT_BRACE, ""},
      {TOKEN_SEMICOLON, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexFunction) {
  auto input = "export fn add(a: int) int {"
               "return a + 10;"
               "}";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_EXPORT, ""},        {TOKEN_FUNC, ""},
      {TOKEN_IDENTIFIER, "add"}, {TOKEN_LEFT_PAREN, ""},
      {TOKEN_IDENTIFIER, "a"},   {TOKEN_COLON, ""},
      {TOKEN_INT, ""},           {TOKEN_RIGHT_PAREN, ""},
      {TOKEN_INT, ""},           {TOKEN_LEFT_BRACE, ""},
      {TOKEN_RETURN, ""},        {TOKEN_IDENTIFIER, "a"},
      {TOKEN_PLUS, ""},          {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},     {TOKEN_RIGHT_BRACE, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexIfChain) {
  auto input = "if(a > 5) {"
               "return a * 10;"
               "}"
               "else if (a % 3 == 0){"
               "return a / -13;"
               "}"
               "else{"
               "return -a - 10;"
               "}";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_COND_IF, ""},
      {TOKEN_LEFT_PAREN, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_GREATER, ""},
      {TOKEN_INTEGER_LITERAL, "5"},
      {TOKEN_RIGHT_PAREN, ""},
      {TOKEN_LEFT_BRACE, ""},
      {TOKEN_RETURN, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_STAR, ""},
      {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},
      {TOKEN_RIGHT_BRACE, ""},
      {TOKEN_COND_ELSE, ""},
      {TOKEN_COND_IF, ""},
      {TOKEN_LEFT_PAREN, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_PERCENT, ""},
      {TOKEN_INTEGER_LITERAL, "3"},
      {TOKEN_EQUAL_EQUAL, ""},
      {TOKEN_INTEGER_LITERAL, "0"},
      {TOKEN_RIGHT_PAREN, ""},
      {TOKEN_LEFT_BRACE, ""},
      {TOKEN_RETURN, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_SLASH, ""},
      {TOKEN_INTEGER_LITERAL, "-13"},
      {TOKEN_SEMICOLON, ""},
      {TOKEN_RIGHT_BRACE, ""},
      {TOKEN_COND_ELSE, ""},
      {TOKEN_LEFT_BRACE, ""},
      {TOKEN_RETURN, ""},
      {TOKEN_MINUS, ""},
      {TOKEN_IDENTIFIER, "a"},
      {TOKEN_MINUS, ""},
      {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},
      {TOKEN_RIGHT_BRACE, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexForLoop) {
  auto input = "for (var i = 0; i < 10; i += 1) {"
               "a[i] = i;"
               "}";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_CTRL_FOR, ""},    {TOKEN_LEFT_PAREN, ""},
      {TOKEN_VAR, ""},         {TOKEN_IDENTIFIER, "i"},
      {TOKEN_EQUAL, ""},       {TOKEN_INTEGER_LITERAL, "0"},
      {TOKEN_SEMICOLON, ""},   {TOKEN_IDENTIFIER, "i"},
      {TOKEN_LESS, ""},        {TOKEN_INTEGER_LITERAL, "10"},
      {TOKEN_SEMICOLON, ""},   {TOKEN_IDENTIFIER, "i"},
      {TOKEN_PLUS_EQUAL, ""},  {TOKEN_INTEGER_LITERAL, "1"},
      {TOKEN_RIGHT_PAREN, ""}, {TOKEN_LEFT_BRACE, ""},
      {TOKEN_IDENTIFIER, "a"}, {TOKEN_LEFT_BRACKET, ""},
      {TOKEN_IDENTIFIER, "i"}, {TOKEN_RIGHT_BRACKET, ""},
      {TOKEN_EQUAL, ""},       {TOKEN_IDENTIFIER, "i"},
      {TOKEN_SEMICOLON, ""},   {TOKEN_RIGHT_BRACE, ""},
  };
  EXPECT_LIST(result, expected);
}

TEST(Lexer, LexWhileLoop) {
  auto input = "while (i < 10) {"
               "continue;"
               "}";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto expected = std::vector<std::pair<TokenType, std::string_view>>{
      {TOKEN_CTRL_WHILE, ""},        {TOKEN_LEFT_PAREN, ""},
      {TOKEN_IDENTIFIER, "i"},       {TOKEN_LESS, ""},
      {TOKEN_INTEGER_LITERAL, "10"}, {TOKEN_RIGHT_PAREN, ""},
      {TOKEN_LEFT_BRACE, ""},        {TOKEN_CTRL_CONTINUE, ""},
      {TOKEN_SEMICOLON, ""},         {TOKEN_RIGHT_BRACE, ""},
  };
  EXPECT_LIST(result, expected);
}
