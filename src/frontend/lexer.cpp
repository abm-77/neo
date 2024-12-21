#include <common/types.h>
#include <frontend/lexer.h>

#include <cctype>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#define make_equal_variant(T) make_token_if_else('=', T##_EQUAL, T)

namespace neo {
namespace lex {
std::string_view LexerResult::get_string_data(Token token) const {
  return string_data.at(token.data);
}

i64 LexerResult::get_int_data(Token token) const {
  return static_cast<i64>(token.data);
}

b32 LexerResult::get_bool_data(Token token) const {
  return static_cast<b32>(token.data);
}

Lexer Lexer::from_file(std::string_view filepath) {
  namespace fs = std::filesystem;
  auto path = fs::path(filepath);
  std::ifstream file(path, std::ios::in | std::ios::binary);
  const auto sz = fs::file_size(path);
  std::string data(sz, '\0');
  file.read(data.data(), sz);
  return Lexer(filepath, data);
}

Lexer Lexer::from_source(std::string_view source) {
  return Lexer("unknown", source);
}

Lexer::Lexer(std::string_view source_name, std::string_view source) {
  src = std::move(source);
  curr_pos = SourceLocation{
      .source_name = source_name.data(),
      .byte_offset = 0,
      .line = 1,
      .col = 0,
  };
}

b32 Lexer::eof() { return curr_pos.byte_offset >= src.size(); }

std::optional<u8> Lexer::peek() {
  if (this->eof()) {
    return {};
  } else {
    return this->src.at(curr_pos.byte_offset);
  }
}

b32 Lexer::peek_is(u8 expected) {
  auto c = peek();
  return c.has_value() && c == expected;
}

b32 Lexer::peek_digit() {
  auto c = peek();
  return c.has_value() && isdigit(c.value());
}

b32 Lexer::peek_alnum() {
  auto c = peek();
  return c.has_value() && isalnum(c.value());
}

b32 Lexer::peek_advance_if(u8 expected) {
  b32 cond = peek() == expected;
  if (cond)
    next_character();
  return cond;
}

std::string_view Lexer::get_src_slice() {
  return src.substr(start_pos.byte_offset,
                    curr_pos.byte_offset - start_pos.byte_offset);
}

Token Lexer::make_token(TokenType type) {
  u64 data = 0;
  switch (type) {
  case TOKEN_STRING_LITERAL:
  case TOKEN_IDENTIFIER: {
    auto lexeme = get_src_slice();
    data = string_data.size();
    string_data.push_back(lexeme);
  } break;
  case TOKEN_INTEGER_LITERAL: {
    auto lexeme = get_src_slice();
    data = static_cast<u64>(std::atoll(lexeme.data()));
  } break;
  case TOKEN_BOOL_LITERAL: {
    auto lexeme = get_src_slice();
    if (lexeme == "true") {
      data = 1;
    } else if (lexeme == "false") {
      data = 0;
    } else {
      data = -1;
    }
  } break;
  default: {
  } break;
  }
  return Token{
      .type = type,
      .data = data,
  };
}

Token Lexer::make_token_if_else(u8 expected, TokenType match, TokenType other) {
  return peek_advance_if(expected) ? make_token(match) : make_token(other);
}

Token Lexer::make_comment() {
  while (!peek_is('\n'))
    next_character();
  return make_token(TOKEN_COMMENT);
}

Token Lexer::make_number() {
  peek_advance_if('-');
  while (peek_digit())
    next_character();
  return make_token(TOKEN_INTEGER_LITERAL);
}

Token Lexer::make_string() {
  b32 terminated = false;
  auto c = next_character();
  while (true) {
    c = next_character();
    if (!c.has_value() || c.value() == '\n')
      break;

    if (c.value() == '\"') {
      terminated = true;
      break;
    }
  }

  if (!terminated)
    return make_token(TOKEN_STRING_LITERAL_UNTERMINATED);

  return make_token(TOKEN_STRING_LITERAL);
}

Token Lexer::make_identifier() {
  while (peek_alnum() || peek_is('_')) {
    next_character();
  }
  auto lexeme = get_src_slice();
  auto keyword = KEYWORDS[lexeme];
  return (keyword) ? make_token(keyword) : make_token(TOKEN_IDENTIFIER);
}

std::optional<u8> Lexer::next_character() {
  if (this->eof())
    return {};

  u8 c = this->peek().value();

  if (c == '\n') {
    this->curr_pos.line += 1;
    this->curr_pos.col = 1;
  } else {
    this->curr_pos.col += 1;
  }
  this->curr_pos.byte_offset += 1;

  return c;
}

Token Lexer::consume_token() {
  start_pos = curr_pos;

  auto nc = next_character();
  if (!nc.has_value())
    return make_token(TOKEN_EOF);

  u8 c = nc.value();
  if (std::isspace(c))
    return make_token(TOKEN_SPACE);
  if (isalpha(c) || c == '_')
    return make_identifier();
  if (isdigit(c) || (c == '-' && peek_digit()))
    return make_number();

  switch (c) {
  case '\"':
    return make_string();
  case '(':
    return make_token(TOKEN_LEFT_PAREN);
  case ')':
    return make_token(TOKEN_RIGHT_PAREN);
  case '{':
    return make_token(TOKEN_LEFT_BRACE);
  case '}':
    return make_token(TOKEN_RIGHT_BRACE);
  case '[':
    return make_token(TOKEN_LEFT_BRACKET);
  case ']':
    return make_token(TOKEN_RIGHT_BRACKET);
  case ';':
    return make_token(TOKEN_SEMICOLON);
  case ':':
    return make_token(TOKEN_COLON);
  case ',':
    return make_token(TOKEN_COMMA);
  case '.':
    return make_token(TOKEN_DOT);
  case '#':
    return make_comment();
  case '+':
    return make_equal_variant(TOKEN_PLUS);
  case '-':
    return make_equal_variant(TOKEN_MINUS);
  case '*':
    return make_equal_variant(TOKEN_STAR);
  case '/':
    return make_equal_variant(TOKEN_SLASH);
  case '%':
    return make_equal_variant(TOKEN_PERCENT);
  case '!':
    return make_equal_variant(TOKEN_BANG);
  case '=':
    return make_equal_variant(TOKEN_EQUAL);
  case '<':
    return make_equal_variant(TOKEN_LESS);
  case '>':
    return make_token_if_else('=', TOKEN_GREATER_EQUAL, TOKEN_GREATER);
  default:
    return make_token(TOKEN_INVALID);
  };
}

LexerResult Lexer::lex() {
  std::vector<Token> tokens;

  while (!eof()) {
    Token token = consume_token();
    if (token.type == TOKEN_SPACE || token.type == TOKEN_COMMENT)
      continue;
    tokens.push_back(token);
  }

  return LexerResult{
      .tokens = std::move(tokens),
      .string_data = std::move(string_data),
  };
}

} // namespace lex
} // namespace neo
