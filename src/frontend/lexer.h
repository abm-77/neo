#pragma once

#include "../common/types.h"

#include <optional>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace neo {
namespace lex {

// TokenType describes the type of a Token.
enum TokenType : u8 {
  // general tokens
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_LEFT_BRACKET,
  TOKEN_RIGHT_BRACKET,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_SEMICOLON,
  TOKEN_COLON,
  TOKEN_MINUS,
  TOKEN_MINUS_EQUAL,
  TOKEN_PLUS,
  TOKEN_PLUS_EQUAL,
  TOKEN_SLASH,
  TOKEN_SLASH_EQUAL,
  TOKEN_STAR,
  TOKEN_STAR_EQUAL,
  TOKEN_PERCENT,
  TOKEN_PERCENT_EQUAL,
  TOKEN_BANG,
  TOKEN_BANG_EQUAL,
  TOKEN_EQUAL,
  TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER,
  TOKEN_GREATER_EQUAL,
  TOKEN_LESS,
  TOKEN_LESS_EQUAL,
  TOKEN_COMMENT,
  TOKEN_SPACE,
  TOKEN_TYPE,
  TOKEN_EOF,
  TOKEN_INVALID,

  // keywords
  TOKEN_VOID,
  TOKEN_FLOAT,
  TOKEN_INT,
  TOKEN_COND_ELSE,
  TOKEN_BOOL,
  TOKEN_CONST,
  TOKEN_VAR,
  TOKEN_IMPORT,
  TOKEN_EXPORT,
  TOKEN_EXTERN,
  TOKEN_STRUCT,
  TOKEN_FUNC,
  TOKEN_CTRL_FOR,
  TOKEN_CTRL_WHILE,
  TOKEN_CTRL_BREAK,
  TOKEN_CTRL_CONTINUE,
  TOKEN_COND_IF,
  TOKEN_BOOL_OR,
  TOKEN_BOOL_AND,
  TOKEN_RETURN,

  // TODO: in the event that we support different sizes of numerics (e.g. i32,
  // f32, u32, etc.) we need to refactor how we handle the extra data.
  // For right now, since the only data that cannot fit in 64-bits are
  // identifiers, the extra data just holds strings.
  //
  // liteals (tokens that have payloads)
  TOKEN_IDENTIFIER,      // data points to string_data
  TOKEN_INTEGER_LITERAL, // data is bitcast to i32
  TOKEN_STRING_LITERAL,  // data is bitcast to i32
  TOKEN_STRING_LITERAL_UNTERMINATED,
  TOKEN_BOOL_LITERAL,
};

static std::unordered_map<std::string_view, TokenType> KEYWORDS{
    {"void", TOKEN_VOID},         {"const", TOKEN_CONST},
    {"var", TOKEN_VAR},           {"int", TOKEN_INT},
    {"bool", TOKEN_BOOL},         {"import", TOKEN_IMPORT},
    {"export", TOKEN_EXPORT},     {"extern", TOKEN_EXTERN},
    {"struct", TOKEN_STRUCT},     {"fn", TOKEN_FUNC},
    {"for", TOKEN_CTRL_FOR},      {"while", TOKEN_CTRL_WHILE},
    {"break", TOKEN_CTRL_BREAK},  {"continue", TOKEN_CTRL_CONTINUE},
    {"if", TOKEN_COND_IF},        {"else", TOKEN_COND_ELSE},
    {"or", TOKEN_BOOL_OR},        {"and", TOKEN_BOOL_AND},
    {"true", TOKEN_BOOL_LITERAL}, {"false", TOKEN_BOOL_LITERAL},
    {"return", TOKEN_RETURN},     {"float", TOKEN_FLOAT}};

const char *token_type_to_string(TokenType type);

/*
 * Token is is the unified type to represent tokens lexed from
 * a source file. Using the type field, one can interpret the use of the
 * data field. Typically, the data field is interpreted in one of 3 ways:
 * 1. Not used at all
 * 2. To be cast into a value of the same size (u32).
 * 3. As a pointer into the extra array within the lexer.
 * */
struct Token {
  TokenType type;
  u64 data;
};

/*
 * Calling the lex function on an instance of Lexer will produce
 * a LexerResult. This object stores all the tokens found during
 * lexing and has some helpers for accessing extra data related
 * to a token.
 * */
struct LexerResult {
  std::vector<Token> tokens;
  std::vector<std::string_view> string_data;

  std::string_view get_string_data(Token token) const;
  i64 get_int_data(Token token) const;
  b32 get_bool_data(Token token) const;
};

/*
 * Lexer takes in an input of either a string or a file and
 * will produce a LexerResult upon a user calling lex().
 * */
class Lexer {
public:
  // Create a lexer from a file source.
  static Lexer from_file(std::string_view filepath);

  // Create a lexer from a string source.
  static Lexer from_source(std::string_view source);

  // Lex the current source input.
  LexerResult lex();

private:
  Lexer(std::string_view source_name, std::string_view source);

  b32 eof();

  std::optional<u8> peek();
  b32 peek_is(u8 expected);
  b32 peek_digit();
  b32 peek_alnum();
  b32 peek_advance_if(u8 expected);

  std::optional<u8> next_character();
  std::string_view get_src_slice();

  Token make_token(TokenType type);
  Token make_token_if_else(u8 expected, TokenType match, TokenType other);
  Token make_comment();
  Token make_string();
  Token make_number();
  Token make_identifier();

  Token consume_token();

private:
  SourceLocation start_pos;
  SourceLocation curr_pos;
  std::string_view src;
  std::vector<std::string_view> string_data;
};

} // namespace lex
} // namespace neo
