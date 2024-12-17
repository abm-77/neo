#include "parser.h"
#include "src/frontend/lexer.h"
#include <cstdlib>
#include <optional>
#include <string.h>
#include <string_view>

namespace neo {
namespace parse {

using namespace ast_data;

const char *node_type_to_string(AstNodeType type) {
  switch (type) {
  // statements
  case AST_EXPR_STMT:
    return "AST_EXPR_STMT";
  case AST_VAR_DEF_STMT:
    return "AST_VAR_DEF_STMT";
  case AST_VAR_ASSIGN_STMT:
    return "AST_VAR_ASSIGN_STMT";
  case AST_BLK_STMT:
    return "AST_BLK_STMT";
  case AST_RET_STMT:
    return "AST_RET_STMT";
  case AST_IF_STMT:
    return "AST_IF_STMT";
  case AST_IF_CONDS:
    return "AST_IF_CONDS";
  case AST_IF_BLKS:
    return "AST_IF_BLKS";
  case AST_WHILE_STMT:
    return "AST_WHILE_STMT";
  case AST_FOR_STMT:
    return "AST_FOR_STMT";
  case AST_FN_DEF_STMT:
    return "AST_FN_DEF_STMT";

  // expressions
  case AST_IDENT_EXPR:
    return "AST_IDENT_EXPR";
  case AST_INT_LIT_EXPR:
    return "AST_INT_LIT_EXPR";
  case AST_BOOL_LIT_EXPR:
    return "AST_BOOL_LIT_EXPR";
  case AST_STR_LIT_EXPR:
    return "AST_STR_LIT_EXPR";
  case AST_PREFIX_EXPR:
    return "AST_PREFIX_EXPR";
  case AST_INFIX_EXPR:
    return "AST_INFIX_EXPR";
  case AST_FN_CALL_EXPR:
    return "AST_FN_CALL_EXPR";

  default:
    return "UNKNOWN_AST_NODE_TYPE";
  }
}

const char *type_to_string(Type type) {
  switch (type) {
  case TYPE_INT:
    return "TYPE_INT";
  case TYPE_FLOAT:
    return "TYPE_FLOAT";
  case TYPE_BOOL:
    return "TYPE_BOOL";
  case TYPE_VOID:
    return "TYPE_VOID";
  default:
    return "UNKNOWN_TYPE";
  }
}

Precedence infix_precedence(TokenType token) {
  switch (token) {
  case TOKEN_EQUAL:
    return PRECEDENCE_ASSIGN;
  case TOKEN_BANG_EQUAL:
  case TOKEN_LESS_EQUAL:
  case TOKEN_GREATER_EQUAL:
  case TOKEN_EQUAL_EQUAL:
    return PRECEDENCE_EQ;
  case TOKEN_LESS:
  case TOKEN_GREATER:
    return PRECEDENCE_LT_GT;
  case TOKEN_PLUS:
  case TOKEN_MINUS:
    return PRECEDENCE_SUM;
  case TOKEN_STAR:
  case TOKEN_SLASH:
    return PRECEDENCE_PRODUCT;
  case TOKEN_LEFT_PAREN:
    return PRECEDENCE_CALL;
  default:
    return PRECEDENCE_LOWEST;
  }
}

Parser::Parser(const LexerResult &lex_res)
    : lexer_result(std::move(lex_res)), token_idx(0), extra_offset(0),
      extra_capacity(default_extra_capacity) {
  extra = static_cast<u8 *>(malloc(default_extra_capacity));
}

Parser::~Parser() { free(extra); }

TokenType Parser::peek() {
  return (token_idx < lexer_result.tokens.size())
             ? lexer_result.tokens.at(token_idx).type
             : TOKEN_EOF;
}

b32 Parser::peek_is(TokenType type) { return peek() == type; }

Token Parser::next_token() {
  auto token = lexer_result.tokens.at(token_idx++);
  return token;
}

std::optional<Token> Parser::consume(TokenType type) {
  auto token = next_token();
  return (token.type == type) ? std::optional(token) : std::nullopt;
}

std::optional<std::string_view> Parser::consume_ident() {
  auto token = consume(lex::TOKEN_IDENTIFIER);
  return (token.has_value())
             ? std::optional(lexer_result.get_string_data(token.value()))
             : std::nullopt;
}

std::optional<i64> Parser::consume_int_literal() {
  auto token = consume(lex::TOKEN_INTEGER_LITERAL);
  return (token.has_value()) ? std::optional(static_cast<i64>(token->data))
                             : std::nullopt;
}

std::optional<b32> Parser::consume_bool_literal() {
  auto token = consume(lex::TOKEN_BOOL_LITERAL);
  return (token.has_value()) ? std::optional(static_cast<b32>(token->data))
                             : std::nullopt;
}

std::optional<Type> Parser::consume_type() {
  auto token = next_token();
  switch (token.type) {
  case lex::TOKEN_INT:
    return TYPE_INT;
  case lex::TOKEN_FLOAT:
    return TYPE_FLOAT;
  case lex::TOKEN_BOOL:
    return TYPE_BOOL;
  case lex::TOKEN_VOID:
    return TYPE_VOID;
  default:
    return std::nullopt;
  }
}

template <typename T> u32 Parser::alloc_data(T *data, u32 count) {
  u32 offset = extra_offset;
  u32 size = sizeof(T) * count;

  // TODO: handle case where size is > 2x the current capacity
  if (extra_offset + size > extra_capacity) {
    extra_capacity *= 2;
    extra = static_cast<u8 *>(realloc(extra, extra_capacity));
  }
  extra_offset += size;

  memcpy(extra + offset, data, size);
  return offset;
}

template <typename T> u32 Parser::alloc_data(T &data) {
  u32 offset = extra_offset;
  u32 size = sizeof(T);

  // TODO: handle case where size is > 2x the current capacity
  if (extra_offset + size > extra_capacity) {
    extra_capacity *= 2;
    extra = static_cast<u8 *>(realloc(extra, extra_capacity));
  }
  extra_offset += size;

  memcpy(extra + offset, &data, size);

  return offset;
}

i32 Parser::make_node(AstNodeType type, i32 lhs, i32 rhs) {
  i32 idx = ast.size();
  ast.push_back(AstNode{type, lhs, rhs});
  return idx;
}

i32 Parser::parse_ident_expr() {
  auto ident = consume_ident();
  if (ident.has_value()) {
    u32 ident_data = alloc_data(ident.value());
    return make_node(AST_IDENT_EXPR, ident_data, 0);
  } else {
    return NULL_NODE;
  }
}

i32 Parser::parse_int_lit_expr() {
  auto data = consume_int_literal();
  return (data.has_value())
             ? make_node(AST_INT_LIT_EXPR, data.value(), NULL_NODE)
             : NULL_NODE;
}

i32 Parser::parse_bool_lit_expr() {
  auto data = consume_bool_literal();
  return (data.has_value())
             ? make_node(AST_BOOL_LIT_EXPR, data.value(), NULL_NODE)
             : NULL_NODE;
}

i32 Parser::parse_infix_expr(i32 lhs) {
  auto op = peek();
  auto precedence = infix_precedence(op);
  consume(op);
  i32 rhs = parse_expr(precedence);
  AstInfixData data = {.op = op, .rhs = rhs};
  return make_node(AST_INFIX_EXPR, lhs, alloc_data(data));
}

i32 Parser::parse_grouped_expr() {
  consume(lex::TOKEN_LEFT_PAREN);
  i32 expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  return expr;
}

i32 Parser::parse_prefix_expr() {
  auto prefix = peek();
  consume(prefix);
  AstPrefixData data = {.prefix = prefix};
  return make_node(AST_PREFIX_EXPR, parse_expr(PRECEDENCE_PREFIX),
                   alloc_data(data));
}

i32 Parser::parse_expr(Precedence precedence) {
  // prefix
  i32 lhs;
  switch (peek()) {
  case lex::TOKEN_IDENTIFIER: {
    lhs = parse_ident_expr();
  } break;
  case lex::TOKEN_INTEGER_LITERAL: {
    lhs = parse_int_lit_expr();
  } break;
  case lex::TOKEN_BOOL_LITERAL: {
    lhs = parse_bool_lit_expr();
  } break;
  case lex::TOKEN_BANG:
  case lex::TOKEN_MINUS: {
    lhs = parse_prefix_expr();
  } break;
  case lex::TOKEN_LEFT_PAREN: {
    lhs = parse_grouped_expr();
  } break;

  default:
    lhs = NULL_NODE;
  }

  if (lhs == NULL_NODE)
    return NULL_NODE;

  // infix
  while (!peek_is(TOKEN_SEMICOLON) && precedence < infix_precedence(peek())) {
    i32 infix;
    switch (peek()) {
    case TOKEN_EQUAL:
    case TOKEN_PLUS_EQUAL:
    case TOKEN_MINUS_EQUAL:
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_SLASH:
    case TOKEN_STAR:
    case TOKEN_EQUAL_EQUAL:
    case TOKEN_BANG_EQUAL:
    case TOKEN_GREATER_EQUAL:
    case TOKEN_LESS_EQUAL:
    case TOKEN_GREATER:
    case TOKEN_LESS:
      infix = parse_infix_expr(lhs);
      break;
    /*case TOKEN_LEFT_PAREN:*/
    /*  infix = parse_call_expr(lhs);*/
    /*  break;*/
    default:
      infix = NULL_NODE;
    }

    if (infix == NULL_NODE)
      return lhs;
    lhs = infix;
  }

  return lhs;
}

i32 Parser::parse_return_stmt() {
  consume(lex::TOKEN_RETURN);
  auto expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_SEMICOLON);
  return make_node(AST_RET_STMT, expr, NULL_NODE);
}

i32 Parser::parse_if_stmt() {
  std::vector<i32> conds;
  std::vector<i32> blks;
  b32 has_default = false;
  do {
    consume(lex::TOKEN_COND_IF);
    consume(lex::TOKEN_LEFT_PAREN);
    conds.push_back(parse_expr(PRECEDENCE_LOWEST));
    consume(lex::TOKEN_RIGHT_PAREN);

    blks.push_back(parse_blk_stmt());
    if (peek_is(lex::TOKEN_COND_ELSE)) {
      consume(lex::TOKEN_COND_ELSE);
      if (!peek_is(lex::TOKEN_COND_IF)) {
        has_default = true;
        break;
      }
    } else {
      break;
    }
  } while (true);

  if (has_default)
    blks.push_back(parse_blk_stmt());

  auto ast_conds = make_node(
      AST_IF_CONDS, alloc_data<i32>(conds.data(), conds.size()), conds.size());

  auto ast_blks = make_node(
      AST_IF_BLKS, alloc_data<i32>(blks.data(), blks.size()), blks.size());

  return make_node(AST_IF_STMT, ast_conds, ast_blks);
}

i32 Parser::parse_for_stmt() {
  consume(lex::TOKEN_CTRL_FOR);
  consume(lex::TOKEN_LEFT_PAREN);
  auto init = parse_statement();
  auto cond = parse_statement();
  auto cont = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  auto blk = parse_blk_stmt();
  AstForData data = {.cond = cond, .cont = cont, .blk = blk};
  return make_node(AST_FOR_STMT, init, alloc_data(data));
}

i32 Parser::parse_while_stmt() {
  consume(lex::TOKEN_CTRL_WHILE);
  consume(lex::TOKEN_LEFT_PAREN);
  auto cond = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  auto blk = parse_blk_stmt();
  return make_node(AST_WHILE_STMT, cond, blk);
}

i32 Parser::parse_var_def_stmt() {
  b32 is_const = peek_is(lex::TOKEN_CONST);
  if (is_const) {
    consume(lex::TOKEN_CONST);
  } else {
    consume(lex::TOKEN_VAR);
  };

  i32 ident = parse_ident_expr();

  // TODO: handle type inference
  std::optional<Type> type;
  if (peek_is(lex::TOKEN_COLON)) {
    consume(lex::TOKEN_COLON);
    type = consume_type();
  }

  consume(lex::TOKEN_EQUAL);
  i32 expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_SEMICOLON);

  AstVarDefData data = {
      .type = type.value(), .value = expr, .is_const = is_const};
  return make_node(AST_VAR_DEF_STMT, ident, alloc_data(data));
}

i32 Parser::parse_blk_stmt() {
  std::vector<i32> stmts;
  consume(lex::TOKEN_LEFT_BRACE);
  while (!peek_is(lex::TOKEN_RIGHT_BRACE) && !peek_is(lex::TOKEN_EOF)) {
    i32 statement = parse_statement();
    stmts.push_back(statement);
  }
  consume(lex::TOKEN_RIGHT_BRACE);
  return make_node(AST_BLK_STMT, alloc_data<i32>(stmts.data(), stmts.size()),
                   stmts.size());
}

i32 Parser::parse_expr_stmt() {
  i32 expr = parse_expr(PRECEDENCE_LOWEST);
  i32 idx = make_node(AST_EXPR_STMT, expr, 0);
  consume(lex::TOKEN_SEMICOLON);
  return idx;
}

i32 Parser::parse_statement() {
  switch (peek()) {
  case lex::TOKEN_CONST:
  case lex::TOKEN_VAR:
    return parse_var_def_stmt();
  case lex::TOKEN_LEFT_BRACE:
    return parse_blk_stmt();
  case lex::TOKEN_RETURN:
    return parse_return_stmt();
  case lex::TOKEN_COND_IF:
    return parse_if_stmt();
  case lex::TOKEN_CTRL_FOR:
    return parse_for_stmt();
  case lex::TOKEN_CTRL_WHILE:
    return parse_while_stmt();
  /*case lex::TOKEN_FUNC:*/
  /*  return parse_func_def_stmt();*/
  default:
    return parse_expr_stmt();
  }
}

std::vector<AstNode> Parser::parse() {
  while (token_idx < lexer_result.tokens.size()) {
    parse_statement();
  }
  return ast;
}

} // namespace parse

} // namespace neo
