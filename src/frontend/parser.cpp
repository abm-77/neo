#include <frontend/lexer.h>
#include <frontend/parser.h>

#include <cstdlib>
#include <optional>
#include <string.h>
#include <string_view>

namespace neo {
namespace parse {

Ast::Ast() : data_offset(0), data_capacity(default_extra_capacity) {
  data = static_cast<u8 *>(malloc(default_extra_capacity));
}

Ast::~Ast() { free(data); }

const std::vector<Ast::Node> &Ast::tree() const { return tree_; };

const Ast::Node &Ast::root() const { return tree_.back(); };

const Ast::Node &Ast::at(Ast::NodePtr i) const { return tree_.at(i); };

template <typename T> u32 Ast::alloc_data(T *src, u32 count) {
  u32 offset = data_offset;
  u32 size = sizeof(T) * count;

  // TODO: handle case where size is > 2x the current capacity
  if (data_offset + size > data_capacity) {
    data_capacity *= 2;
    data = static_cast<u8 *>(realloc(data, data_capacity));
  }
  data_offset += size;

  memcpy(data + offset, src, size);
  return offset;
}

template <typename T> u32 Ast::alloc_data(T &src) {
  u32 offset = data_offset;
  u32 size = sizeof(T);

  // TODO: handle case where size is > 2x the current capacity
  if (data_offset + size > data_capacity) {
    data_capacity *= 2;
    data = static_cast<u8 *>(realloc(data, data_capacity));
  }
  data_offset += size;

  memcpy(data + offset, &src, size);

  return offset;
}

Ast::NodePtr Ast::make_node(Ast::NodeType type, Ast::NodePtr lhs,
                            Ast::NodePtr rhs) {
  Ast::NodePtr idx = tree_.size();
  tree_.push_back(Ast::Node{type, lhs, rhs});
  return idx;
}

Parser::Precedence Parser::infix_precedence(TokenType token) {
  switch (token) {
  case TOKEN_PLUS_EQUAL:
  case TOKEN_MINUS_EQUAL:
  case TOKEN_SLASH_EQUAL:
  case TOKEN_STAR_EQUAL:
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
  case TOKEN_LEFT_BRACKET:
    return PRECEDENCE_INDEX;
  default:
    return PRECEDENCE_LOWEST;
  }
}

Parser::Parser(Ast &ast, const LexerResult &lex_res)
    : ast(ast), token_idx(0), lexer_result(std::move(lex_res)) {}

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

Ast::NodePtr Parser::parse_type() {
  using MetaType = Ast::TypeData::MetaType;

  auto token = next_token();
  switch (token.type) {
  case lex::TOKEN_INT: {
    Ast::TypeData data = {.meta_type = MetaType::SCALAR};
    return ast.make_node(Ast::AST_TYPE, TYPE_INT, ast.alloc_data(data));
  } break;
  case lex::TOKEN_FLOAT: {
    Ast::TypeData data = {.meta_type = MetaType::SCALAR};
    return ast.make_node(Ast::AST_TYPE, TYPE_FLOAT, ast.alloc_data(data));
  } break;
  case lex::TOKEN_BOOL: {
    Ast::TypeData data = {.meta_type = MetaType::SCALAR};
    return ast.make_node(Ast::AST_TYPE, TYPE_BOOL, ast.alloc_data(data));
  } break;
  case lex::TOKEN_VOID: {
    Ast::TypeData data = {.meta_type = MetaType::SCALAR};
    return ast.make_node(Ast::AST_TYPE, TYPE_VOID, ast.alloc_data(data));
  } break;
  case lex::TOKEN_LEFT_BRACKET: {
    auto capacity = consume_int_literal();
    if (!capacity.has_value())
      return Ast::NULL_NODE;
    consume(lex::TOKEN_RIGHT_BRACKET);

    auto type = parse_type();

    Ast::TypeData data = {.meta_type = MetaType::ARRAY};
    return ast.make_node(Ast::AST_TYPE, type, ast.alloc_data(data));
  } break;
  default:
    return Ast::NULL_NODE;
  }
}

Ast::NodePtr Parser::parse_ident_expr() {
  auto ident = consume_ident();
  return (ident.has_value()) ? ast.make_node(Ast::AST_IDENT_EXPR,
                                             ast.alloc_data(ident.value()), 0)
                             : Ast::NULL_NODE;
}

Ast::NodePtr Parser::parse_int_lit_expr() {
  auto data = consume_int_literal();
  return (data.has_value()) ? ast.make_node(Ast::AST_INT_LIT_EXPR, data.value(),
                                            Ast::NULL_NODE)
                            : Ast::NULL_NODE;
}

Ast::NodePtr Parser::parse_bool_lit_expr() {
  auto data = consume_bool_literal();
  return (data.has_value()) ? ast.make_node(Ast::AST_BOOL_LIT_EXPR,
                                            data.value(), Ast::NULL_NODE)
                            : Ast::NULL_NODE;
}

Ast::NodePtr Parser::parse_arr_lit_expr() {
  consume(lex::TOKEN_LEFT_BRACKET);
  std::optional<i32> capacity;
  if (peek_is(lex::TOKEN_INTEGER_LITERAL))
    capacity = consume_int_literal();
  consume(lex::TOKEN_RIGHT_BRACKET);

  auto el_type = parse_type();
  auto init_list =
      parse_expr_list(lex::TOKEN_LEFT_BRACE, lex::TOKEN_RIGHT_BRACE);

  i32 n = (capacity.has_value())
              ? capacity.value()
              : ast.get_array_of<Ast::NodePtr>(ast.at(init_list)).size();

  Ast::TypeData type_data{.meta_type = Ast::TypeData::MetaType::ARRAY};
  auto type = ast.make_node(Ast::AST_TYPE, el_type, ast.alloc_data(type_data));

  Ast::ArrayLitData data = {.type = type, .init_list = init_list};
  return ast.make_node(Ast::AST_ARR_LIT_EXPR, n,
                       ast.alloc_data<Ast::ArrayLitData>(data));
}

Ast::NodePtr Parser::parse_arr_index_expr(Ast::NodePtr arr) {
  consume(lex::TOKEN_LEFT_BRACKET);
  auto index = consume_int_literal();
  if (!index.has_value())
    return Ast::NULL_NODE;
  consume(lex::TOKEN_RIGHT_BRACKET);
  return ast.make_node(Ast::AST_ARR_INDEX_EXPR, arr, index.value());
}

Ast::NodePtr Parser::parse_infix_expr(Ast::NodePtr lhs) {
  auto op = next_token().type;
  auto precedence = infix_precedence(op);
  Ast::NodePtr rhs = parse_expr(precedence);
  Ast::InfixData data = {.op = op, .rhs = rhs};
  return ast.make_node(Ast::AST_INFIX_EXPR, lhs, ast.alloc_data(data));
}

Ast::NodePtr Parser::parse_grouped_expr() {
  consume(lex::TOKEN_LEFT_PAREN);
  Ast::NodePtr expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  return expr;
}

Ast::NodePtr Parser::parse_prefix_expr() {
  auto prefix = peek();
  consume(prefix);
  Ast::PrefixData data = {.prefix = prefix};
  return ast.make_node(Ast::AST_PREFIX_EXPR, parse_expr(PRECEDENCE_PREFIX),
                       ast.alloc_data(data));
}

Ast::NodePtr Parser::parse_func_call_expr(Ast::NodePtr fn) {
  auto args = parse_expr_list(lex::TOKEN_LEFT_PAREN, lex::TOKEN_RIGHT_PAREN);
  return ast.make_node(Ast::AST_FN_CALL_EXPR, fn, args);
}

Ast::NodePtr Parser::parse_expr(Precedence precedence) {
  // prefix
  Ast::NodePtr lhs;
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
  case lex::TOKEN_LEFT_BRACKET: {
    lhs = parse_arr_lit_expr();
  } break;

  default:
    lhs = Ast::NULL_NODE;
  }

  if (lhs == Ast::NULL_NODE)
    return Ast::NULL_NODE;

  // infix
  while (!peek_is(TOKEN_SEMICOLON) && precedence < infix_precedence(peek())) {
    Ast::NodePtr infix;
    switch (peek()) {
    case TOKEN_EQUAL:
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_SLASH:
    case TOKEN_STAR:
    case TOKEN_PLUS_EQUAL:
    case TOKEN_MINUS_EQUAL:
    case TOKEN_SLASH_EQUAL:
    case TOKEN_STAR_EQUAL:
    case TOKEN_EQUAL_EQUAL:
    case TOKEN_BANG_EQUAL:
    case TOKEN_GREATER_EQUAL:
    case TOKEN_LESS_EQUAL:
    case TOKEN_GREATER:
    case TOKEN_LESS:
      infix = parse_infix_expr(lhs);
      break;
    case TOKEN_LEFT_PAREN:
      infix = parse_func_call_expr(lhs);
      break;
    case TOKEN_LEFT_BRACKET:
      infix = parse_arr_index_expr(lhs);
      break;
    default:
      infix = Ast::NULL_NODE;
    }

    if (infix == Ast::NULL_NODE)
      return lhs;
    lhs = infix;
  }

  return lhs;
}

Ast::NodePtr Parser::parse_expr_list(TokenType start, TokenType end) {
  std::vector<Ast::NodePtr> exprs;
  consume(start);
  if (!peek_is(end)) {
    exprs.push_back(parse_expr(PRECEDENCE_LOWEST));
    while (peek_is(lex::TOKEN_COMMA)) {
      consume(lex::TOKEN_COMMA);
      exprs.push_back(parse_expr(PRECEDENCE_LOWEST));
    }
  }
  consume(end);
  return ast.make_node(
      Ast::AST_SPAN, ast.alloc_data(exprs.data(), exprs.size()), exprs.size());
}

Ast::NodePtr Parser::parse_return_stmt() {
  consume(lex::TOKEN_RETURN);
  auto expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_SEMICOLON);
  return ast.make_node(Ast::AST_RET_STMT, expr, Ast::NULL_NODE);
}

Ast::NodePtr Parser::parse_if_stmt() {
  std::vector<Ast::NodePtr> conds;
  std::vector<Ast::NodePtr> blks;
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

  auto ast_conds = ast.make_node(
      Ast::AST_IF_CONDS,
      ast.alloc_data<Ast::NodePtr>(conds.data(), conds.size()), conds.size());

  auto ast_blks = ast.make_node(
      Ast::AST_IF_BLKS, ast.alloc_data<Ast::NodePtr>(blks.data(), blks.size()),
      blks.size());

  return ast.make_node(Ast::AST_IF_STMT, ast_conds, ast_blks);
}

Ast::NodePtr Parser::parse_for_stmt() {
  consume(lex::TOKEN_CTRL_FOR);
  consume(lex::TOKEN_LEFT_PAREN);
  auto init = parse_statement();
  auto cond = parse_statement();
  auto cont = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  auto blk = parse_blk_stmt();
  Ast::ForData data = {.cond = cond, .cont = cont, .blk = blk};
  return ast.make_node(Ast::AST_FOR_STMT, init, ast.alloc_data(data));
}

Ast::NodePtr Parser::parse_while_stmt() {
  consume(lex::TOKEN_CTRL_WHILE);
  consume(lex::TOKEN_LEFT_PAREN);
  auto cond = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_RIGHT_PAREN);
  auto blk = parse_blk_stmt();
  return ast.make_node(Ast::AST_WHILE_STMT, cond, blk);
}

Ast::NodePtr Parser::parse_var_def_stmt() {
  b32 is_const = peek_is(lex::TOKEN_CONST);
  if (is_const) {
    consume(lex::TOKEN_CONST);
  } else {
    consume(lex::TOKEN_VAR);
  };

  Ast::NodePtr ident = parse_ident_expr();

  // TODO: handle type inference
  std::optional<i32> type;
  if (peek_is(lex::TOKEN_COLON)) {
    consume(lex::TOKEN_COLON);
    type = parse_type();
  }

  consume(lex::TOKEN_EQUAL);
  Ast::NodePtr expr = parse_expr(PRECEDENCE_LOWEST);
  consume(lex::TOKEN_SEMICOLON);

  Ast::VarDefData data = {
      .type = type.value(), .value = expr, .is_const = is_const};
  return ast.make_node(Ast::AST_VAR_DEF_STMT, ident, ast.alloc_data(data));
}

Ast::NodePtr Parser::parse_blk_stmt() {
  std::vector<Ast::NodePtr> stmts;
  consume(lex::TOKEN_LEFT_BRACE);
  while (!peek_is(lex::TOKEN_RIGHT_BRACE) && !peek_is(lex::TOKEN_EOF)) {
    Ast::NodePtr statement = parse_statement();
    stmts.push_back(statement);
  }
  consume(lex::TOKEN_RIGHT_BRACE);
  return ast.make_node(Ast::AST_SPAN,
                       ast.alloc_data<Ast::NodePtr>(stmts.data(), stmts.size()),
                       stmts.size());
}

Ast::NodePtr Parser::parse_expr_stmt() {
  Ast::NodePtr expr = parse_expr(PRECEDENCE_LOWEST);
  Ast::NodePtr idx = ast.make_node(Ast::AST_EXPR_STMT, expr, 0);
  consume(lex::TOKEN_SEMICOLON);
  return idx;
}

Ast::NodePtr Parser::parse_func_def_stmt() {
  using FuncParam = Ast::FuncDefData::FuncParam;
  consume(lex::TOKEN_FUNC);
  auto name = parse_ident_expr();

  std::vector<FuncParam> params;
  consume(lex::TOKEN_LEFT_PAREN);
  while (!peek_is(lex::TOKEN_RIGHT_PAREN)) {
    auto param_name = parse_ident_expr();
    consume(lex::TOKEN_COLON);

    auto param_type = parse_type();
    params.push_back({.ident = param_name, .type = param_type});
    if (peek_is(lex::TOKEN_COMMA)) {
      consume(TOKEN_COMMA);
    } else {
      break;
    }
  }
  consume(lex::TOKEN_RIGHT_PAREN);

  auto ret_type = parse_type();
  auto blk = parse_blk_stmt();
  auto param_list = ast.make_node(
      Ast::AST_SPAN, ast.alloc_data<FuncParam>(params.data(), params.size()),
      params.size());
  Ast::FuncDefData data = {
      .ret_type = ret_type, .params = param_list, .blk = blk};
  return ast.make_node(Ast::AST_FN_DEF_STMT, name, ast.alloc_data(data));
}

Ast::NodePtr Parser::parse_statement() {
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
  case lex::TOKEN_FUNC:
    return parse_func_def_stmt();
  default:
    return parse_expr_stmt();
  }
}

Ast Parser::parse(const LexerResult &lex_result) {
  Ast A;
  Parser P(A, lex_result);
  while (P.token_idx < P.lexer_result.tokens.size()) {
    P.parse_statement();
  }
  return A;
}

} // namespace parse
} // namespace neo
