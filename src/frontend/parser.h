#pragma once

#include "../common/types.h"
#include "src/frontend/lexer.h"

#include <string_view>
#include <vector>

namespace neo {
namespace parse {

using namespace lex;

enum AstNodeType : u8 {
  // statements
  AST_EXPR_STMT,       // lhs = expr
  AST_VAR_DEF_STMT,    // lhs = ident, rhs -> { value, type, is_const }
  AST_VAR_ASSIGN_STMT, // lhs = ident, rhs = value
  AST_BLK_STMT,        // lhs -> statements begin, rhs = statements length
  AST_RET_STMT,        // lhs = return value
  AST_IF_STMT,         // lhs -> conds, rhs -> blks
  AST_IF_CONDS,        // lhs -> conds begin, rhs = conds length
  AST_IF_BLKS,         // lhs -> blks begin, rhs = blks length
  AST_WHILE_STMT,      // lhs = cond, rhs = blk
  AST_FOR_STMT,        // lhs = init, rhs -> { cond, cont, blk }
  AST_FN_DEF_STMT,

  // expressions
  AST_IDENT_EXPR,    // lhs -> { lexeme }
  AST_INT_LIT_EXPR,  // lhs = value
  AST_BOOL_LIT_EXPR, // lhs = value
  AST_STR_LIT_EXPR,  // lhs -> { string }
  AST_PREFIX_EXPR,   // lhs = value, rhs -> { prefix }
  AST_INFIX_EXPR,    // lhs = lhs (of binop), rhs -> { op, rhs }
  AST_FN_CALL_EXPR,
};

const char *node_type_to_string(AstNodeType type);
constexpr i32 NULL_NODE = -1;

enum Precedence : u8 {
  PRECEDENCE_LOWEST,
  PRECEDENCE_ASSIGN,
  PRECEDENCE_EQ,
  PRECEDENCE_LT_GT,
  PRECEDENCE_SUM,
  PRECEDENCE_PRODUCT,
  PRECEDENCE_PREFIX,
  PRECEDENCE_CALL,
};

enum Type : u8 {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_BOOL,
  TYPE_VOID,
};
const char *type_to_string(Type type);

struct AstNode {
  AstNodeType type;
  i32 lhs;
  i32 rhs;
};

namespace ast_data {
struct AstStringData {
  std::string_view data;
};

struct AstVarDefData {
  Type type;
  i32 value; // expr
  b32 is_const;
};

struct AstInfixData {
  TokenType op;
  i32 rhs;
};

struct AstPrefixData {
  TokenType prefix;
};

struct AstForData {
  i32 cond;
  i32 cont;
  i32 blk;
};

struct AstArrayData {
  Span<i32> items;
};
} // namespace ast_data

class Parser {
public:
  Parser(const LexerResult &lex_res);
  ~Parser();

  std::vector<AstNode> parse();

  template <typename T> T *get_data(AstNode node) const {
    switch (node.type) {
    case AST_IDENT_EXPR:
      return reinterpret_cast<T *>(extra + node.lhs);
    default:
      return reinterpret_cast<T *>(extra + node.rhs);
    }
  }

  ast_data::AstArrayData get_array_data(AstNode node) const {
    Span span(reinterpret_cast<i32 *>(extra + node.lhs), node.rhs);
    return ast_data::AstArrayData{
        .items = span,
    };
  }

private:
  TokenType peek();
  b32 peek_is(TokenType type);
  Token next_token();

  i32 make_node(AstNodeType type, i32 lhs, i32 rhs);

  std::optional<Token> consume(TokenType token);
  std::optional<std::string_view> consume_ident();
  std::optional<Type> consume_type();
  std::optional<std::string_view> consume_string_literal();
  std::optional<i64> consume_int_literal();
  std::optional<b32> consume_bool_literal();

  i32 parse_expr_stmt();
  i32 parse_ident_expr();
  i32 parse_statement();
  i32 parse_var_def_stmt();
  i32 parse_return_stmt();
  i32 parse_if_stmt();
  i32 parse_blk_stmt();
  i32 parse_for_stmt();
  i32 parse_while_stmt();
  i32 parse_func_def_stmt();

  i32 parse_expr(Precedence precedence);
  i32 parse_int_lit_expr();
  i32 parse_bool_lit_expr();
  i32 parse_grouped_expr();
  i32 parse_prefix_expr();
  i32 parse_call_expr(i32 lhs);
  i32 parse_infix_expr(i32 lhs);

  template <typename T> u32 alloc_data(T &data);
  template <typename T> u32 alloc_data(T *data, u32 count);

private:
  std::vector<AstNode> ast;
  const LexerResult &lexer_result;

  u32 token_idx;

  // TODO: see about extracting this out into either a more generic allocator
  // or some sort of AstContext class
  u8 *extra;
  u32 extra_offset;
  u32 extra_capacity;

private:
  static constexpr u32 default_extra_capacity = 4096;
};

} // namespace parse
} // namespace neo
