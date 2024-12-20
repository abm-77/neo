#pragma once

#include "../common/types.h"
#include "src/frontend/lexer.h"

#include <string_view>
#include <vector>

namespace neo {
namespace parse {

using namespace lex;

enum Type : u8 {
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_BOOL,
  TYPE_VOID,
};

/*
 * Ast represents an abstract syntax tree. This is a read only data structure
 * that will be used for semantic analysis and, later, conversion into NeoIR.
 **/
class Ast {
public:
  // NodeType describes the type of Ast node.
  enum NodeType : u8 {
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

  /*
   * Node is is the unified type to represent AST nodes parsed from
   * a token source. Using the type field, one can interpret the use of the
   * lhs and rhs fields. Typically, the lhs and rhs fields are interpreted in
   * one of 3 ways:
   * 1. All data is held between lhs and rhs.
   * 2. Some basic data is stored in lhs, and rhs points to extra data.
   * 3. lhs and rhs outline the span of a collection of nodes.
   * */
  struct Node {
    NodeType type;
    i32 lhs;
    i32 rhs;
  };

  // Used to describe an empty or invalid AST node.
  static constexpr i32 NULL_NODE = -1;

public:
  using IntLitData = i32;
  using BoolLitData = b32;
  using StringData = std::string_view;
  using ArrayData = Span<i32>;

  struct VarDefData {
    Type type;
    i32 value;
    b32 is_const;
  };

  struct InfixData {
    TokenType op;
    i32 rhs;
  };

  struct PrefixData {
    TokenType prefix;
  };

  struct ForData {
    i32 cond;
    i32 cont;
    i32 blk;
  };

public:
  Ast();
  ~Ast();

  template <typename T> u32 alloc_data(T &data);
  template <typename T> u32 alloc_data(T *data, u32 count);

  i32 make_node(NodeType type, i32 lhs, i32 rhs);

  const std::vector<Node> &tree() const;
  const Node &root() const;
  const Node &at(i32 i) const;

  template <typename T> T get(Node node) const {
    return *reinterpret_cast<T *>(data + node.rhs);
  }

  template <> IntLitData get<IntLitData>(Node node) const {
    return static_cast<IntLitData>(node.lhs);
  }

  template <> BoolLitData get<BoolLitData>(Node node) const {
    return static_cast<BoolLitData>(node.lhs);
  }

  template <> ArrayData get<ArrayData>(Node node) const {
    return Span(reinterpret_cast<i32 *>(data + node.lhs), node.rhs);
  }

  template <> StringData get<StringData>(Node node) const {
    return *reinterpret_cast<StringData *>(data + node.lhs);
  }

private:
  u8 *data;
  u32 data_offset;
  u32 data_capacity;
  std::vector<Node> _tree;

private:
  static constexpr u32 default_extra_capacity = 4096;
};

/*
 * Parser takes the tokenized output of Lexer and parses it
 * into an abstract syntax tree.
 * */
class Parser {
public:
  // Create an AST from a LexerResult. Caller owns data.
  static Ast parse(const LexerResult &lex_res);

private:
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

private:
  Parser(Ast &ast, const LexerResult &lex_res);

  TokenType peek();
  b32 peek_is(TokenType type);
  Token next_token();

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

  Precedence infix_precedence(TokenType token);

private:
  Ast &ast;
  u32 token_idx;
  const LexerResult &lexer_result;
};

} // namespace parse
} // namespace neo
