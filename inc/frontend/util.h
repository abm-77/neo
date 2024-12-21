#pragma once

#include "lexer.h"
#include "parser.h"

namespace neo {

namespace lex {
[[maybe_unused]]
static const char *token_type_to_string(TokenType type) {
  switch (type) {
  case TOKEN_LEFT_PAREN:
    return "TOKEN_LEFT_PAREN";
  case TOKEN_RIGHT_PAREN:
    return "TOKEN_RIGHT_PAREN";
  case TOKEN_LEFT_BRACE:
    return "TOKEN_LEFT_BRACE";
  case TOKEN_RIGHT_BRACE:
    return "TOKEN_RIGHT_BRACE";
  case TOKEN_LEFT_BRACKET:
    return "TOKEN_LEFT_BRACKET";
  case TOKEN_RIGHT_BRACKET:
    return "TOKEN_RIGHT_BRACKET";
  case TOKEN_COMMA:
    return "TOKEN_COMMA";
  case TOKEN_DOT:
    return "TOKEN_DOT";
  case TOKEN_SEMICOLON:
    return "TOKEN_SEMICOLON";
  case TOKEN_COLON:
    return "TOKEN_COLON";
  case TOKEN_MINUS:
    return "TOKEN_MINUS";
  case TOKEN_MINUS_EQUAL:
    return "TOKEN_MINUS_EQUAL";
  case TOKEN_PLUS:
    return "TOKEN_PLUS";
  case TOKEN_PLUS_EQUAL:
    return "TOKEN_PLUS_EQUAL";
  case TOKEN_SLASH:
    return "TOKEN_SLASH";
  case TOKEN_SLASH_EQUAL:
    return "TOKEN_SLASH_EQUAL";
  case TOKEN_STAR:
    return "TOKEN_STAR";
  case TOKEN_STAR_EQUAL:
    return "TOKEN_STAR_EQUAL";
  case TOKEN_PERCENT:
    return "TOKEN_PERCENT";
  case TOKEN_PERCENT_EQUAL:
    return "TOKEN_PERCENT_EQUAL";
  case TOKEN_BANG:
    return "TOKEN_BANG";
  case TOKEN_BANG_EQUAL:
    return "TOKEN_BANG_EQUAL";
  case TOKEN_EQUAL:
    return "TOKEN_EQUAL";
  case TOKEN_EQUAL_EQUAL:
    return "TOKEN_EQUAL_EQUAL";
  case TOKEN_GREATER:
    return "TOKEN_GREATER";
  case TOKEN_GREATER_EQUAL:
    return "TOKEN_GREATER_EQUAL";
  case TOKEN_LESS:
    return "TOKEN_LESS";
  case TOKEN_LESS_EQUAL:
    return "TOKEN_LESS_EQUAL";
  case TOKEN_COMMENT:
    return "TOKEN_COMMENT";
  case TOKEN_SPACE:
    return "TOKEN_SPACE";
  case TOKEN_TYPE:
    return "TOKEN_TYPE";
  case TOKEN_EOF:
    return "TOKEN_EOF";
  case TOKEN_INVALID:
    return "TOKEN_INVALID";
  case TOKEN_VOID:
    return "TOKEN_VOID";
  case TOKEN_CONST:
    return "TOKEN_CONST";
  case TOKEN_VAR:
    return "TOKEN_VAR";
  case TOKEN_INT:
    return "TOKEN_INT";
  case TOKEN_IMPORT:
    return "TOKEN_IMPORT";
  case TOKEN_EXPORT:
    return "TOKEN_EXPORT";
  case TOKEN_EXTERN:
    return "TOKEN_EXTERN";
  case TOKEN_STRUCT:
    return "TOKEN_STRUCT";
  case TOKEN_FUNC:
    return "TOKEN_FUNC";
  case TOKEN_CTRL_FOR:
    return "TOKEN_CTRL_FOR";
  case TOKEN_CTRL_WHILE:
    return "TOKEN_CTRL_WHILE";
  case TOKEN_CTRL_BREAK:
    return "TOKEN_CTRL_BREAK";
  case TOKEN_CTRL_CONTINUE:
    return "TOKEN_CTRL_CONTINUE";
  case TOKEN_COND_IF:
    return "TOKEN_COND_IF";
  case TOKEN_COND_ELSE:
    return "TOKEN_COND_ELSE";
  case TOKEN_BOOL:
    return "TOKEN_BOOL";
  case TOKEN_BOOL_OR:
    return "TOKEN_BOOL_OR";
  case TOKEN_BOOL_AND:
    return "TOKEN_BOOL_AND";
  case TOKEN_RETURN:
    return "TOKEN_RETURN";
  case TOKEN_IDENTIFIER:
    return "TOKEN_IDENTIFIER";
  case TOKEN_INTEGER_LITERAL:
    return "TOKEN_INTEGER_LITERAL";
  case TOKEN_STRING_LITERAL:
    return "TOKEN_STRING_LITERAL";
  case TOKEN_STRING_LITERAL_UNTERMINATED:
    return "TOKEN_STRING_LITERAL_UNTERMINATED";
  case TOKEN_BOOL_LITERAL:
    return "TOKEN_BOOL_LITERAL";
  default:
    return "UNKNOWN_TOKEN";
  }
}
} // namespace lex

namespace parse {
[[maybe_unused]]
static const char *type_to_string(ValueType type) {
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

[[maybe_unused]]
static const char *node_type_to_string(Ast::NodeType type) {
  switch (type) {
  // statements
  case Ast::AST_EXPR_STMT:
    return "Ast::AST_EXPR_STMT";
  case Ast::AST_VAR_DEF_STMT:
    return "Ast::AST_VAR_DEF_STMT";
  case Ast::AST_SPAN:
    return "Ast::AST_SPAN";
  case Ast::AST_TYPE:
    return "Ast::AST_TYPE";
  case Ast::AST_RET_STMT:
    return "Ast::AST_RET_STMT";
  case Ast::AST_IF_STMT:
    return "Ast::AST_IF_STMT";
  case Ast::AST_IF_CONDS:
    return "Ast::AST_IF_CONDS";
  case Ast::AST_IF_BLKS:
    return "Ast::AST_IF_BLKS";
  case Ast::AST_WHILE_STMT:
    return "Ast::AST_WHILE_STMT";
  case Ast::AST_FOR_STMT:
    return "Ast::AST_FOR_STMT";
  case Ast::AST_FN_DEF_STMT:
    return "Ast::AST_FN_DEF_STMT";

  // expressions
  case Ast::AST_IDENT_EXPR:
    return "Ast::AST_IDENT_EXPR";
  case Ast::AST_INT_LIT_EXPR:
    return "Ast::AST_INT_LIT_EXPR";
  case Ast::AST_BOOL_LIT_EXPR:
    return "Ast::AST_BOOL_LIT_EXPR";
  case Ast::AST_STR_LIT_EXPR:
    return "Ast::AST_STR_LIT_EXPR";
  case Ast::AST_PREFIX_EXPR:
    return "Ast::AST_PREFIX_EXPR";
  case Ast::AST_INFIX_EXPR:
    return "Ast::AST_INFIX_EXPR";
  case Ast::AST_FN_CALL_EXPR:
    return "Ast::AST_FN_CALL_EXPR";

  default:
    return "UNKNOWN_Ast::AST_NODE_TYPE";
  }
}

} // namespace parse
} // namespace neo
