#include "lexer.h"
#include "parser.h"

#include <gtest/gtest.h>
#include <ostream>
#include <vector>

using namespace neo::lex;
using namespace neo::parse;
using namespace neo::parse::ast_data;

void debug_print_ast(const std::vector<AstNode> &ast) {
  for (u32 i = 0; i < ast.size(); i++) {
    std::cout << i << " : " << node_type_to_string(ast.at(i).type) << std::endl;
  }
}
TEST(Parser, ParseExprStmt) {
  auto input = "a;";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto stmt = ast.back();
  EXPECT_EQ(stmt.type, AST_EXPR_STMT);
  EXPECT_EQ(ast.at(stmt.lhs).type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(ast.at(stmt.lhs))->data, "a");
}

TEST(Parser, ParseRetStmt) {
  auto input = "return 7;";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto stmt = ast.back();
  EXPECT_EQ(stmt.type, AST_RET_STMT);
  EXPECT_EQ(ast.at(stmt.lhs).type, AST_INT_LIT_EXPR);
  EXPECT_EQ(ast.at(stmt.lhs).lhs, 7);
}

TEST(Parser, ParseVarDefStmtConst) {
  auto input = "const a: int = 123;";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto var_def_stmt = ast.back();
  EXPECT_EQ(var_def_stmt.type, AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(ident_stmt)->data, "a");

  auto var_def_data = P.get_data<AstVarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data->type, TYPE_INT);
  EXPECT_EQ(var_def_data->is_const, true);

  auto value = ast.at(var_def_data->value);
  EXPECT_EQ(value.lhs, 123);
}

TEST(Parser, ParseVarDefStmtVar) {
  auto input = "var a: int = -1001;";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto var_def_stmt = ast.back();
  EXPECT_EQ(var_def_stmt.type, AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(ident_stmt)->data, "a");

  auto var_def_data = P.get_data<AstVarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data->type, TYPE_INT);
  EXPECT_EQ(var_def_data->is_const, false);

  auto value = ast.at(var_def_data->value);
  EXPECT_EQ(value.lhs, -1001);
}

TEST(Parser, ParseVarDefStmtBool) {
  auto input = "const a: bool = true;";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto var_def_stmt = ast.back();
  EXPECT_EQ(var_def_stmt.type, AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(ident_stmt)->data, "a");

  auto var_def_data = P.get_data<AstVarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data->type, TYPE_BOOL);
  EXPECT_EQ(var_def_data->is_const, true);

  auto value = ast.at(var_def_data->value);
  EXPECT_EQ(value.lhs, true);
}

TEST(Parser, ParsePrefixGroupedExpr) {
  auto input = "var a: int = -(b + c);";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto var_def_stmt = ast.back();
  EXPECT_EQ(var_def_stmt.type, AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(ident_stmt)->data, "a");

  auto var_def_data = P.get_data<AstVarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data->type, TYPE_INT);
  EXPECT_EQ(var_def_data->is_const, false);

  auto prefix_expr = ast.at(var_def_data->value);
  EXPECT_EQ(prefix_expr.type, AST_PREFIX_EXPR);
  EXPECT_EQ(P.get_data<AstPrefixData>(prefix_expr)->prefix, TOKEN_MINUS);

  auto infix_expr = ast.at(prefix_expr.lhs);
  EXPECT_EQ(infix_expr.type, AST_INFIX_EXPR);

  auto lhs = ast.at(infix_expr.lhs);
  EXPECT_EQ(lhs.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(lhs)->data, "b");

  auto infix_data = P.get_data<AstInfixData>(infix_expr);
  EXPECT_EQ(infix_data->op, TOKEN_PLUS);

  auto rhs = ast.at(infix_data->rhs);
  EXPECT_EQ(rhs.type, AST_IDENT_EXPR);
  EXPECT_EQ(P.get_data<AstStringData>(rhs)->data, "c");
}

TEST(Parser, ParseBlkStmt) {
  auto input = "{ 0; 1; 2; }";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();

  auto blk_stmt = ast.back();
  EXPECT_EQ(blk_stmt.type, AST_BLK_STMT);

  auto blk_data = P.get_array_data(blk_stmt);
  for (u32 i = 0; i < blk_data.items.size(); i++) {
    auto expr_stmt = ast.at(blk_data.items[i]);
    auto int_lit = ast.at(expr_stmt.lhs).lhs;
    EXPECT_EQ(int_lit, i);
  }
}

void validate_if_stmt(const Parser &P, const std::vector<AstNode> &ast) {
  auto if_stmt = ast.back();
  EXPECT_EQ(if_stmt.type, AST_IF_STMT);

  auto conds = P.get_array_data(ast.at(if_stmt.lhs));
  for (u32 i = 0; i < conds.items.size(); i++) {
    auto cond_expr = ast.at(conds.items[i]);
    EXPECT_EQ(cond_expr.type, AST_INFIX_EXPR);

    auto lhs = ast.at(cond_expr.lhs).lhs;
    auto infix_data = P.get_data<AstInfixData>(cond_expr);
    auto rhs = ast.at(infix_data->rhs).lhs;
    EXPECT_EQ(lhs, i);
    EXPECT_EQ(rhs, i + 1);
  }

  auto blks = P.get_array_data(ast.at(if_stmt.rhs));
  for (u32 i = 0; i < blks.items.size(); i++) {
    auto blk_stmt = ast.at(blks.items[i]);
    EXPECT_EQ(blk_stmt.type, AST_BLK_STMT);

    auto blk_data = P.get_array_data(blk_stmt);
    auto expr = ast.at(ast.at(blk_data.items[0]).lhs);
    EXPECT_EQ(expr.type, AST_INT_LIT_EXPR);
    EXPECT_EQ(expr.lhs, i);
  }
}

TEST(Parser, ParseIfStmt) {
  auto input = "if (0 < 1) { 0; }";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();
  validate_if_stmt(P, ast);
}

TEST(Parser, ParseIfElseStmt) {
  auto input = "if (0 < 1) { 0; } else { 1; }";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();
  validate_if_stmt(P, ast);
}

TEST(Parser, ParseIfChainStmt) {
  auto input =
      "if (0 < 1) { 0; } else if (1 < 2) { 1; } else if (2 < 3) { 2; } ";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();
  validate_if_stmt(P, ast);
}

TEST(Parser, ParseIfChainWithElseStmt) {
  auto input = "if (0 < 1) { 0; } else if (1 < 2) { 1; } else if (2 < 3) { 2; "
               "} else { 3; }";
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  auto P = Parser(result);
  auto ast = P.parse();
  validate_if_stmt(P, ast);
}
