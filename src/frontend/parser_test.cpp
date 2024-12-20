#include "lexer.h"
#include "parser.h"
#include "util.h"

#include <gtest/gtest.h>
#include <ostream>
#include <string_view>
#include <vector>

using namespace neo::lex;
using namespace neo::parse;

void debug_print_ast(Ast &ast) {
  for (u32 i = 0; i < ast.tree().size(); i++) {
    std::cout << i << " : " << node_type_to_string(ast.tree().at(i).type)
              << std::endl;
  }
}

Ast parse_input(std::string_view input) {
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  return Parser::parse(result);
}

TEST(Parser, ParseExprStmt) {
  auto ast = parse_input("a;");

  auto stmt = ast.root();
  EXPECT_EQ(stmt.type, Ast::AST_EXPR_STMT);

  auto ident_expr = ast.at(stmt.lhs);
  EXPECT_EQ(ident_expr.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_expr), "a");
}

TEST(Parser, ParseRetStmt) {
  auto ast = parse_input("return 7;");

  auto stmt = ast.root();
  EXPECT_EQ(stmt.type, Ast::AST_RET_STMT);
  EXPECT_EQ(ast.at(stmt.lhs).type, Ast::AST_INT_LIT_EXPR);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(stmt.lhs)), 7);
}

TEST(Parser, ParseVarDefStmtConst) {
  auto ast = parse_input("const a: int = 123;");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_expr = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_expr.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_expr), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.type, TYPE_INT);
  EXPECT_EQ(var_def_data.is_const, true);

  auto value = ast.get<Ast::IntLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, 123);
}

TEST(Parser, ParseVarDefStmtVar) {
  auto ast = parse_input("var a: int = -1001;");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.type, TYPE_INT);
  EXPECT_EQ(var_def_data.is_const, false);

  auto value = ast.get<Ast::IntLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, -1001);
}

TEST(Parser, ParseVarDefStmtBool) {
  auto ast = parse_input("const a: bool = true;");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.type, TYPE_BOOL);
  EXPECT_EQ(var_def_data.is_const, true);

  auto value = ast.get<Ast::BoolLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, true);
}

TEST(Parser, ParsePrefixGroupedExpr) {
  auto ast = parse_input("var a: int = -(b + c);");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.type, TYPE_INT);
  EXPECT_EQ(var_def_data.is_const, false);

  auto prefix_expr = ast.at(var_def_data.value);
  EXPECT_EQ(prefix_expr.type, Ast::AST_PREFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::PrefixData>(prefix_expr).prefix, TOKEN_MINUS);

  auto infix_expr = ast.at(prefix_expr.lhs);
  EXPECT_EQ(infix_expr.type, Ast::AST_INFIX_EXPR);

  auto lhs = ast.at(infix_expr.lhs);
  EXPECT_EQ(lhs.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(lhs), "b");

  auto infix_data = ast.get<Ast::InfixData>(infix_expr);
  EXPECT_EQ(infix_data.op, TOKEN_PLUS);

  auto rhs = ast.at(infix_data.rhs);
  EXPECT_EQ(rhs.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(rhs), "c");
}

TEST(Parser, ParseBlkStmt) {
  auto ast = parse_input("{ 0; 1; 2; }");

  auto blk_stmt = ast.root();
  EXPECT_EQ(blk_stmt.type, Ast::AST_BLK_STMT);

  auto blk_data = ast.get<Ast::ArrayData>(blk_stmt);
  for (u32 i = 0; i < blk_data.size(); i++) {
    auto expr_stmt = ast.at(blk_data[i]);
    auto int_lit = ast.get<Ast::IntLitData>(ast.at(expr_stmt.lhs));
    EXPECT_EQ(int_lit, i);
  }
}

void validate_if_stmt(std::string_view input) {
  auto ast = parse_input(input);

  auto if_stmt = ast.root();
  EXPECT_EQ(if_stmt.type, Ast::AST_IF_STMT);

  auto conds = ast.get<Ast::ArrayData>(ast.at(if_stmt.lhs));
  for (u32 i = 0; i < conds.size(); i++) {
    auto cond_expr = ast.at(conds[i]);
    EXPECT_EQ(cond_expr.type, Ast::AST_INFIX_EXPR);

    auto lhs = ast.at(cond_expr.lhs).lhs;
    auto infix_data = ast.get<Ast::InfixData>(cond_expr);
    auto rhs = ast.at(infix_data.rhs).lhs;
    EXPECT_EQ(lhs, i);
    EXPECT_EQ(rhs, i + 1);
  }

  auto blks = ast.get<Ast::ArrayData>(ast.at(if_stmt.rhs));
  for (u32 i = 0; i < blks.size(); i++) {
    auto blk_stmt = ast.at(blks[i]);
    EXPECT_EQ(blk_stmt.type, Ast::AST_BLK_STMT);

    auto blk_data = ast.get<Ast::ArrayData>(blk_stmt);
    auto expr = ast.at(ast.at(blk_data[0]).lhs);
    EXPECT_EQ(expr.type, Ast::AST_INT_LIT_EXPR);
    EXPECT_EQ(expr.lhs, i);
  }
}

TEST(Parser, ParseIfStmt) { validate_if_stmt("if (0 < 1) { 0; }"); }

TEST(Parser, ParseIfElseStmt) {
  validate_if_stmt("if (0 < 1) { 0; } else { 1; }");
}

TEST(Parser, ParseIfChainStmt) {
  validate_if_stmt(
      "if (0 < 1) { 0; } else if (1 < 2) { 1; } else if (2 < 3) { 2; } ");
}

TEST(Parser, ParseIfChainWithElseStmt) {
  validate_if_stmt(
      "if (0 < 1) { 0; } else if (1 < 2) { 1; } else if (2 < 3) { 2; "
      "} else { 3; }");
}

TEST(Parser, ParseWhileStmt) {
  auto ast = parse_input("while (1 < 2) { 0; }");

  auto while_stmt = ast.root();
  EXPECT_EQ(while_stmt.type, Ast::AST_WHILE_STMT);

  auto cond_expr = ast.at(while_stmt.lhs);
  EXPECT_EQ(cond_expr.type, Ast::AST_INFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(cond_expr.lhs)), 1);

  auto infix_data = ast.get<Ast::InfixData>(cond_expr);
  EXPECT_EQ(infix_data.op, TOKEN_LESS);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(infix_data.rhs)), 2);
  auto while_blk = ast.at(while_stmt.rhs);
  EXPECT_EQ(while_blk.type, Ast::AST_BLK_STMT);

  auto stmts = ast.get<Ast::ArrayData>(while_blk);

  auto int_lit_expr = ast.at(ast.at(stmts[0]).lhs);
  EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
  EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), 0);
}
