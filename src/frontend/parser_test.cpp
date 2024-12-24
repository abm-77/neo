#include <frontend/lexer.h>
#include <frontend/parser.h>
#include <frontend/util.h>

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

void EXPECT_TYPE(Ast &ast, Ast::NodePtr actual_ptr, Ast::Type expected) {
  auto actual = ast.get_type(actual_ptr);
  EXPECT_EQ(actual.kind, expected.kind);
  EXPECT_EQ(actual.primitive_type, expected.primitive_type);
  EXPECT_EQ(actual.array_size, expected.array_size);
  if (actual.el_type != Ast::NULL_NODE)
    EXPECT_TYPE(ast, actual.el_type, ast.get_type(expected.el_type));
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
  EXPECT_EQ(var_def_data.is_const, true);

  EXPECT_TYPE(ast, var_def_data.type, Ast::Type(Ast::Type::PrimitiveType::INT));

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
  EXPECT_EQ(var_def_data.is_const, false);

  EXPECT_TYPE(ast, var_def_data.type, Ast::Type(Ast::Type::PrimitiveType::INT));

  auto value = ast.get<Ast::IntLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, -1001);
}

TEST(Parser, ParseVarDefStmtArray) {
  auto ast = parse_input("var a: [2]int = []int{0, 1};");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.is_const, false);

  EXPECT_TYPE(
      ast, var_def_data.type,
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));

  auto value = ast.get<Ast::ArrayLitData>(ast.at(var_def_data.value));
  EXPECT_TYPE(
      ast, value.type,
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));

  auto arr_size = ast.get<Ast::IntLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(arr_size, 2);

  auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(value.init_list));
  EXPECT_EQ(init_list.size(), arr_size);
  for (u32 i = 0; i < init_list.size(); i++) {
    auto int_lit_expr = ast.at(init_list[i]);
    EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
    EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), i);
  }
}

TEST(Parser, ParseVarDefStmtBool) {
  auto ast = parse_input("const a: bool = true;");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.is_const, true);

  EXPECT_TYPE(ast, var_def_data.type,
              Ast::Type(Ast::Type::PrimitiveType::BOOL));

  auto value = ast.get<Ast::BoolLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, true);
}

TEST(Parser, ParseVarAssign) {
  auto ast = parse_input("a /= 2;");
  auto expr_stmt = ast.root();
  EXPECT_EQ(expr_stmt.type, Ast::AST_EXPR_STMT);

  auto infix_expr = ast.at(expr_stmt.lhs);
  std::cout << node_type_to_string(infix_expr.type) << std::endl;
  EXPECT_EQ(infix_expr.type, Ast::AST_INFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ast.at(infix_expr.lhs)), "a");

  auto infix_data = ast.get<Ast::InfixData>(infix_expr);
  EXPECT_EQ(infix_data.op, TOKEN_SLASH_EQUAL);

  auto rhs = ast.get<Ast::IntLitData>(ast.at(infix_data.rhs));
  EXPECT_EQ(rhs, 2);
}

TEST(Parser, ParsePrefixGroupedExpr) {
  auto ast = parse_input("var a: int = -(b + c);");

  auto var_def_stmt = ast.root();
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "a");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.is_const, false);

  EXPECT_TYPE(ast, var_def_data.type, Ast::Type(Ast::Type::PrimitiveType::INT));

  auto prefix_expr = ast.at(var_def_data.value);
  EXPECT_EQ(prefix_expr.type, Ast::AST_PREFIX_EXPR);
  EXPECT_EQ(static_cast<TokenType>(prefix_expr.rhs), TOKEN_MINUS);

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
  EXPECT_EQ(blk_stmt.type, Ast::AST_SPAN);

  auto blk_data = ast.get_array_of<i32>(blk_stmt);
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

  auto conds = ast.get_array_of<i32>(ast.at(if_stmt.lhs));
  for (u32 i = 0; i < conds.size(); i++) {
    auto cond_expr = ast.at(conds[i]);
    EXPECT_EQ(cond_expr.type, Ast::AST_INFIX_EXPR);

    auto lhs = ast.at(cond_expr.lhs).lhs;
    auto infix_data = ast.get<Ast::InfixData>(cond_expr);
    auto rhs = ast.at(infix_data.rhs).lhs;
    EXPECT_EQ(lhs, i);
    EXPECT_EQ(rhs, i + 1);
  }

  auto blks = ast.get_array_of<i32>(ast.at(if_stmt.rhs));
  for (u32 i = 0; i < blks.size(); i++) {
    auto blk_stmt = ast.at(blks[i]);
    EXPECT_EQ(blk_stmt.type, Ast::AST_SPAN);

    auto blk_data = ast.get_array_of<i32>(blk_stmt);
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
  EXPECT_EQ(while_blk.type, Ast::AST_SPAN);

  auto stmts = ast.get_array_of<i32>(while_blk);

  auto int_lit_expr = ast.at(ast.at(stmts[0]).lhs);
  EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
  EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), 0);
}

TEST(Parser, ParseForStmt) {
  auto ast = parse_input("for (var i: int = 0; i < 10; i += 1) { 0; } ");

  auto for_stmt = ast.root();
  EXPECT_EQ(for_stmt.type, Ast::AST_FOR_STMT);

  auto var_def_stmt = ast.at(for_stmt.lhs);
  EXPECT_EQ(var_def_stmt.type, Ast::AST_VAR_DEF_STMT);

  auto ident_stmt = ast.at(var_def_stmt.lhs);
  EXPECT_EQ(ident_stmt.type, Ast::AST_IDENT_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ident_stmt), "i");

  auto var_def_data = ast.get<Ast::VarDefData>(var_def_stmt);
  EXPECT_EQ(var_def_data.is_const, false);

  EXPECT_TYPE(ast, var_def_data.type, Ast::Type(Ast::Type::PrimitiveType::INT));

  auto value = ast.get<Ast::IntLitData>(ast.at(var_def_data.value));
  EXPECT_EQ(value, 0);

  auto for_data = ast.get<Ast::ForData>(for_stmt);

  auto cond = ast.at(for_data.cond);
  EXPECT_EQ(cond.type, Ast::AST_INFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ast.at(cond.lhs)), "i");
  auto infix_data = ast.get<Ast::InfixData>(cond);
  EXPECT_EQ(infix_data.op, TOKEN_LESS);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(infix_data.rhs)), 10);

  auto cont = ast.at(for_data.cont);
  EXPECT_EQ(cont.type, Ast::AST_INFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ast.at(cont.lhs)), "i");
  infix_data = ast.get<Ast::InfixData>(cont);
  EXPECT_EQ(infix_data.op, TOKEN_PLUS_EQUAL);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(infix_data.rhs)), 1);

  auto blk = ast.at(for_data.blk);
  auto stmts = ast.get_array_of<Ast::NodePtr>(blk);
  EXPECT_EQ(ast.get<Ast::IntLitData>(ast.at(ast.at(stmts[0]).lhs)), 0);
}

TEST(Parser, ParseFuncDefStmt) {
  auto ast = parse_input("fn add (a: int, b: int) int { return a + b; }");

  auto func_def_stmt = ast.root();
  EXPECT_EQ(func_def_stmt.type, Ast::AST_FN_DEF_STMT);
  EXPECT_EQ(ast.get<Ast::StringData>(func_def_stmt), "add");

  auto func_def_data = ast.get<Ast::FuncDefData>(func_def_stmt);

  EXPECT_TYPE(ast, func_def_data.ret_type,
              Ast::Type(Ast::Type::PrimitiveType::INT));

  auto func_params = ast.get_array_of<Ast::FuncDefData::FuncParam>(
      ast.at(func_def_data.params));

  std::vector<std::string_view> expected_params = {"a", "b"};
  for (u32 i = 0; i < func_params.size(); i++) {
    EXPECT_EQ(ast.get<Ast::StringData>(ast.at(func_params[i].ident)),
              expected_params[i]);
    EXPECT_TYPE(ast, func_params[i].type,
                Ast::Type(Ast::Type::PrimitiveType::INT));
  }

  auto func_blk = ast.get_array_of<Ast::NodePtr>(ast.at(func_def_data.blk));
  auto ret_stmt = ast.at(func_blk[0]);
  EXPECT_EQ(ret_stmt.type, Ast::AST_RET_STMT);

  auto infix_expr = ast.at(ret_stmt.lhs);
  EXPECT_EQ(infix_expr.type, Ast::AST_INFIX_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(ast.at(infix_expr.lhs)), "a");

  auto infix_data = ast.get<Ast::InfixData>(infix_expr);
  EXPECT_EQ(infix_data.op, TOKEN_PLUS);
  EXPECT_EQ(ast.get<Ast::StringData>(ast.at(infix_data.rhs)), "b");
}

TEST(Parser, ParseFuncCallExpr) {
  auto ast = parse_input("add(0, 1);");

  auto expr_stmt = ast.root();
  EXPECT_EQ(expr_stmt.type, Ast::AST_EXPR_STMT);

  auto call_expr = ast.at(expr_stmt.lhs);
  EXPECT_EQ(ast.get<Ast::StringData>(call_expr), "add");

  auto args = ast.get_array_of<Ast::NodePtr>(ast.at(call_expr.rhs));
  for (u32 i = 0; i < args.size(); i++) {
    auto int_lit_expr = ast.at(args[i]);
    EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
    EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), i);
  }
}

TEST(Parser, ParseArrLitExprFull) {
  auto ast = parse_input("[2]int{0, 1};");

  auto arr_lit_expr = ast.at(ast.root().lhs);
  EXPECT_EQ(arr_lit_expr.type, Ast::AST_ARR_LIT_EXPR);

  auto arr_size = ast.get<Ast::IntLitData>(arr_lit_expr);
  EXPECT_EQ(arr_size, 2);

  auto arr_data = ast.get<Ast::ArrayLitData>(arr_lit_expr);
  EXPECT_TYPE(
      ast, arr_data.type,
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));

  auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(arr_data.init_list));
  EXPECT_EQ(init_list.size(), arr_size);
  for (u32 i = 0; i < init_list.size(); i++) {
    auto int_lit_expr = ast.at(init_list[i]);
    EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
    EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), i);
  }
}

TEST(Parser, ParseArrLitExprOmitSize) {
  auto ast = parse_input("[]int{0, 1};");

  auto arr_lit_expr = ast.at(ast.root().lhs);
  EXPECT_EQ(arr_lit_expr.type, Ast::AST_ARR_LIT_EXPR);

  auto arr_size = ast.get<Ast::IntLitData>(arr_lit_expr);
  EXPECT_EQ(arr_size, 2);

  auto arr_data = ast.get<Ast::ArrayLitData>(arr_lit_expr);
  EXPECT_TYPE(
      ast, arr_data.type,
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));

  auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(arr_data.init_list));
  EXPECT_EQ(init_list.size(), arr_size);
  for (u32 i = 0; i < init_list.size(); i++) {
    auto int_lit_expr = ast.at(init_list[i]);
    EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
    EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), i);
  }
}

TEST(Parser, ParseArrLitExprOmitInit) {
  auto ast = parse_input("[2]int{};");

  auto arr_lit_expr = ast.at(ast.root().lhs);
  EXPECT_EQ(arr_lit_expr.type, Ast::AST_ARR_LIT_EXPR);

  auto arr_size = ast.get<Ast::IntLitData>(arr_lit_expr);
  EXPECT_EQ(arr_size, 2);

  auto arr_data = ast.get<Ast::ArrayLitData>(arr_lit_expr);
  EXPECT_TYPE(
      ast, arr_data.type,
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));

  auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(arr_data.init_list));
  EXPECT_EQ(init_list.size(), 0);
}

TEST(Parser, ParseArrLitNested) {
  auto ast = parse_input("[2][2]int{ [2]int{0,1}, [2]int{0,1} };");

  auto arr_lit_expr = ast.at(ast.root().lhs);
  auto arr_size = ast.get<Ast::IntLitData>(arr_lit_expr);
  EXPECT_EQ(arr_size, 2);

  auto arr_data = ast.get<Ast::ArrayLitData>(arr_lit_expr);
  auto el_type = ast.register_type(
      Ast::Type(Ast::Type::Kind::ARRAY, Ast::Type::PrimitiveType::INT, 2));
  EXPECT_TYPE(ast, arr_data.type,
              Ast::Type(Ast::Type::Kind::ARRAY, el_type, 2));

  auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(arr_data.init_list));
  for (u32 i = 0; i < init_list.size(); i++) {
    auto arr_el_lit_expr = ast.at(init_list[i]);
    EXPECT_EQ(arr_el_lit_expr.type, Ast::AST_ARR_LIT_EXPR);

    auto arr_el_data = ast.get<Ast::ArrayLitData>(arr_el_lit_expr);
    auto arr_el_init_list =
        ast.get_array_of<Ast::NodePtr>(ast.at(arr_el_data.init_list));
    for (u32 j = 0; j < arr_el_init_list.size(); j++) {
      auto int_lit_expr = ast.at(arr_el_init_list[j]);
      EXPECT_EQ(int_lit_expr.type, Ast::AST_INT_LIT_EXPR);
      EXPECT_EQ(ast.get<Ast::IntLitData>(int_lit_expr), j);
    }
  }
}

TEST(Parser, ParseArrayAccessExpr) {
  auto ast = parse_input("a[3];");
  auto arr_index_expr = ast.at(ast.root().lhs);
  EXPECT_EQ(arr_index_expr.type, Ast::AST_ARR_INDEX_EXPR);
  EXPECT_EQ(ast.get<Ast::StringData>(arr_index_expr), "a");
  EXPECT_EQ(arr_index_expr.rhs, 3);
}
