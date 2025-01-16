#include "ir/ir.h"
#include <ir/irgen.h>
#include <ir/iropt.h>

#include <gtest/gtest.h>
#include <string_view>

/*
 * ir.cpp has a lot of just plain getter/setter functions that I am omittin from
 * our test suite because most of them are trivial and one line long. I will be
 * testing the functions that have more ocmplex logic.
 *
 * */
namespace neo {
namespace ir {

using namespace parse;

TEST(IRTest, OperandReplacement) {
  IRContext ctx;
  Function func;
  auto block = func.add_block("test");

  auto int_type = ctx.get_type(Type("int", 4));

  auto out = ctx.new_value("result", int_type);
  auto a = ctx.new_value("a", int_type);
  auto b = ctx.new_value("b", int_type);
  auto c = ctx.new_value("c", int_type);

  auto instr = block->push_instr(OP_ADD, int_type);
  instr->add_operand(out);
  instr->add_operand(a);
  instr->add_operand(b);
  out->set_def_instr(instr);
  a->add_user(instr);
  b->add_user(instr);

  EXPECT_EQ(instr->get_operand(1), a);
  instr->replace_operand(a, c);
  EXPECT_EQ(instr->get_operand(1), c);
}

TEST(IRTest, UseReplacement) {
  IRContext ctx;
  IRBuilder builder(ctx);
  Function func;
  auto block = func.add_block("test");

  auto int_type = ctx.get_type(Type("int", 4));

  auto out = ctx.new_value("result", int_type);
  auto a = ctx.new_value("a", int_type);
  auto b = ctx.new_value("b", int_type);
  auto c = ctx.new_value("c", int_type);

  auto instr = block->push_instr(OP_ADD, int_type);
  instr->add_operand(out);
  instr->add_operand(a);
  instr->add_operand(b);
  out->set_def_instr(instr);
  a->add_user(instr);
  b->add_user(instr);

  EXPECT_EQ(instr->get_operand(1), a);
  a->replace_all_uses_with(c);
  EXPECT_EQ(instr->get_operand(1), c);
  EXPECT_EQ(c->get_users()[0], instr);
}

TEST(IRTest, RemoveDeadInstrs) {
  IRContext ctx;
  IRBuilder builder(ctx);
  Function func;
  auto block = func.add_block("test");

  auto int_type = ctx.get_type(Type("int", 4));

  auto instr = block->push_instr(OP_ADD, int_type);

  EXPECT_EQ(instr->alive(), true);
  EXPECT_EQ(block->get_instructions().size(), 1);

  instr->kill();
  EXPECT_EQ(instr->alive(), false);

  func.remove_dead_instrs();
  EXPECT_EQ(block->get_instructions().size(), 0);
}

TEST(IRTest, Scopes) {
  IRContext ctx;
  Program program(ctx, "test");

  auto a0 = ctx.new_value("a0", 1);
  auto a1 = ctx.new_value("a1", 2);

  // new symbol
  program.new_symbol("var_a", a0);
  EXPECT_EQ(std::get<int>(program.lookup_symbol("var_a")->get_const_value()),
            1);

  // reference symbol in encapsulating scope
  program.push_scope();
  EXPECT_EQ(std::get<int>(program.lookup_symbol("var_a")->get_const_value()),
            1);

  // reassign symbol in scope (shadowing)
  program.new_symbol("var_a", a1);
  EXPECT_EQ(std::get<int>(program.lookup_symbol("var_a")->get_const_value()),
            2);

  // pop scope
  program.pop_scope();
  EXPECT_EQ(std::get<int>(program.lookup_symbol("var_a")->get_const_value()),
            1);
}

void EXPECT_TYPE(std::string_view input, Type expected_type) {
  IRContext ctx;
  auto ast = Parser::parse(Lexer::from_source(input).lex());
  auto node = ast.root();
  auto var_data = ast.get<Ast::VarDefData>(node);
  auto var_type = ctx.convert_ast_type_to_ir_type(ast, var_data.type);
  EXPECT_EQ(*var_type, *ctx.get_type(expected_type));
}

TEST(IRTest, TypeConversion) {
  Type int_type("int", 4, 4);
  EXPECT_TYPE("var a: int = 10;", int_type);
  EXPECT_TYPE("var a: bool = true;", Type("bool", 1, 1));
  EXPECT_TYPE("var a: float = 3;", Type("float", 4, 4));
  EXPECT_TYPE("var a: [3]int = []int{1, 2, 3};", Type::arr_of(&int_type, 3));
}

} // namespace ir
} // namespace neo
