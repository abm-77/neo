#include "ir/ir.h"
#include <ir/ir_gen.h>
#include <ir/ir_opt.h>

#include <gtest/gtest.h>

namespace neo {
namespace ir {

#define void_type ctx.get_type(Type("void", 0, 0))
#define int_type ctx.get_type(Type("int", 4))
using namespace parse;

Ast parse_input(std::string_view input) {
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  return Parser::parse(result);
}

TEST(IRBuilder, PushUnaryOp) {
  IRContext ctx;
  Function func("test", void_type);
  auto block = func.add_block("block");
  IRBuilder bld(ctx);
  bld.set_cursor(block);

  auto dest = ctx.new_value("dest", int_type);
  auto val = ctx.new_value("val", int_type);
  auto res = bld.push_unop(OP_NEG, dest, val);

  EXPECT_EQ(res->get_op(), OP_NEG);
  EXPECT_EQ(res->get_type(), int_type);
  EXPECT_EQ(res->get_dest(), dest);
  EXPECT_EQ(res->get_operand(0), val);
}

TEST(IRBuilder, PushBinaryOp) {
  IRContext ctx;
  Function func("test", void_type);
  auto block = func.add_block("block");
  IRBuilder bld(ctx);
  bld.set_cursor(block);

  auto dest = ctx.new_value("dest", int_type);
  auto a = ctx.new_value("a", int_type);
  auto b = ctx.new_value("b", int_type);
  auto res = bld.push_binop(OP_ADD, dest, a, b);

  EXPECT_EQ(res->get_op(), OP_ADD);
  EXPECT_EQ(res->get_type(), int_type);
  EXPECT_EQ(res->get_dest(), dest);
  EXPECT_EQ(res->get_operand(0), a);
  EXPECT_EQ(res->get_operand(1), b);
}

TEST(IRBuilder, PushJmp) {
  IRContext ctx;
  Function func("test", void_type);
  IRBuilder bld(ctx);
  auto start = func.add_block("start");
  auto block = func.add_block("block");
  bld.set_cursor(start);

  auto res = bld.push_jmp(block);

  EXPECT_EQ(res->get_op(), OP_JMP);
  EXPECT_EQ(res->get_labels()[0].fn_name, "test");
  EXPECT_EQ(res->get_labels()[0].name, "block");
  EXPECT_EQ(res->get_labels()[0].number, 0);
  EXPECT_EQ(start->get_succs()[0]->get_name(), "block");
}

TEST(IRBuilder, PushBr) {
  IRContext ctx;
  Function func("test", void_type);
  IRBuilder bld(ctx);
  auto start = func.add_block("start");
  auto blockt = func.add_block("blockt");
  auto blockf = func.add_block("blockf");
  auto cond = ctx.new_value("cond", ctx.get_type(Type("bool", 1, 1)));
  bld.set_cursor(start);

  auto res = bld.push_br(cond, blockt, blockf);

  EXPECT_EQ(res->get_op(), OP_BR);
  EXPECT_EQ(res->get_labels()[0].fn_name, "test");
  EXPECT_EQ(res->get_labels()[0].name, "blockt");
  EXPECT_EQ(res->get_labels()[0].number, 0);
  EXPECT_EQ(res->get_labels()[1].fn_name, "test");
  EXPECT_EQ(res->get_labels()[1].name, "blockf");
  EXPECT_EQ(res->get_labels()[1].number, 0);
  EXPECT_EQ(start->get_succs()[0]->get_name(), "blockt");
  EXPECT_EQ(start->get_succs()[1]->get_name(), "blockf");
}

TEST(IRBuilder, PushCall) {
  IRContext ctx;
  Function func("test", void_type);
  auto block = func.add_block("block");
  Function callee("callee", void_type);
  callee.add_arg(Function::Arg{.type = int_type, .name = "a"});
  IRBuilder bld(ctx);
  bld.set_cursor(block);

  auto type = ctx.get_type(Type("int", 4));
  auto a = ctx.new_value("a", type);
  auto res = bld.push_call(nullptr, &callee, {a});

  EXPECT_EQ(res->get_op(), OP_CALL);
  EXPECT_EQ(res->get_function(), &callee);
  EXPECT_EQ(res->get_operand(0), a);
}

TEST(IRBuilder, PushRet) {
  IRContext ctx;
  Function func("test", int_type);
  IRBuilder bld(ctx);
  auto block = func.add_block("block");
  bld.set_cursor(block);

  auto a = ctx.new_value("a", 1);
  auto res = bld.push_ret(a);

  EXPECT_EQ(res->get_op(), OP_RET);
  EXPECT_EQ(res->get_operand(0), a);
}

TEST(IRBuilder, PushAlloca) {
  IRContext ctx;
  Function func("test", void_type);
  IRBuilder bld(ctx);
  auto block = func.add_block("block");
  bld.set_cursor(block);

  auto ptr = ctx.new_value("ptr", ctx.get_type(Type::ptr_to(int_type)));
  auto res = bld.push_alloca(ptr, int_type, 10);

  EXPECT_EQ(res->get_op(), OP_ALLOCA);
  EXPECT_EQ(res->get_dest(), ptr);
  /*EXPECT_EQ(*res->get_operand(1), *ctx.new_value("count", 10));*/
}

TEST(IRBuilder, PushStr) {
  IRContext ctx;
  Function func("test", void_type);
  IRBuilder bld(ctx);
  auto block = func.add_block("block");
  bld.set_cursor(block);

  auto ptr = ctx.new_value("ptr", ctx.get_type(Type::ptr_to(int_type)));
  auto a = ctx.new_value("a", int_type);
  auto res = bld.push_str(ptr, a);

  EXPECT_EQ(res->get_op(), OP_STR);
  EXPECT_EQ(res->get_operand(0), ptr);
  EXPECT_EQ(res->get_operand(1), a);
}

TEST(IRBuilder, PushLd) {
  IRContext ctx;
  Function func("test", void_type);
  IRBuilder bld(ctx);
  auto block = func.add_block("block");
  bld.set_cursor(block);

  auto ptr = ctx.new_value("ptr", ctx.get_type(Type::ptr_to(int_type)));
  auto a = ctx.new_value("a", int_type);
  auto res = bld.push_ld(a, ptr);

  EXPECT_EQ(res->get_op(), OP_LD);
  EXPECT_EQ(res->get_dest(), a);
  EXPECT_EQ(res->get_operand(0), ptr);
}

TEST(IR, IrTest) {
  /*auto ast = parse_input(*/
  /*    "fn add(a: int, b: int) int {"*/
  /*    "if (1 < 2) { a += 1; } else if (2 < 3) { a = 2; } else { a = b; }"*/
  /*    " return a + b;"*/
  /*    "}");*/
  auto ast = parse_input("fn add(a: int, b: int) int {"
                         "const c: int = 5 + 1;"
                         "const d: int = c + 4;"
                         "const e: int = d + 10;"
                         " return e + d + c + a + b;"
                         "}");
  IRGenerator generator(ast);
  auto program = generator.make_program();

  /*auto &funcs = program.get_functions();*/
  /*for (auto &[_, func] : funcs) {*/
  /**/
  /*  for (auto &bb : func.get_blocks()) {*/
  /*    bb->label().debug_print();*/
  /*    std::cout << ":" << std::endl;*/
  /**/
  /*    std::cout << "Preds: " << std::endl;*/
  /*    for (auto &pred : bb->get_preds()) {*/
  /*      std::cout << "\t\t";*/
  /*      pred->label().debug_print();*/
  /*      std::cout << std::endl;*/
  /*    }*/
  /**/
  /*    std::cout << "Succs: " << std::endl;*/
  /*    for (auto &succ : bb->get_succs()) {*/
  /*      std::cout << "\t\t";*/
  /*      succ->label().debug_print();*/
  /*      std::cout << std::endl;*/
  /*    }*/
  /*  }*/
  /*}*/
  /**/
  /*std::cout << std::endl;*/
  /*std::cout << std::endl;*/
  /*std::cout << std::endl;*/

  IROptimizer opt(generator.get_context(), program);
  opt.optimize();
  program.debug_print();
}

} // namespace ir
} // namespace neo
