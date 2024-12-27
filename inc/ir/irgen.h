#pragma once

#include "ir.h"

#include <frontend/parser.h>

namespace neo {
namespace ir {

class IRBuilder {
public:
  IRBuilder(IRContext &ctx);

  void set_cursor(BasicBlock *bb);
  BasicBlock *get_cursor();

  Instruction *push_unop(InstructionOp op, Value *dest, Value *value);
  Instruction *push_binop(InstructionOp op, Value *dest, Value *lhs,
                          Value *rhs);

  Instruction *push_jmp(BasicBlock *T);
  Instruction *push_br(Value *cond, BasicBlock *T, BasicBlock *F);
  Instruction *push_call(Value *dest, Function *callee,
                         const std::vector<Value *> &args);
  Instruction *push_ret(Value *dest, Value *value);

  Instruction *push_alloca(Value *dest, Type *type, i32 count = 1);
  Instruction *push_str(Value *dest, Value *value, Value *offset = nullptr);
  Instruction *push_ld(Value *dest, Value *src, Value *offset = nullptr);

private:
  IRContext &ctx;
  BasicBlock *cursor;
};

class IRGenerator {
public:
  IRGenerator(parse::Ast &ast);
  Program make_program();
  Value *gen(Program &program, parse::Ast::NodePtr node);

public:
  void mem2reg(ir::Function &func);

private:
  IRContext ctx;
  IRBuilder builder;
  parse::Ast &ast;
};
} // namespace ir
} // namespace neo
