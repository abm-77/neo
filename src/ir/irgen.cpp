#include <cassert>
#include <frontend/lexer.h>
#include <ir/ir.h>
#include <ir/irgen.h>
#include <memory>

namespace neo {
namespace ir {

using namespace lex;
using namespace parse;

// Builder
IRBuilder::IRBuilder(IRContext &ctx) : ctx(ctx) {}

Instruction *IRBuilder::push_unop(InstructionOp op, Value *dest, Value *value) {
  Instruction *un_instr = cursor->push_instr(op, dest->get_type());
  un_instr->set_dest(dest);
  un_instr->add_operand(value);

  dest->set_def_instr(un_instr);
  value->add_user(un_instr);

  return un_instr;
}

Instruction *IRBuilder::push_binop(InstructionOp op, Value *dest, Value *lhs,
                                   Value *rhs) {
  Instruction *bin_instr = cursor->push_instr(op, dest->get_type());
  bin_instr->set_dest(dest);
  bin_instr->add_operand(lhs);
  bin_instr->add_operand(rhs);

  dest->set_def_instr(bin_instr);
  lhs->add_user(bin_instr);
  rhs->add_user(bin_instr);

  return bin_instr;
}

BasicBlock *IRBuilder::get_cursor() { return cursor; }

void IRBuilder::set_cursor(BasicBlock *bb) { cursor = bb; }

Instruction *IRBuilder::push_jmp(BasicBlock *bb) {
  Instruction *jmp_instr =
      cursor->push_instr(OP_BR, ctx.get_type(Type("void", 0, 0)));

  jmp_instr->add_label(bb->get_name());

  cursor->add_succ(bb);
  bb->add_pred(cursor);

  return jmp_instr;
}

Instruction *IRBuilder::push_br(Value *cond, BasicBlock *T, BasicBlock *F) {
  Instruction *br_instr =
      cursor->push_instr(OP_BR, ctx.get_type(Type("void", 0, 0)));

  br_instr->add_operand(cond);
  br_instr->add_label(T->get_name());
  br_instr->add_label(F->get_name());

  cond->add_user(br_instr);

  cursor->add_succ(T);
  cursor->add_succ(F);
  T->add_pred(cursor);
  F->add_pred(cursor);

  return br_instr;
}

Instruction *IRBuilder::push_call(Value *dest, Function *callee,
                                  const std::vector<Value *> &args) {
  assert(callee->get_return_type() == dest->get_type());

  Instruction *call_instr = cursor->push_instr(OP_CALL, dest->get_type());
  call_instr->set_dest(dest);
  call_instr->set_function(callee);

  auto callee_args = callee->get_args();
  for (i32 i = 0; i < args.size(); i++) {
    assert(args[i]->get_type() == callee_args[i].type);
    call_instr->add_operand(args[i]);
    args[i]->add_user(call_instr);
  }

  dest->set_def_instr(call_instr);

  return call_instr;
}

Instruction *IRBuilder::push_ret(Value *dest, Value *value) {
  assert(dest->get_type() == value->get_type());

  Instruction *ret_instr = cursor->push_instr(OP_RET, dest->get_type());
  ret_instr->set_dest(dest);
  ret_instr->add_operand(value);

  dest->set_def_instr(ret_instr);
  value->add_user(ret_instr);

  return ret_instr;
}

// IRGenerator
IRGenerator::IRGenerator(Ast &ast) : builder(ctx), ast(ast) {}

Program IRGenerator::make_program() {
  Program P(ctx, "program");
  for (auto stmt : ast.stmts()) {
    gen(P, stmt);
  }
  return P;
}

Value *IRGenerator::gen(Program &program, Ast::NodePtr ptr) {
  auto node = ast.at(ptr);
  switch (node.type) {
  case Ast::AST_INT_LIT_EXPR: {
    return ctx.new_value("inttmp", ast.get<Ast::IntLitData>(node));
  } break;

  case Ast::AST_BOOL_LIT_EXPR: {
    return ctx.new_value("booltmp", ast.get<Ast::BoolLitData>(node));
  } break;

  case Ast::AST_PREFIX_EXPR: {
    auto value = gen(program, node.lhs);
    TokenType prefix = static_cast<TokenType>(node.rhs);
    if (!value)
      return nullptr;

    auto result = ctx.new_value("prefixtmp", value->get_type());
    switch (prefix) {
    case lex::TOKEN_MINUS:
      builder.push_unop(OP_NEG, result, value);
      break;
    case lex::TOKEN_BANG:
      builder.push_unop(OP_NOT, result, value);
      break;
    default:
      return nullptr;
    }

    return result;
  } break;

  case Ast::AST_INFIX_EXPR: {
    auto infix_data = ast.get<Ast::InfixData>(node);
    auto lhs = gen(program, node.lhs); // lhs is stored within node
    auto rhs = gen(program, infix_data.rhs);
    auto op = infix_data.op;
    Value *result;

    if (!lhs || !rhs)
      return nullptr;

    switch (op) {
    case lex::TOKEN_PLUS:
      result = ctx.new_value("infixtmp", lhs->get_type());
      builder.push_binop(OP_ADD, result, lhs, rhs);
      break;
    case lex::TOKEN_MINUS:
      result = ctx.new_value("infixtmp", lhs->get_type());
      builder.push_binop(OP_SUB, result, lhs, rhs);
      break;
    case lex::TOKEN_STAR:
      result = ctx.new_value("infixtmp", lhs->get_type());
      builder.push_binop(OP_MUL, result, lhs, rhs);
      break;
    case lex::TOKEN_SLASH:
      result = ctx.new_value("infixtmp", lhs->get_type());
      builder.push_binop(OP_DIV, result, lhs, rhs);
      break;
    case lex::TOKEN_EQUAL_EQUAL:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CEQ, result, lhs, rhs);
      break;
    case lex::TOKEN_LESS:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CLT, result, lhs, rhs);
      break;
    case lex::TOKEN_LESS_EQUAL:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CLE, result, lhs, rhs);
      break;
    case lex::TOKEN_GREATER:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CGT, result, lhs, rhs);
      break;
    case lex::TOKEN_GREATER_EQUAL:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CGE, result, lhs, rhs);
      break;
    default:
      return nullptr;
    }
    return result;
  } break;

  case Ast::AST_FN_CALL_EXPR: {
    auto fn_name = ast.get<Ast::StringData>(ast.at(node.lhs));
    auto fn_args = ast.get_array_of<Ast::NodePtr>(ast.at(node.rhs));
    auto F = program.get_function(fn_name);
    if (!F)
      return nullptr;

    std::vector<Value *> args;
    for (auto arg : fn_args)
      args.push_back(gen(program, arg));

    if (args.size() != fn_args.size())
      return nullptr;

    auto result = ctx.new_value("calltmp", F->get_return_type());
    builder.push_call(result, F, args);
    return result;
  } break;

  case Ast::AST_EXPR_STMT: {
    return gen(program, node.lhs);
  } break;

  case Ast::AST_RET_STMT: {
    auto value = gen(program, node.lhs);
    auto result = ctx.new_value("rettmp", value->get_type());
    builder.push_ret(result, value);
    return result;
  } break;

  case Ast::AST_FN_DEF_STMT: {
    auto fn_data = ast.get<Ast::FuncDefData>(node);
    auto fn_name = ast.get<Ast::StringData>(ast.at(node.lhs));

    auto F = program.get_function(fn_name);
    if (!F)
      F = program.new_function(fn_name);

    // function already defined
    if (!F->empty())
      return nullptr;

    auto fn_params =
        ast.get_array_of<Ast::FuncDefData::FuncParam>(ast.at(fn_data.params));
    for (auto param : fn_params) {
      auto arg_name = ast.get<Ast::StringData>(ast.at(param.ident));
      auto arg_type = ctx.convert_ast_type_to_ir_type(ast, param.type);
      F->add_arg(Function::Arg{.type = arg_type, .name = arg_name});
    }

    auto ret_type = ctx.convert_ast_type_to_ir_type(ast, fn_data.ret_type);
    F->set_return_type(ret_type);

    if (fn_data.blk != Ast::NULL_NODE) {
      auto entry_bb = F->add_block("entry");
      builder.set_cursor(entry_bb);

      // TODO: Add stores and allocas for arguments

      auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(fn_data.blk));
      for (auto stmt : stmts)
        gen(program, stmt);
    }
    return nullptr;
  } break;

  case Ast::AST_IF_STMT: {
    auto F = builder.get_cursor()->get_parent();
    auto conds = ast.get_array_of<Ast::NodePtr>(ast.at(node.lhs));
    auto blks = ast.get_array_of<Ast::NodePtr>(ast.at(node.lhs));
    auto endif = std::make_unique<BasicBlock>(F, "endif");

    // if (cond) {}
    if (conds.size() == 1 && blks.size() == 1) {
      auto if_block = F->add_block("if");
      auto cond = gen(program, conds[0]);

      builder.push_br(cond, if_block, endif.get());
      builder.set_cursor(if_block);
      auto blk = ast.get_array_of<Ast::NodePtr>(ast.at(blks[0]));
      for (auto stmt : blk)
        gen(program, stmt);
      builder.push_jmp(endif.get());
    }
    // if (cond) {} else {}
    else if (conds.size() == 1 && blks.size() == 2) {
      auto if_block = F->add_block("if");
      auto else_block = F->add_block("else");

      auto cond = gen(program, conds[0]);
      builder.push_br(cond, if_block, else_block);

      builder.set_cursor(if_block);
      auto ifstmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[0]));
      for (auto stmt : ifstmts)
        gen(program, stmt);
      builder.push_jmp(endif.get());

      builder.set_cursor(else_block);
      auto elstmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[1]));
      for (auto stmt : elstmts)
        gen(program, stmt);
      builder.push_jmp(endif.get());
    }
    // if (cond0) {} else if (cond1) ... else {}
    else {
      auto cond_header = F->add_block("cond_header");
      builder.push_jmp(cond_header);

      i32 i = 0;
      for (; i < conds.size() - 1; i++) {
        auto cond = gen(program, conds[i]);
        auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i]));

        builder.set_cursor(cond_header);

        auto me = F->add_block("me");
        auto next_cond_header = F->add_block("next_cond_header");
        builder.push_br(cond, me, next_cond_header);

        builder.set_cursor(me);
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());

        cond_header = next_cond_header;
      }

      auto cond = gen(program, i);
      auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i]));
      auto me = F->add_block("me");

      builder.set_cursor(cond_header);
      if (blks.size() == conds.size()) {
        builder.push_br(cond, me, endif.get());
        builder.set_cursor(me);
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
      } else {
        auto else_stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i + 1]));
        auto else_bb = F->add_block("else");
        builder.push_br(cond, me, else_bb);

        builder.set_cursor(me);
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());

        builder.set_cursor(else_bb);
        for (auto stmt : else_stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
      }
    }
    builder.set_cursor(F->add_block(std::move(endif)));
    return nullptr;
  } break;

  case Ast::AST_WHILE_STMT: {
    auto F = builder.get_cursor()->get_parent();

    auto loop_header = F->add_block("loop_header");
    auto loop_body = F->add_block("loop_body");
    auto loop_end = F->add_block("loop_end");

    builder.push_jmp(loop_header);

    builder.set_cursor(loop_header);
    auto cond = gen(program, node.lhs);
    if (!cond)
      return nullptr;
    builder.push_br(cond, loop_body, loop_end);

    builder.set_cursor(loop_body);
    auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(node.rhs));
    for (auto stmt : stmts)
      gen(program, stmt);
    builder.push_jmp(loop_header);

    builder.set_cursor(loop_end);
    return nullptr;
  } break;

  default:
    return nullptr;
  }
}

} // namespace ir
} // namespace neo
