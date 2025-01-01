#include <alloca.h>
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
  un_instr->add_operand(dest);
  un_instr->add_operand(value);

  dest->set_def_instr(un_instr);
  value->add_user(un_instr);

  return un_instr;
}

Instruction *IRBuilder::push_binop(InstructionOp op, Value *dest, Value *lhs,
                                   Value *rhs) {
  Instruction *bin_instr = cursor->push_instr(op, dest->get_type());
  bin_instr->add_operand(dest);
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

  jmp_instr->add_label(bb->label());

  cursor->add_succ(bb);
  bb->add_pred(cursor);

  return jmp_instr;
}

Instruction *IRBuilder::push_br(Value *cond, BasicBlock *T, BasicBlock *F) {
  Instruction *br_instr =
      cursor->push_instr(OP_BR, ctx.get_type(Type("void", 0, 0)));

  br_instr->add_operand(cond);
  br_instr->add_label(T->label());
  br_instr->add_label(F->label());

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
  call_instr->add_operand(dest);
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
  ret_instr->add_operand(dest);
  ret_instr->add_operand(value);

  dest->set_def_instr(ret_instr);
  value->add_user(ret_instr);

  return ret_instr;
}

Instruction *IRBuilder::push_alloca(Value *dest, Type *type, i32 count) {
  assert(count >= 1 && "must alloc at least one item");
  assert(dest->get_type()->get_derived_type() == Type::DerivedType::POINTER ||
         dest->get_type()->get_derived_type() == Type::DerivedType::ARRAY &&
             "destination must be array or pointer");
  assert(dest->get_type()->get_base_type() == type &&
         "destination base type must match allocated type");

  Instruction *alloca_instr = cursor->push_instr(OP_ALLOCA, dest->get_type());
  alloca_instr->add_operand(dest);

  auto count_val = ctx.new_value("count", count);
  alloca_instr->add_operand(count_val);

  dest->set_def_instr(alloca_instr);
  count_val->add_user(alloca_instr);

  return alloca_instr;
}

Instruction *IRBuilder::push_str(Value *dest, Value *value, Value *offset) {
  assert(dest->get_type()->get_derived_type() == Type::DerivedType::POINTER ||
         dest->get_type()->get_derived_type() == Type::DerivedType::ARRAY &&
             "destination must be array or pointer");
  assert(dest->get_type()->get_base_type() == value->get_type() &&
         "destination base type must match value type");

  Instruction *str_instr = cursor->push_instr(OP_STR, dest->get_type());
  str_instr->add_operand(dest);
  str_instr->add_operand(value);
  if (offset)
    str_instr->add_operand(offset);

  dest->set_def_instr(str_instr);
  value->add_user(str_instr);
  if (offset)
    offset->add_user(str_instr);

  return str_instr;
}

Instruction *IRBuilder::push_ld(Value *dest, Value *src, Value *offset) {
  assert(src->get_type()->get_derived_type() == Type::DerivedType::POINTER ||
         src->get_type()->get_derived_type() == Type::DerivedType::ARRAY &&
             "source must be array or pointer");

  // since we distinguish between pointers and arrays, we implicitly dereference
  // all immediate pointers to arrays (i.e. *[]T -> []T / *T). the pointer in
  // this instance points to the address of the array, which is the address of
  // the first element. an actual pointer to an array would be **[]T -> *[]T /
  // **T.
  assert(
      (((src->get_type()->get_derived_type() == Type::DerivedType::POINTER) &&
        (src->get_type()->get_base_type()->get_derived_type() ==
         Type::DerivedType::ARRAY) &&
        dest->get_type() ==
            src->get_type()->get_base_type()->get_base_type()) ||

       (src->get_type()->get_derived_type() == Type::DerivedType::POINTER) &&
           dest->get_type() == src->get_type()->get_base_type()) &&
      "destination type must match value base type");

  Instruction *ld_instr = cursor->push_instr(OP_LD, dest->get_type());
  ld_instr->add_operand(dest);
  ld_instr->add_operand(src);
  if (offset)
    ld_instr->add_operand(offset);

  dest->set_def_instr(ld_instr);
  src->add_user(ld_instr);
  if (offset)
    offset->add_user(ld_instr);

  return ld_instr;
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
    return ctx.new_value("intlit", ast.get<Ast::IntLitData>(node));
  } break;

  case Ast::AST_BOOL_LIT_EXPR: {
    return ctx.new_value("boollit", ast.get<Ast::BoolLitData>(node));
  } break;

  case Ast::AST_ARR_LIT_EXPR: {
    auto arr_type = ctx.convert_ast_type_to_ir_type(ast, node.lhs);
    auto init_list = ast.get_array_of<Ast::NodePtr>(ast.at(node.rhs));
    auto arr_ptr = ctx.new_value("arrlit", arr_type);
    builder.push_alloca(arr_ptr, arr_type->get_base_type(),
                        arr_type->get_size());

    for (i32 i = 0; i < init_list.size(); i++) {
      auto init_val = gen(program, init_list[i]);
      builder.push_str(
          arr_ptr, init_val,
          ctx.new_value("offset",
                        i * (i32)arr_type->get_base_type()->get_size()));
    }

    return arr_ptr;
  } break;

  case Ast::AST_ARR_INDEX_EXPR: {
    auto arr_name = ast.get<Ast::StringData>(ast.at(node.lhs));
    auto i = static_cast<i32>(node.rhs);
    auto arr_ptr = program.lookup_symbol(arr_name);
    if (!arr_ptr)
      return nullptr;
    auto arr_type = arr_ptr->get_type()->get_base_type();

    auto result = ctx.new_value("arridxtmp", arr_type->get_base_type());
    builder.push_ld(
        result, arr_ptr,
        ctx.new_value("offset",
                      i * (i32)arr_type->get_base_type()->get_size()));
    return result;
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

    if (infix_data.op >= lex::TOKEN_EQUAL &&
        infix_data.op <= lex::TOKEN_STAR_EQUAL) {
      auto var_name = ast.get<Ast::StringData>(ast.at(node.lhs));
      auto var = program.lookup_symbol(var_name);
      if (!var)
        return nullptr;

      auto rhs = gen(program, infix_data.rhs);
      if (!rhs)
        return nullptr;

      Value *new_value = nullptr;
      if (infix_data.op == TOKEN_EQUAL) {
        new_value = rhs;
      } else {
        auto old_val =
            ctx.new_value("oldtmp", var->get_type()->get_base_type());
        builder.push_ld(old_val, var);
        new_value = ctx.new_value("newtmp", old_val->get_type());

        switch (infix_data.op) {
        case TOKEN_MINUS_EQUAL:
          builder.push_binop(OP_SUB, new_value, old_val, rhs);
          break;
        case TOKEN_PLUS_EQUAL:
          builder.push_binop(OP_ADD, new_value, old_val, rhs);
          break;
        case TOKEN_SLASH_EQUAL:
          builder.push_binop(OP_DIV, new_value, old_val, rhs);
          break;
        case TOKEN_STAR_EQUAL:
          builder.push_binop(OP_MUL, new_value, old_val, rhs);
          break;
        default:
          return nullptr;
        }
      }

      builder.push_str(var, new_value);

      return new_value;
    }

    auto lhs = gen(program, node.lhs); // lhs is stored within node
    auto rhs = gen(program, infix_data.rhs);
    Value *result;

    if (!lhs || !rhs)
      return nullptr;

    switch (infix_data.op) {
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
    case lex::TOKEN_BANG_EQUAL:
      result = ctx.new_value("infixtmp", ctx.get_type(Type("bool", 1, 1)));
      builder.push_binop(OP_CNE, result, lhs, rhs);
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

  case Ast::AST_IDENT_EXPR: {
    auto var_name = ast.get<Ast::StringData>(node);
    auto var_slot = program.lookup_symbol(var_name);
    auto result =
        ctx.new_value("identtmp", var_slot->get_type()->get_base_type());
    builder.push_ld(result, var_slot);
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

  case Ast::AST_VAR_DEF_STMT: {
    auto var_name = ast.get<Ast::StringData>(ast.at(node.lhs));
    auto var_data = ast.get<Ast::VarDefData>(node);
    auto var_type = ctx.convert_ast_type_to_ir_type(ast, var_data.type);
    auto var_value = gen(program, var_data.value);

    auto alloca = ctx.new_value("deftmp", ctx.get_type(Type::ptr_to(var_type)));
    builder.push_alloca(alloca, var_type);
    builder.push_str(alloca, var_value);
    program.new_symbol(var_name, alloca);

    return nullptr;
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
      program.push_scope();

      for (auto param : fn_params) {
        auto arg_name = ast.get<Ast::StringData>(ast.at(param.ident));
        auto arg_type = ctx.convert_ast_type_to_ir_type(ast, param.type);
        auto alloca =
            ctx.new_value("alloctmp", ctx.get_type(Type::ptr_to(arg_type)));
        auto arg_val = ctx.new_value("argtmp", arg_type);

        builder.push_alloca(alloca, arg_type);
        builder.push_str(alloca, arg_val);
        program.new_symbol(arg_name, alloca);
      }

      auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(fn_data.blk));
      for (auto stmt : stmts)
        gen(program, stmt);

      program.pop_scope();
    }
    return nullptr;
  } break;

  case Ast::AST_IF_STMT: {
    auto F = builder.get_cursor()->get_parent();
    auto conds = ast.get_array_of<Ast::NodePtr>(ast.at(node.lhs));
    auto blks = ast.get_array_of<Ast::NodePtr>(ast.at(node.rhs));
    auto endif = F->create_block("endif");

    // if (cond) {}
    if (conds.size() == 1 && blks.size() == 1) {
      auto if_block = F->add_block("if");
      auto cond = gen(program, conds[0]);

      builder.push_br(cond, if_block, endif.get());
      builder.set_cursor(if_block);
      program.push_scope();

      auto blk = ast.get_array_of<Ast::NodePtr>(ast.at(blks[0]));
      for (auto stmt : blk)
        gen(program, stmt);
      builder.push_jmp(endif.get());

      program.pop_scope();
    }
    // if (cond) {} else {}
    else if (conds.size() == 1 && blks.size() == 2) {
      auto if_block = F->add_block("if");
      auto else_block = F->add_block("else");

      auto cond = gen(program, conds[0]);
      builder.push_br(cond, if_block, else_block);

      builder.set_cursor(if_block);
      program.push_scope();
      auto ifstmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[0]));
      for (auto stmt : ifstmts)
        gen(program, stmt);
      builder.push_jmp(endif.get());
      program.pop_scope();

      builder.set_cursor(else_block);
      program.push_scope();
      auto elstmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[1]));
      for (auto stmt : elstmts)
        gen(program, stmt);
      builder.push_jmp(endif.get());
      program.pop_scope();
    }
    // if (cond0) {} else if (cond1) ... else {}
    else {
      auto *cond_header = F->add_block("cond_header");
      builder.push_jmp(cond_header);

      i32 i = 0;
      for (; i < conds.size() - 1; i++) {
        builder.set_cursor(cond_header);
        auto cond = gen(program, conds[i]);
        auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i]));

        auto me = F->add_block("me");
        auto *next_cond_header = F->add_block("next_cond_header");
        builder.push_br(cond, me, next_cond_header);

        builder.set_cursor(me);
        program.push_scope();
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
        program.pop_scope();

        cond_header = next_cond_header;
      }

      builder.set_cursor(cond_header);
      auto cond = gen(program, conds[i]);
      auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i]));
      auto me = F->add_block("me");
      if (blks.size() == conds.size()) {
        builder.push_br(cond, me, endif.get());
        builder.set_cursor(me);
        program.push_scope();
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
        program.pop_scope();
      } else {
        auto else_stmts = ast.get_array_of<Ast::NodePtr>(ast.at(blks[i + 1]));
        auto else_bb = F->add_block("else");
        builder.push_br(cond, me, else_bb);

        builder.set_cursor(me);
        program.push_scope();
        for (auto stmt : stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
        program.pop_scope();

        builder.set_cursor(else_bb);
        program.push_scope();
        for (auto stmt : else_stmts)
          gen(program, stmt);
        builder.push_jmp(endif.get());
        program.pop_scope();
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
    program.push_scope();
    auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(node.rhs));
    for (auto stmt : stmts)
      gen(program, stmt);
    builder.push_jmp(loop_header);
    program.pop_scope();

    builder.set_cursor(loop_end);
    return nullptr;
  } break;

  case Ast::AST_FOR_STMT: {
    auto F = builder.get_cursor()->get_parent();
    auto for_data = ast.get<Ast::ForData>(node);

    program.push_scope();
    auto preheader = F->add_block("loop_preheader");
    builder.push_jmp(preheader);

    builder.set_cursor(preheader);
    gen(program, node.lhs); // generate init stmt, pushes store for loop var

    auto header = F->add_block("loop_header");
    builder.push_jmp(header);

    auto loop = F->add_block("loop");
    auto loop_end = F->add_block("loop_end");

    builder.set_cursor(header);
    auto cond = gen(program, for_data.cond);
    builder.push_br(cond, loop, loop_end);

    builder.set_cursor(loop);
    auto stmts = ast.get_array_of<Ast::NodePtr>(ast.at(for_data.blk));
    for (auto stmt : stmts)
      gen(program, stmt);
    gen(program, for_data.cont);

    builder.push_jmp(header);
    program.pop_scope();

    builder.set_cursor(loop_end);
    return nullptr;
  } break;

  default:
    return nullptr;
  }
}

IRContext &IRGenerator::get_context() { return ctx; }

} // namespace ir
} // namespace neo
