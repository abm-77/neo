#include "common/types.h"
#include <codegen/codegen.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <ir/ir.h>
#include <ir/ir_anal.h>
#include <string_view>

#define write_text(msg) outfile << msg
#define write_line(msg) outfile << msg << std::endl
#define reg(val) "x" << val2reg[val]

#define branch_dest(block)                                                     \
  block->label().fn_name << "_" << block->label().name << "_"                  \
                         << block->label().number
#define block_label(block)                                                     \
  block->label().fn_name << "_" << block->label().name << "_"                  \
                         << block->label().number << ":"
#define newline() outfile << std::endl

namespace neo {
namespace codegen {

using namespace ir;

void write_value(std::ofstream &out, std::unordered_map<Value *, i32> &val2reg,
                 Value *val) {
  if (val2reg.contains(val)) {
    out << "x" << val2reg[val];
  } else {
    auto opt = val->get_const_value();
    if (auto v = std::get_if<i32>(&opt)) {
      out << "#" << *v;
    }
  }
}

static inline u32 pow_round16(uint32_t n) {
  if (n == 0)
    return 16;
  n--;
  n |= n >> 4;
  n |= n >> 8;
  n |= n >> 16;
  return (n + 1);
}

static std::vector<Value *> find_alloca_variables(Function &function) {
  std::vector<Value *> vars;
  for (auto &bb : function.get_blocks()) {
    for (auto &instr : bb->get_instructions()) {
      if (instr->get_op() == OP_ALLOCA) {
        vars.push_back(instr->get_dest());
      }
    }
  }
  return vars;
}

static std::string_view op2mnemonic(InstructionOp op) {
  switch (op) {
  case OP_ADD:
    return "add ";
  case OP_SUB:
    return "sub ";
  case OP_MUL:
    return "mul ";
  case OP_DIV:
    return "div ";
  case OP_NEG:
    return "neg ";
  case OP_NOT:
    return "div ";
  case OP_AND:
    return "and ";
  case OP_OR:
    return "orr ";
  case OP_JMP:
    return "b ";
  case OP_CALL:
    return "bl ";
  case OP_RET:
    return "ret ";
  case OP_LD:
    return "ldr ";
  case OP_STR:
    return "str ";
  case OP_BR:
    return "cbz ";
  default:
    return "";
  }
}

CodeGenerator::CodeGenerator(IRContext &ctx, Program &program,
                             std::string_view filepath)
    : ctx(ctx), program(program), filepath(filepath) {
  outfile.open(filepath.data());
  memset((void *)gpr_registers.data(), 0, GPR_COUNT);
  memset((void *)fpr_registers.data(), 0, FPR_COUNT);
}

CodeGenerator::~CodeGenerator() {
  if (outfile.is_open())
    finish();
}

void CodeGenerator::gen() {
  program.debug_print();
  write_line(".section .text");
  for (auto &[func_name, func] : program.get_functions()) {
    write_text("\t.global ");
    write_line(func_name);
  }
  newline();
  for (auto &[func_name, func] : program.get_functions()) {
    write_function(func);
  }
}

void CodeGenerator::finish() { outfile.close(); }

void CodeGenerator::write_function(Function &func) {
  RegisterAllocation alloc(func, GPR_COUNT);
  i32 i = 0;
  for (i = 0; i < 8 && i < func.get_params().size(); i++) {
    alloc.reserve_register(i, func.get_params()[i].value);
  }
  for (; i < func.get_params().size(); i++) {
    alloc.reserve_spill(func.get_params()[i].value);
  }
  auto val2reg = alloc.allocate_registers();

  StackLocations stack_locs;
  stack_locs.space_allocated = 16;
  auto alloca_vars = find_alloca_variables(func);
  for (auto alloc : alloca_vars) {
    stack_locs.offsets[alloc] = stack_locs.space_allocated;
    if (alloc->get_type()->get_derived_type() == ir::Type::POINTER) {
      stack_locs.space_allocated +=
          alloc->get_type()->get_base_type()->get_size();
    } else if (alloc->get_type()->get_derived_type() == ir::Type::ARRAY) {
      stack_locs.space_allocated += alloc->get_type()->get_size();
    }
  }
  stack_locs.space_allocated = pow_round16(stack_locs.space_allocated);

  write_line(func.get_name() << ":");
  for (i32 i = 0; i < func.get_blocks().size(); i++) {
    auto block = func.get_blocks()[i].get();
    write_line(block_label(block));
    if (i == 0) {
      write_line("stp fp, lr, [sp, -#" << stack_locs.space_allocated << "]!");
      write_line("mov fp, sp");
    }
    for (auto &instr : block->get_instructions()) {
      if (instr->get_op() == OP_ALLOCA)
        continue;
      write_instruction(val2reg, stack_locs, instr.get());
    }
  }
  newline();
}

void CodeGenerator::write_instruction(std::unordered_map<Value *, i32> &val2reg,
                                      StackLocations &stack_locs,
                                      Instruction *instr) {
  if (instr->op_is_arithmetic()) {
    write_text(op2mnemonic(instr->get_op()) << reg(instr->get_dest()) << ", ");
    write_value(outfile, val2reg, instr->get_operand(0));
    write_text(", ");
    write_value(outfile, val2reg, instr->get_operand(1));
  } else if (instr->op_is_comparison()) {
    write_text("cmp ");
    write_value(outfile, val2reg, instr->get_operand(0));
    write_text(", ");
    write_value(outfile, val2reg, instr->get_operand(1));
    newline();

    write_text("cset " << reg(instr->get_dest()) << ", ");
    switch (instr->get_op()) {
    case OP_CEQ: {
      write_text("EQ");
    } break;
    case OP_CNE: {
      write_text("NE");
    } break;
    case OP_CLT: {
      write_text("LT");
    } break;
    case OP_CGT: {
      write_text("GT");
    } break;
    case OP_CLE: {
      write_text("LE");
    } break;
    case OP_CGE: {
      write_text("GE");
    } break;
    default: {
      write_text("INVALID");
    } break;
    }
  } else {
    switch (instr->get_op()) {
    case OP_LD: {
      write_text(op2mnemonic(instr->get_op())
                 << reg(instr->get_dest()) << ", [sp, "
                 << stack_locs.offsets[instr->get_operand(0)]);
      if (instr->get_operands().size() > 1) {
        write_text(
            ", " << std::get<i32>(instr->get_operand(1)->get_const_value()));
      }
      write_text("]");
    } break;
    case OP_STR: {
      write_text(op2mnemonic(instr->get_op())
                 << reg(instr->get_operand(1)) << ", [sp, "
                 << stack_locs.offsets[instr->get_operand(0)] << "]");
    } break;
    case OP_JMP: {
      write_line(op2mnemonic(instr->get_op())
                 << branch_dest(instr->get_block(0)));
    } break;
    case OP_BR: {
      write_line(op2mnemonic(instr->get_op())
                 << reg(instr->get_operand(0)) << ", "
                 << branch_dest(instr->get_block(1)));
    } break;
    case OP_RET: {
      if (instr->get_operands().size() > 0) {
        auto ret_val = instr->get_operand(0);
        auto reg = val2reg[ret_val];
        if (reg != 0) {
          write_line("mov x0, x" << reg << std::endl);
        }
      }
      write_line("ldp fp, lr, [sp], #" << stack_locs.space_allocated);
      write_text(op2mnemonic(instr->get_op()));
    } break;
    case OP_MOV: {
      if (!val2reg.contains(instr->get_operand(0)) ||
          val2reg[instr->get_dest()] != val2reg[instr->get_operand(0)]) {
        write_text("mov " << reg(instr->get_dest()) << ", ");
        write_value(outfile, val2reg, instr->get_operand(0));
      }
    } break;
    case OP_PHI: {
      write_text("phi " << reg(instr->get_dest()) << ", ");
      for (i32 i = 0; i < instr->get_blocks().size(); i++) {
        write_text(branch_dest(instr->get_block(i))
                   << ", " << reg(instr->get_operand(i)));
        if (i < instr->get_blocks().size() - 1) {
          write_text(", ");
        }
      }
    } break;
    case OP_CALL: {
      // TODO: Save caller-saved registers
      i32 i = 0;
      for (i = 0; i < 8 && i < instr->get_operands().size(); i++) {
        write_text("mov x" << i << ", ");
        write_value(outfile, val2reg, instr->get_operand(i));
        newline();
      }
      write_text("bl " << instr->get_function()->get_name());
      if (val2reg[instr->get_dest()] != 0) {
        newline();
        write_text("mov " << reg(instr->get_dest()) << ", x0");
      }
    } break;
    default: {
      write_text(op2str(instr->get_op()));
    } break;
    }
  }
  newline();
}

RegisterAllocation::RegisterAllocation(Function &function, i32 register_count)
    : function(function), register_count(register_count) {}

void RegisterAllocation::reserve_register(i32 reg, Value *value) {
  allocation[value] = reg;
}

void RegisterAllocation::reserve_spill(Value *value) {
  spilled_values.push_back(value);
}

std::vector<Value *> RegisterAllocation::generate_interference_graph() {
  auto insert_edge = [&](Value *a, Value *b) {
    default_insert(graph, a, b, std::unordered_set<Value *>());
    default_insert(graph, b, a, std::unordered_set<Value *>());
  };

  auto liveness = get_liveness(function);
  for (auto &block : function.get_blocks()) {
    auto &live_set = liveness[block.get()].live_out;
    for (auto it = block->get_instructions().rbegin();
         it != block->get_instructions().rend(); ++it) {
      auto instr = it->get();

      // instruction operands interfere with each other
      auto &operands = instr->get_operands();
      for (auto op_a = operands.begin(); op_a != operands.end(); ++op_a) {
        for (auto op_b = std::next(op_a); op_b != operands.end(); ++op_b) {
          insert_edge(*op_a, *op_b);
        }
      }

      // edges for defined values
      if (instr->has_dest()) {
        for (auto live_val : live_set) {
          if (live_val != instr->get_dest()) {
            insert_edge(instr->get_dest(), live_val);
          }
        }
        live_set.erase(instr->get_dest());
      }

      // add used values to live set
      for (auto op : operands) {
        live_set.insert(op);
      }
    }
  }

  for (auto [val, inter] : graph) {
    std::cout << val->get_name() << " interferes with: ";
    for (auto i : inter) {
      std::cout << i->get_name() << ", ";
    }
    std::cout << std::endl;
  }

  auto reduced = graph;
  std::vector<Value *> allocation_order;
  for (auto it = spilled_values.begin(); it != spilled_values.end(); ++it) {
    auto spilled = *it;
    allocation_order.push_back(spilled);
    for (auto &neighbor : reduced[spilled]) {
      reduced[neighbor].erase(spilled);
    }
    reduced.erase(spilled);
  }

  while (!reduced.empty()) {
    b32 simplified = false;
    for (auto it = reduced.begin(); it != reduced.end();) {
      // attempt to remove low-degree nodes
      if (it->second.size() < register_count) {
        allocation_order.push_back(it->first);
        for (auto &neighbor : it->second) {
          reduced[neighbor].erase(it->first);
        }
        it = reduced.erase(it);
        simplified = true;
      } else {
        ++it;
      }
    }

    // spill high-degree nodes if necessary
    if (!simplified) {
      auto spilled = reduced.begin()->first;
      allocation_order.push_back(spilled);
      reduced.erase(spilled);
    }
  }

  return allocation_order;
}

std::unordered_map<Value *, i32> RegisterAllocation::allocate_registers() {
  auto value_stack = generate_interference_graph();
  while (!value_stack.empty()) {
    auto value = value_stack.back();
    value_stack.pop_back();

    if (allocation.contains(value))
      continue;

    if (value->is_const_value())
      continue;

    // get used colors
    std::unordered_set<i32> used_colors;
    for (const auto &neighbor : graph[value]) {
      if (allocation.contains(neighbor)) {
        used_colors.insert(allocation[neighbor]);
      }
    }

    // assign new color to value
    for (i32 color = 0; color < register_count; color++) {
      if (!used_colors.contains(color)) {
        allocation[value] = color;
        break;
      }
    }

    // register must be spliied
    if (!allocation.contains(value)) {
      // TODO: Spill register
    }
  }

  for (auto &[val, reg] : allocation) {
    std::cout << "val: " << val->get_name() << " in  reg" << reg << std::endl;
  }

  return allocation;
}

} // namespace codegen
} // namespace neo
