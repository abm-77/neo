#pragma once

#include <array>
#include <common/types.h>
#include <fstream>
#include <ir/ir.h>
#include <ir/ir_anal.h>
#include <string_view>
#include <unordered_set>
#include <vector>

namespace neo {
namespace codegen {

constexpr i32 GPR_COUNT = 32;
constexpr i32 FPR_COUNT = 32;

using namespace ir;

class RegisterAllocation {
public:
  RegisterAllocation(Function &function, i32 register_count);

  void reserve_register(i32 reg, Value *value);
  void reserve_spill(Value *value);
  std::unordered_map<Value *, i32> allocate_registers();

private:
  std::vector<Value *> generate_interference_graph();

private:
  Function &function;
  i32 register_count;

  std::unordered_map<Value *, i32> allocation;
  std::vector<Value *> spilled_values;
  std::unordered_map<Value *, std::unordered_set<Value *>> graph;
};

class CodeGenerator {
private:
  struct StackLocations {
    std::unordered_map<Value *, u32> offsets;
    u32 space_allocated;
  };

public:
  CodeGenerator(IRContext &ctx, Program &program, std::string_view filepath);
  ~CodeGenerator();

  void gen();

private:
  void write_function(Function &func);
  void write_instruction(std::unordered_map<Value *, i32> &val2reg,
                         StackLocations &stack_locs, Instruction *instr);
  void finish();

private:
  IRContext &ctx;
  Program &program;
  std::array<Value *, GPR_COUNT> gpr_registers;
  std::array<Value *, GPR_COUNT> fpr_registers;

  std::string_view filepath;
  std::ofstream outfile;
};

} // namespace codegen
} // namespace neo
