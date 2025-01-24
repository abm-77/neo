#pragma once

#include <array>
#include <common/types.h>
#include <fstream>
#include <ir/ir.h>
#include <ir/ir_anal.h>
#include <string_view>
#include <unordered_set>

namespace neo {
namespace codegen {

constexpr i32 GPR_COUNT = 32;
constexpr i32 FPR_COUNT = 32;

using namespace ir;

class CodeGenerator {
private:
  struct StackLocations {
    std::unordered_map<Value *, u32> offsets;
    u32 space_allocated;
  };
  using InterferenceGraph =
      std::unordered_map<Value *, std::unordered_set<Value *>>;
  using GraphColoring = std::unordered_map<Value *, i32>;

  void interference_graph_insert_edge(InterferenceGraph &graph, Value *a,
                                      Value *b);

public:
  CodeGenerator(IRContext &ctx, Program &program, std::string_view filepath);
  ~CodeGenerator();

  void gen();

private:
  InterferenceGraph get_interference_graph(Function &function);
  std::vector<Value *> reduce_interference_graph(InterferenceGraph graph,
                                                 i32 register_count);
  GraphColoring regalloc(Function &function, i32 register_count);

  void write_function(CodeGenerator::GraphColoring &val2reg, Function &func);
  void write_instruction(CodeGenerator::GraphColoring &val2reg,
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
