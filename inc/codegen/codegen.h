#pragma once

#include <common/types.h>
#include <ir/ir.h>
#include <ir/ir_anal.h>
#include <unordered_set>

namespace neo {
namespace codegen {

using namespace ir;

class CodeGenerator {
public:
  CodeGenerator(IRContext &ctx, Program &program);

private:
  using InterferenceGraph =
      std::unordered_map<Value *, std::unordered_set<Value *>>;
  using GraphColoring = std::unordered_map<Value *, i32>;

  InterferenceGraph get_interference_graph(Function &function);
  std::vector<Value *> reduce_interference_graph(InterferenceGraph &graph,
                                                 i32 register_count);
  GraphColoring color_interference_graph(InterferenceGraph &graph,
                                         std::vector<Value *> value_stack,
                                         i32 register_count);

private:
  IRContext &ctx;
  Program &program;
};

} // namespace codegen
} // namespace neo
