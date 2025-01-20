#include "ir/ir.h"
#include "ir/ir_anal.h"
#include <codegen/codegen.h>

namespace neo {
namespace codegen {

using namespace ir;

CodeGenerator::InterferenceGraph
CodeGenerator::get_interference_graph(Function &function) {
  std::unordered_map<Value *, std::unordered_set<Value *>> graph;
  auto liveness = get_liveness(function);

  for (auto &block : function.get_blocks()) {
    auto &live_set = liveness[block.get()].live_out;
    for (auto it = block->get_instructions().rbegin();
         it != block->get_instructions().rend(); ++it) {
      auto instr = it->get();

      // edges for defined values
      if (instr->has_dest()) {
        for (auto live_val : live_set) {
          if (live_val != instr->get_dest()) {
            graph.try_emplace(instr->get_dest(), std::unordered_set<Value *>())
                .first->second.insert(live_val);
            graph.try_emplace(live_val, std::unordered_set<Value *>())
                .first->second.insert(instr->get_dest());
          }
        }
        live_set.erase(instr->get_dest());
      }

      // add used values to live set
      for (auto op : instr->get_operands()) {
        live_set.insert(op);
      }
    }
  }

  return graph;
}

std::vector<Value *>
CodeGenerator::reduce_interference_graph(InterferenceGraph &graph,
                                         i32 register_count) {
  std::vector<Value *> stack;
  while (!graph.empty()) {
    b32 simplified = false;

    for (auto it = graph.begin(); it != graph.end();) {
      // attempt to remove low-degree nodes
      if (it->second.size() < register_count) {
        stack.push_back(it->first);
        for (auto &neighbor : it->second) {
          graph[neighbor].erase(it->first);
        }
        it = graph.erase(it);
        simplified = true;
      } else {
        ++it;
      }
    }

    // spill high-degree nodes if necessary
    if (!simplified) {
      auto spilled = graph.begin()->first;
      stack.push_back(spilled);
      graph.erase(spilled);
    }
  }

  return stack;
}

} // namespace codegen
} // namespace neo
