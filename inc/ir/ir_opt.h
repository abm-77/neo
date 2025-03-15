#pragma once

#include <ir/ir.h>
#include <unordered_set>

namespace neo {
namespace ir {
using Graph =
    std::unordered_map<BasicBlock *, std::unordered_set<BasicBlock *>>;

struct Edge {
  BasicBlock *incoming_block;
  Value *incoming_value;

  b32 operator==(const Edge &other) const;
  b32 operator!=(const Edge &other) const;

  struct Hash {
    size_t operator()(const Edge &edge) const;
  };

  struct Equal {
    b32 operator()(const Edge &lhs, const Edge &rhs) const;
  };
};

void graph_print(const Graph &g);

/*
 * TODO:
 * - make a unified way of passing dominator information around.
 *   currently, we derive it in every function. we should reuse it.
 * */
class IROptimizer {
public:
  IROptimizer(IRContext &ctx, Program &program);
  void optimize();

  // dominator helpers
  Graph get_dominators(Function &function);
  Graph get_dominator_tree(const Graph &dominators);
  BasicBlock *get_immediate_dominator(const Graph &dominators, BasicBlock *bb);
  Graph
  get_dominance_frontiers(std::vector<std::unique_ptr<BasicBlock>> &blocks,
                          const Graph &doms);

  // loop helpers
  std::vector<std::unordered_set<BasicBlock *>> find_loops(Function &function);

  // TODO: move block helpers into BasicBlock class
  //
  // block helpers
  std::vector<Value *> find_alloca_variables(Function &function);
  std::unordered_map<Value *,
                     std::pair<std::unordered_set<BasicBlock *>, Value *>>
  find_stores(Function &function);

  // control-flow helpers
  std::unordered_map<BasicBlock *, std::unordered_set<Value *>>
  get_phi_locations(Function &function);

  // optimization passes
  void do_alloca_promotion_pass(IRContext &ctx, Function &function);
  void do_costant_folding_pass(IRContext &ctx, Function &function);
  void do_branch_elimination_pass(IRContext &ctx, Function &function);
  void do_loop_optimization_pass(IRContext &ctx, Function &function);
  void do_remove_phi_nodes_pass(IRContext &ctx, Function &function);

private:
  IRContext &ctx;
  Program &program;
};

} // namespace ir
} // namespace neo
