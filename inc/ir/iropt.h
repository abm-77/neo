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

Graph dominators(Function &function);

Graph dominator_tree(const Graph &dominators);

BasicBlock *immediate_dominator(const Graph &dominators, BasicBlock *bb);

Graph dominance_frontiers(Function &function);

std::unordered_map<BasicBlock *, std::unordered_set<Value *>>
phi_locations(Function &function);

void add_phi_nodes(IRContext &ctx, Function &function);

} // namespace ir
} // namespace neo
