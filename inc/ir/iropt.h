#pragma once

#include <ir/ir.h>
#include <unordered_set>

namespace neo {
namespace ir {
using Graph =
    std::unordered_map<BasicBlock *, std::unordered_set<BasicBlock *>>;

void graph_print(const Graph &g);

Graph dominators(const std::vector<std::unique_ptr<BasicBlock>> &blocks);

Graph dominator_tree(const Graph &dominators);

BasicBlock *immediate_dominator(const Graph &dominators, BasicBlock *bb);

Graph dominance_frontiers(
    const std::vector<std::unique_ptr<BasicBlock>> &blocks);

} // namespace ir
} // namespace neo
