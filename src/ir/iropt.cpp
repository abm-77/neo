#include <iostream>
#include <ir/iropt.h>

namespace neo {
namespace ir {

void graph_print(const Graph &g) {
  for (auto &[bb, connections] : g) {
    bb->label().debug_print();
    std::cout << ": " << std::endl;
    for (auto &conn : connections) {
      std::cout << "\t";
      conn->label().debug_print();
      std::cout << std::endl;
    }
  }
}

void graph_insert_edge(Graph &g, BasicBlock *key, BasicBlock *val) {
  g.try_emplace(key, std::unordered_set<BasicBlock *>())
      .first->second.insert(val);
}

Graph dominators(const std::vector<std::unique_ptr<BasicBlock>> &blocks) {
  Graph dominators;
  while (true) {
    auto old_doms = dominators;
    for (auto &bb : blocks) {
      std::vector<std::unordered_set<BasicBlock *>> sets;
      for (auto p : bb->get_preds()) {
        if (auto it = dominators.find(p); it != dominators.end()) {
          sets.push_back(it->second);
        }
      }

      std::unordered_set<BasicBlock *> intersection;
      if (sets.size() > 0) {
        intersection = sets[0];
        for (auto &set : sets) {
          for (auto it = intersection.begin(); it != intersection.end();) {
            it = (!set.contains(*it)) ? intersection.erase(it) : ++it;
          }
        }
      }
      intersection.insert(bb.get());
      dominators[bb.get()] = std::move(intersection);
    }
    if (dominators == old_doms)
      break;
  }
  return dominators;
}

Graph dominator_tree(const Graph &dominators) {
  Graph dt;
  for (auto &[bb, doms] : dominators) {
    auto idom = immediate_dominator(dominators, bb);
    if (idom)
      graph_insert_edge(dt, idom, bb);
  }
  return dt;
}

BasicBlock *immediate_dominator(const Graph &dominators, BasicBlock *bb) {
  auto &doms = dominators.at(bb);
  if (doms.size() == 1)
    return nullptr;

  BasicBlock *idom = nullptr;
  for (BasicBlock *dom : doms) {
    if (dom == bb)
      continue;
    if (idom == nullptr) {
      idom = dom;
    }
    // if our candidate idom dominates anoher one of our dominators, we should
    // update idom to the other dominator because that dominator is closer.
    else if (auto &domdoms = dominators.at(dom); domdoms.contains(idom)) {
      idom = dom;
    }
  }
  return idom;
}

Graph dominance_frontiers(
    const std::vector<std::unique_ptr<BasicBlock>> &blocks) {
  Graph dom_front;
  auto doms = dominators(blocks);
  for (auto &block : blocks) {
    auto &preds = block->get_preds();
    if (preds.size() < 2)
      continue;
    for (auto pred : preds) {
      BasicBlock *run_dom;
      BasicBlock *runner = pred;
      while (runner != immediate_dominator(doms, block.get())) {
        graph_insert_edge(dom_front, runner, block.get());
        run_dom = immediate_dominator(doms, runner);
        runner = run_dom;
      }
    }
  }
  return dom_front;
}

} // namespace ir
} // namespace neo
