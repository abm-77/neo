#include <iostream>
#include <ir/ir.h>
#include <ir/iropt.h>

namespace std {
template <> struct hash<neo::ir::Edge> {
  size_t operator()(const neo::ir::Edge edge) const {
    return neo::ir::Edge::Hash()(edge);
  }
};
} // namespace std
  //
namespace neo {
namespace ir {

size_t Edge::Hash::operator()(const Edge &edge) const {
  size_t hash = std::hash<const BasicBlock *>()(edge.incoming_block);
  hash ^= std::hash<const Value *>()(edge.incoming_value) + +0x9e3779b9 +
          (hash << 6) + (hash >> 2);
  return hash;
}

b32 Edge::operator==(const Edge &other) const {
  return (incoming_block == other.incoming_block) &&
         (incoming_value == other.incoming_value);
}

b32 Edge::operator!=(const Edge &other) const { return !(*this == other); }

b32 Edge::Equal::operator()(const Edge &lhs, const Edge &rhs) const {
  return lhs == rhs;
}

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

Graph dominators(Function &function) {
  Graph dominators;
  auto &blocks = function.get_blocks();

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

Graph dominance_frontiers(std::vector<std::unique_ptr<BasicBlock>> &blocks,
                          const Graph &doms) {
  Graph dom_front;
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

std::vector<Value *> alloca_variables(Function &function) {
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

std::unordered_map<Value *, std::unordered_set<BasicBlock *>>
find_stores(Function &function) {
  std::unordered_map<Value *, std::unordered_set<BasicBlock *>> stores;
  for (auto &bb : function.get_blocks()) {
    for (auto &instr : bb->get_instructions()) {
      if (instr->get_op() != OP_STR)
        continue;

      stores.try_emplace(instr->get_dest(), std::unordered_set<BasicBlock *>())
          .first->second.insert(bb.get());
    }
  }
  return stores;
}

void add_phi_nodes(IRContext &ctx, Function &function) {
  auto doms = dominators(function);
  auto store_locs = find_stores(function);
  auto dom_fronts = dominance_frontiers(function.get_blocks(), doms);
  auto dom_tree = dominator_tree(doms);

  std::unordered_map<BasicBlock *, std::unordered_set<Value *>> phi_locs;
  for (auto [var, store_blocks] : store_locs) {
    auto worklist = std::vector(store_blocks.begin(), store_blocks.end());
    while (!worklist.empty()) {
      auto block = worklist.back();
      worklist.pop_back();
      for (auto df : dom_fronts[block]) {
        phi_locs.try_emplace(df, std::unordered_set<Value *>())
            .first->second.insert(var);

        if (!store_blocks.contains(df)) {
          worklist.push_back(df);
        }
      }
    }
  }

  std::vector<BasicBlock *> worklist{function.get_entry()};
  std::unordered_map<Edge, Value *> phi_dests;
  std::unordered_map<Edge, std::vector<std::pair<Label, Value *>>> phi_args;
  std::unordered_map<Value *, std::vector<Value *>> val_stack;

  while (!worklist.empty()) {
    auto bb = worklist.back();
    worklist.pop_back();

    auto old_stack = val_stack;

    for (auto phi_val : phi_locs[bb]) {
      auto phi_dest = ctx.new_value("phitemp", phi_val->get_type());
      phi_dests[Edge{.incoming_block = bb, .incoming_value = phi_val}] =
          phi_dest;
    }

    for (auto &instr : bb->get_instructions()) {
      if (instr->get_op() == OP_STR) {
        auto old = instr->get_dest();
        val_stack.try_emplace(old, std::vector<Value *>())
            .first->second.push_back(instr->get_operand(1));
      }
    }

    for (auto succ : bb->get_succs()) {
      for (auto phi_val : phi_locs[succ]) {
        auto e = Edge{.incoming_block = succ, .incoming_value = phi_val};

        phi_args.try_emplace(e, std::vector<std::pair<Label, Value *>>())
            .first->second.push_back({bb->label(), val_stack[phi_val].back()});
      }
    }

    for (auto sub : dom_tree[bb]) {
      worklist.push_back(sub);
    }

    val_stack.clear();
    val_stack.insert(old_stack.begin(), old_stack.end());
  }

  std::cout << "phi_dests: " << std::endl;
  for (auto [edge, dest] : phi_dests) {
    std::cout << "bb: ";
    edge.incoming_block->label().debug_print();
    std::cout << std::endl;
    std::cout << "value: " << edge.incoming_value->get_name() << std::endl;
  }

  std::cout << std::endl;
  std::cout << std::endl;

  std::cout << "phi_args: " << std::endl;
  for (auto [edge, args] : phi_args) {
    std::cout << "bb to insert into: ";
    edge.incoming_block->label().debug_print();
    std::cout << std::endl;

    std::cout << "value to phi: " << edge.incoming_value->get_name()
              << std::endl;

    std::cout << "args: " << std::endl;
    for (auto [label, value] : args) {
      std::cout << "inc bb: ";
      label.debug_print();
      std::cout << std::endl;

      std::cout << "inc value: " << value->get_name() << std::endl;
    }
  }

  function.remove_dead_instrs();
}

} // namespace ir
} // namespace neo
