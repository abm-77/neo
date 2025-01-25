#include <common/types.h>
#include <iostream>
#include <ir/ir.h>
#include <ir/ir_opt.h>

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

IROptimizer::IROptimizer(IRContext &ctx, Program &program)
    : ctx(ctx), program(program) {}

void IROptimizer::optimize() {
  for (auto &[_, func] : program.get_functions()) {
    do_alloca_promotion_pass(ctx, func);
    do_costant_folding_pass(ctx, func);
    do_remove_phi_nodes_pass(ctx, func);
  }
}

Graph IROptimizer::get_dominators(Function &function) {
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

Graph IROptimizer::get_dominator_tree(const Graph &dominators) {
  Graph dt;
  for (auto &[bb, doms] : dominators) {
    auto idom = get_immediate_dominator(dominators, bb);
    if (idom)
      default_insert(dt, idom, bb, std::unordered_set<BasicBlock *>());
  }
  return dt;
}

BasicBlock *IROptimizer::get_immediate_dominator(const Graph &dominators,
                                                 BasicBlock *bb) {
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

Graph IROptimizer::get_dominance_frontiers(
    std::vector<std::unique_ptr<BasicBlock>> &blocks, const Graph &doms) {
  Graph dom_front;
  for (auto &block : blocks) {
    auto &preds = block->get_preds();
    if (preds.size() < 2)
      continue;
    for (auto pred : preds) {
      BasicBlock *run_dom;
      BasicBlock *runner = pred;
      while (runner != get_immediate_dominator(doms, block.get())) {
        default_insert(dom_front, runner, block.get(),
                       std::unordered_set<BasicBlock *>());
        run_dom = get_immediate_dominator(doms, runner);
        runner = run_dom;
      }
    }
  }
  return dom_front;
}

// TODO differentiate between stores to variables and stores to arrays/pointers
std::unordered_map<Value *,
                   std::pair<std::unordered_set<BasicBlock *>, Value *>>
IROptimizer::find_stores(Function &function) {
  std::unordered_map<Value *,
                     std::pair<std::unordered_set<BasicBlock *>, Value *>>
      stores;
  for (auto &bb : function.get_blocks()) {
    for (auto &instr : bb->get_instructions()) {
      if (instr->get_op() != OP_STR)
        continue;

      // add basic block to list of store sites for value
      stores
          .try_emplace(instr->get_operand(0),
                       std::pair<std::unordered_set<BasicBlock *>, Value *>())
          .first->second.first.insert(bb.get());

      // record initial store value
      if (!stores[instr->get_operand(0)].second)
        stores[instr->get_operand(0)].second = instr->get_operand(1);
    }
  }
  return stores;
}

void IROptimizer::do_alloca_promotion_pass(IRContext &ctx, Function &function) {
  using ValueStack = std::unordered_map<Value *, std::vector<Value *>>;

  auto doms = get_dominators(function);
  auto store_locs = find_stores(function);
  auto dom_fronts = get_dominance_frontiers(function.get_blocks(), doms);
  auto dom_tree = get_dominator_tree(doms);

  ValueStack val_stack;
  std::unordered_map<Edge, Value *> phi_dests;
  std::unordered_map<BasicBlock *, std::unordered_set<Value *>> phi_locs;
  std::unordered_map<Edge, std::vector<std::pair<BasicBlock *, Value *>>>
      phi_args;

  // find locations to place phi nodes
  while (true) {
    auto old_locs = store_locs;
    for (auto [var, stores] : old_locs) {
      auto &[store_blocks, initial_store] = stores;
      // TODO: handle loads before intial stores(?)
      // set the intial value of the variable
      default_push_back(val_stack, var, initial_store, std::vector<Value *>());
      for (auto store_block : store_blocks) {
        for (auto df : dom_fronts[store_block]) {
          default_insert(phi_locs, df, var, std::unordered_set<Value *>());
          if (!store_blocks.contains(df)) {
            auto def = std::unordered_map<
                Value *,
                std::pair<std::unordered_set<BasicBlock *>, Value *>>();

            store_locs
                .try_emplace(
                    var, std::pair<std::unordered_set<BasicBlock *>, Value *>())
                .first->second.first.insert(df);
          }
        }
      }
    }
    if (old_locs == store_locs) {
      break;
    }
  }

  std::vector<std::pair<BasicBlock *, ValueStack>> worklist{
      {function.get_entry(), val_stack}};
  while (!worklist.empty()) {
    auto [bb, curr_val_stack] = worklist.back();
    worklist.pop_back();

    // generate new values to store the results of each phi node
    if (phi_locs.contains(bb)) {
      for (auto phi_val : phi_locs[bb]) {
        auto phi_dest = ctx.new_value("phitemp", phi_val->get_type());
        default_push_back(curr_val_stack, phi_val, phi_dest,
                          std::vector<Value *>());
        phi_dests[Edge{.incoming_block = bb, .incoming_value = phi_val}] =
            phi_dest;
      }
    }

    for (auto &instr : bb->get_instructions()) {
      // get the value being stored to this variable and push it onto the stack
      // then eliminate store.
      if (instr->get_op() == OP_STR) {
        auto store_var = instr->get_operand(0);
        auto store_val = instr->get_operand(1);
        curr_val_stack.try_emplace(store_var, std::vector<Value *>())
            .first->second.push_back(store_val);
        store_var->get_def_instr()->kill();
        instr->kill();
      }
      // replace all uses of the value being loaded into with the most recent
      // value stored to the variable being loaded from then eliminate load
      else if (instr->get_op() == OP_LD) {
        auto load_val = instr->get_dest();
        auto load_var = instr->get_operand(0);
        load_val->replace_all_uses_with(curr_val_stack[load_var].back());
        instr->kill();
      }
    }

    for (auto succ : bb->get_succs()) {
      // add incoming phi edges using basic block and most recent stored value
      // for variable (back of stack)
      for (auto phi_val : phi_locs[succ]) {
        auto e = Edge{.incoming_block = succ, .incoming_value = phi_val};
        std::pair<BasicBlock *, Value *> v = {bb,
                                              curr_val_stack[phi_val].back()};
        auto def = std::vector<std::pair<BasicBlock *, Value *>>();
        default_push_back(phi_args, e, v, def);
      }
    }

    // add blocks dominated by the current block to worklist
    for (auto sub : dom_tree[bb]) {
      worklist.push_back({sub, curr_val_stack});
    }
  }

  // insert phi nodes into basic blocks
  for (auto [edge, dest] : phi_dests) {
    auto phi_instr =
        edge.incoming_block->prepend_instr(OP_PHI, dest->get_type());
    phi_instr->set_dest(dest);
    dest->set_def_instr(phi_instr);

    auto args = phi_args[edge];
    for (auto [block, val] : args) {
      phi_instr->add_block(block);
      phi_instr->add_operand(val);
      val->add_user(phi_instr);
    }
  }

  function.remove_dead_instrs();
}

// TODO: Make constant folding pass agnostic of expression chain order.
// Currently, if an expression is folded into a non-const instruction, e.g.,
// a infix expression, this pass will not be able to optimize it unless the
// expression is rearranged such that the non-const operands are folded into a
// const operand.
void IROptimizer::do_costant_folding_pass(IRContext &ctx, Function &function) {
  b32 constant_folded = false;
  do {
    for (auto &block : function.get_blocks()) {
      for (auto &instr : block->get_instructions()) {
        if (instr->has_const_operands()) {
          Value *folded_value = nullptr;
          auto lhs = instr->get_operand(0)->get_const_value();
          Value::ConstantValue rhs = -1;
          if (instr->get_operands().size() > 1)
            rhs = instr->get_operand(1)->get_const_value();

          switch (instr->get_op()) {
          case OP_ADD: {
            folded_value = ctx.new_value("addfold", std::get<i32>(lhs) +
                                                        std::get<i32>(rhs));
          } break;
          case OP_SUB: {
            folded_value = ctx.new_value("subfold", std::get<i32>(lhs) -
                                                        std::get<i32>(rhs));
          } break;
          case OP_MUL: {
            folded_value = ctx.new_value("mulfold", std::get<i32>(lhs) *
                                                        std::get<i32>(rhs));
          } break;
          case OP_DIV: {
            // TODO: Handle divide by zero
            folded_value = ctx.new_value("mulfold", std::get<i32>(lhs) /
                                                        std::get<i32>(rhs));
          } break;
          case OP_NEG: {
            folded_value = ctx.new_value("negfold", -std::get<i32>(lhs));
          } break;
          case OP_AND: {
            folded_value = ctx.new_value("andfold", std::get<b32>(lhs) &&
                                                        std::get<b32>(rhs));
          } break;
          case OP_OR: {
            folded_value = ctx.new_value("orfold", std::get<b32>(lhs) ||
                                                       std::get<b32>(rhs));
          } break;
          case OP_NOT: {
            folded_value = ctx.new_value("orfold", !std::get<b32>(lhs));
          } break;
          case OP_CEQ: {
            folded_value = ctx.new_value("ceqfold", lhs == rhs);
          } break;
          case OP_CNE: {
            folded_value = ctx.new_value("cnefold", lhs != rhs);
          } break;
          case OP_CLT: {
            folded_value = ctx.new_value("cltfold", std::get<i32>(lhs) <
                                                        std::get<i32>(rhs));
          } break;
          case OP_CGT: {
            folded_value = ctx.new_value("cgtfold", std::get<i32>(lhs) >
                                                        std::get<i32>(rhs));
          } break;
          case OP_CLE: {
            folded_value = ctx.new_value("clefold", std::get<i32>(lhs) <=
                                                        std::get<i32>(rhs));
          } break;
          case OP_CGE: {
            folded_value = ctx.new_value("cgefold", std::get<i32>(lhs) >=
                                                        std::get<i32>(rhs));
          } break;
          default: {
            assert(false && "unreachable");
          } break;
          }

          instr->get_dest()->replace_all_uses_with(folded_value);
          instr->kill();
        }
      }
    }
    function.remove_dead_instrs();
  } while (constant_folded);
}

void IROptimizer::do_remove_phi_nodes_pass(IRContext &ctx, Function &function) {
  struct Location {
    BasicBlock *block;
    Value *dest;
    Value *val;
  };

  std::vector<Location> locations;
  for (auto &bb : function.get_blocks()) {
    for (auto &instr : bb->get_instructions()) {
      if (instr->get_op() == OP_PHI) {
        for (i32 i = 0; i < instr->get_blocks().size(); i++) {
          locations.push_back({
              .block = instr->get_block(i),
              .dest = instr->get_dest(),
              .val = instr->get_operand(i),
          });
        }
        instr->kill();
      }
    }
  }

  for (auto loc : locations) {
    auto instr = loc.block->push_instr_before_end(
        OP_MOV, ctx.get_type(Type("void", 0, 0)));
    instr->set_dest(loc.dest);
    instr->add_operand(loc.val);
  }

  function.remove_dead_instrs();
}

} // namespace ir
} // namespace neo
