#include <ir/ir_anal.h>

namespace neo {
namespace ir {
LivenessAnalysis get_liveness(Function &function) {
  LivenessAnalysis analysis;
  b32 changed;

  do {
    changed = false;

    for (auto it = function.get_blocks().rbegin();
         it != function.get_blocks().rend(); ++it) {
      BasicBlock *block = it->get();
      LivenessResult &L = analysis[block];

      // live_out = union of live_in of all successors
      auto old_out = L.live_out;
      L.live_out.clear();
      for (auto &succ : block->get_succs()) {
        L.live_out.insert(analysis[succ].live_in.begin(),
                          analysis[succ].live_in.end());
      }

      // live_in = (live_out - def) U use
      auto use_def_info = block->get_uses_and_defs();
      auto new_in = L.live_out;
      for (auto def : use_def_info.defs) {
        new_in.erase(def);
      }
      new_in.insert(use_def_info.uses.begin(), use_def_info.uses.end());

      changed = new_in != L.live_in || old_out != L.live_out;

      L.live_in = std::move(new_in);
    }
  } while (changed);

  return analysis;
}

} // namespace ir
} // namespace neo
