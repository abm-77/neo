#pragma once

#include <ir/ir.h>

namespace neo {
namespace ir {

struct LivenessResult {
  std::unordered_set<Value *> live_in;
  std::unordered_set<Value *> live_out;
};
using LivenessAnalysis = std::unordered_map<BasicBlock *, LivenessResult>;

// data flow analyses
LivenessAnalysis get_liveness(Function &function);

} // namespace ir
} // namespace neo
