#include <ir/irgen.h>
#include <ir/iropt.h>

#include <gtest/gtest.h>
#include <iostream>

namespace neo {
namespace ir {

using namespace parse;

Ast parse_input(std::string_view input) {
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  return Parser::parse(result);
}

TEST(IR, IrTest) {
  auto ast = parse_input(
      "fn add(a: int, b: int) int {"
      "if (1 < 2) { a += 1; } else if (2 < 3) { a += 2; } else { a += 3; }"
      " return b + 2;"
      "}");
  IRGenerator generator(ast);
  auto program = generator.make_program();
  auto &funcs = program.get_functions();

  for (auto &[_, func] : funcs) {
    for (auto &bb : func.get_blocks()) {
      bb->label().debug_print();
      std::cout << ":" << std::endl;

      std::cout << "Preds: " << std::endl;
      for (auto &pred : bb->get_preds()) {
        std::cout << "\t\t";
        pred->label().debug_print();
        std::cout << std::endl;
      }

      std::cout << "Succs: " << std::endl;
      for (auto &succ : bb->get_succs()) {
        std::cout << "\t\t";
        succ->label().debug_print();
        std::cout << std::endl;
      }
    }
  }

  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << std::endl;

  for (auto &[_, func] : funcs) {
    auto doms = dominators(func.get_blocks());
    // auto dom_tree = dominator_tree(doms);
    // auto dom_fronts = dominance_frontiers(func.get_blocks());
    // graph_print(dom_fronts);
    graph_print(dominators(func.get_blocks()));
    std::cout << std::endl;
    std::cout << std::endl;
    graph_print(dominator_tree(doms));
  }
}
} // namespace ir
} // namespace neo
