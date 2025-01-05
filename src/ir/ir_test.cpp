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
      "if (1 < 2) { a += 1; } else if (2 < 3) { a = 2; } else { a = b; }"
      " return a + b;"
      "}");
  IRGenerator generator(ast);
  auto program = generator.make_program();
  auto &funcs = program.get_functions();

  for (auto &[_, func] : funcs) {

    /*for (auto &bb : func.get_blocks()) {*/
    /*  bb->label().debug_print();*/
    /*  std::cout << ":" << std::endl;*/
    /**/
    /*  std::cout << "Preds: " << std::endl;*/
    /*  for (auto &pred : bb->get_preds()) {*/
    /*    std::cout << "\t\t";*/
    /*    pred->label().debug_print();*/
    /*    std::cout << std::endl;*/
    /*  }*/
    /**/
    /*  std::cout << "Succs: " << std::endl;*/
    /*  for (auto &succ : bb->get_succs()) {*/
    /*    std::cout << "\t\t";*/
    /*    succ->label().debug_print();*/
    /*    std::cout << std::endl;*/
    /*  }*/
    /*}*/
  }

  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << std::endl;

  for (auto &[_, func] : funcs) {
    func.debug_print();
    add_phi_nodes(generator.get_context(), func);
    func.debug_print();
  }
}
} // namespace ir
} // namespace neo
