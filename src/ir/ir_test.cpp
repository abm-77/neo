#include <ir/irgen.h>

#include <gtest/gtest.h>

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
      "fn sub (a: int, b: int) int { return 1 - 2; }"
      "fn add(a: int, b: int) int {"
      " while (1 < 5) { sub (1, 2); }"
      " var c: int = a;"
      " c = a / b;"
      " var cond: bool = a < b;"
      " if (cond) { a += 1; }"
      " if (1 < 2) { a += 1; } else if (2 < 3) { b += 1; } else { c += 1; }"
      " for (var t: int = 0; t < 10; t += 1) { a; }"
      " return b + 2;"
      "}");
  IRGenerator generator(ast);
  auto program = generator.make_program();
  auto &funcs = program.get_functions();

  for (auto &[_, func] : funcs) {
    for (auto &bb : func.get_blocks()) {
      std::cout << "block: " << bb->get_name() << std::endl;
    }
  }
}
} // namespace ir
} // namespace neo
