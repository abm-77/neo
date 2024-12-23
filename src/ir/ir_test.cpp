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
  // clang-format off
  auto ast = parse_input(
    "fn sub (a: int, b: int) int { return 1 - 2; }"
    "fn add(a: int, b: int) int {"
    " while (1 < 5) { sub (1, 2); }"
    " return 1 + 2;"
    "}"
  );
  IRGenerator generator(ast);
  auto program = generator.make_program();
  auto& funcs =  program.get_functions();

  for (auto& [_, func] : funcs) {
    for (auto& bb : func.get_blocks()) {
      std::cout << "block: " << bb->get_name() << std::endl;
    }
  }
}
} // namespace ir
} // namespace neo
