#include <codegen/codegen.h>
#include <gtest/gtest.h>
#include <ir/ir_gen.h>
#include <string_view>

namespace neo {
namespace codegen {

using namespace parse;

Ast parse_input(std::string_view input) {
  auto L = Lexer::from_source(input);
  auto result = L.lex();
  return Parser::parse(result);
}

TEST(CodegenTest, Gen) {
  /*auto ast = parse_input(*/
  /*    "fn add(a: int, b: int) int {"*/
  /*    " if (1 < 2) { a = 1; } else if (3 > 4) { a += 3; } else { a = b; }"*/
  /*    " return a + b;"*/
  /*    "}");*/
  /*auto ast = parse_input("fn add(a: int, b: int) int {"*/
  /*                       "for (var i: int = 0; i < 10; i += 1) { a += 1; }"*/
  /*                       " return a + b;"*/
  /*                       "}");*/
  auto ast = parse_input("fn add(a: int, b: int) int {"
                         "var i: int = 0;"
                         "while (i < 10) { a += 1; }"
                         " return a + b;"
                         "}");
  IRGenerator IG(ast);
  auto program = IG.make_program();

  CodeGenerator CG(IG.get_context(), program, "programs/asm/test.S");
  CG.gen();
}

} // namespace codegen
} // namespace neo
