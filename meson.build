project('neo', 'cpp', default_options : ['cpp_std=c++20'],)

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_main_dep')
gmock_dep = gtest_proj.get_variable('gmock_dep')

driver_src = files(
  'src/main.cpp'
)

frontend_src = files(
  'src/frontend/lexer.cpp',
  'src/frontend/parser.cpp',
)

ir_src = files(
  'src/ir/ir.cpp',
  'src/ir/irgen.cpp',
  'src/ir/iropt.cpp',
)

codegen_src = files(
  'src/codegen/codegen.cpp',
)

compiler_src = frontend_src + ir_src + codegen_src

incdir = include_directories('./inc')
executable(meson.project_name(), driver_src + compiler_src,  include_directories : incdir)

frontend_test_src = files(
  'src/frontend/lexer_test.cpp',
  'src/frontend/parser_test.cpp',
)
frontend_test_exec = executable(
  'frontend',
  frontend_test_src + frontend_src,
  dependencies : [gtest_dep, gmock_dep],
  include_directories : incdir,
)
test('frontend', frontend_test_exec)


ir_test_src = files(
  'src/ir/ir_test.cpp',
)
ir_test_exec = executable(
  'ir',
  ir_test_src + ir_src + frontend_src,
  dependencies : [gtest_dep, gmock_dep],
  include_directories : incdir,
)
test('ir', ir_test_exec)

codegen_test_src = files(
  'src/codegen/codegen_test.cpp',
)
codegen_test_exec = executable(
  'codegen',
  codegen_test_src + codegen_src + ir_src + frontend_src,
  dependencies : [gtest_dep, gmock_dep],
  include_directories : incdir,
)
# test('codegen', codegen_test_exec)


