project('neo', 'cpp', default_options : ['cpp_std=c++20'])

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_main_dep')
gmock_dep = gtest_proj.get_variable('gmock_dep')

driver_src = files(
  'src/main.cpp'
)
compiler_src = files(
  'src/frontend/lexer.cpp',
  'src/frontend/parser.cpp',
)
executable(meson.project_name(), driver_src + compiler_src)

frontend_test_src = files(
  'src/frontend/lexer_test.cpp',
  'src/frontend/parser_test.cpp',
)
frontend_test_exec = executable(
  'frontend',
  frontend_test_src + compiler_src,
  dependencies : [gtest_dep, gmock_dep]
)
test('frontend', frontend_test_exec)


