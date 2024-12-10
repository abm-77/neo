project('neo', 'cpp')

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_main_dep')
gmock_dep = gtest_proj.get_variable('gmock_dep')

prog_src = files(
  'src/frontend/lexer.cpp'
)

test_src = files(
  'src/frontend/lexer_test.cpp'
)

test_exec = executable(
  'gtest',
  test_src + prog_src,
  dependencies : [gtest_dep, gmock_dep]
)

test('gtest', test_exec)

