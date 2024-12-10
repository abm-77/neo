project('neo', 'cpp')

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_main_dep')
gmock_dep = gtest_proj.get_variable('gmock_dep')

driver_src = files(
  'src/main.cpp'
)

compiler_src = files(
  'src/frontend/lexer.cpp'
)

test_src = files(
  'src/frontend/lexer_test.cpp'
)

test_exec = executable(
  'gtest',
  test_src + compiler_src,
  dependencies : [gtest_dep, gmock_dep]
)
test('gtest', test_exec)

executable(meson.project_name(), driver_src + compiler_src)

