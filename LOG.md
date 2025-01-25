LOG
====

TODO:
- [] differentiate between variable, array, and pointer allocas
- [] think about handling load before store
- [] add ir_opt tests
- [] add ir_anal tests
- [] add codegen_tests
- [] handle register spills

1/25/25:
- [X] phi node removal pass 
- [X] added mov instructions
- [X] direct substitution for constant vals
- [X] refactor register allocation
- [X] refactored branching instructions
- [X] removed some debug prints
- [X] fixed control-flow copy propagation bug
- [X] enforce calling convention on callee side
- [X] enforce calling convention on caller side (ish)


1/23/25:
- [X] assemble branches
- [X] assemble jumps
- [X] assemble arithmetic ops
- [X] assemble comparison ops
- [X] assemble loads and stores (to stack locations)

1/19/25:
- [X] add ir_gen tests
- [X] remove alloca instrs
- [X] add constant folding
- [X] break out ir analysis code into its own submodule
- [X] differentiate between instructions destination and other operands
- [X] perform liveness analysis
- [X] construct interference graph 
- [X] simplify interference graph 

1/15/25:
- [X] add basic ir tests.

1/04/25:
- [X] eliminate variable alloca loads
- [X] eliminate variable alloca stores
- [X] insert phi nodes with args

12/28/24
- [X] placed phi nodes
- [X] determined phi node args 

12/26/24
- [X] calculate dominators
- [X] calculate immediate dominators
- [X] calculate dominator trees
- [X] calculate dominance frontiers

12/25/24
- [X] generate unique label and block names 
- [X] flesh out debug print more

12/24/24
- [X] array lit irgen
- [X] array index irgen
- [X] pointer-array disambiguation (sorta)
- [X] added some debug printing
- [X] extended type registry with hashable types
- [X] fixed some errors with if-chain irgen
- [X] starting to name some of the values other than just random tmps

12/23/24
- [X] for stmt irgen 
- [X] var def irgen 
- [X] var assignment irgen 
- [X] var read irgen 
- [X] program symbol table 
- [X] implement (stack) memory instructions in ir 

12/22/24
- [X] design ir
- [X] implement ir backend ops
- [X] implement primitive literal expr irgen 
- [X] implement infix and prefix expr irgen 
- [X] implement call expr irgen 
- [X] implement expr stmt irgen 
- [X] implement ret stmt irgen 
- [X] implement func def stmt irgen 
- [X] implement if stmt irgen 
- [X] implement while stmt irgen 

12/21/24
- [X] finish testing for for stmts
- [X] parse function defs
- [X] parse function calls
- [X] parse array literals 
- [X] parse array variable defs 
- [X] parse array accesses 
- [X] revamp type representation

12/20/24
- [X] finish testing for while stmts
- [X] refactor parser to decouple ast 

12/16/24
- [X] parse return stmts
- [X] parse block stmts 
- [X] parse for stmts 
- [X] parse while stmts 
- [X] parse if stmts 

12/15/24
- [X] parse var def stmts
- [X] parse int lit exprs
- [X] parse bool lit exprs
- [X] parse identifier exprs
- [X] parse infix exprs
- [X] parse prefix exprs

12/11/24
- [X] handle unterminated string literals
- [X] add string literal lexing tests
- [X] start on parser

12/10/24
- [X] start basic lexer
- [X] write basic test cases for lexer
