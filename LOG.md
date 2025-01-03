LOG
====

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
