#pragma once

#include <common/types.h>
#include <frontend/parser.h>

#include <memory>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace neo {
namespace ir {

struct Label {
  std::string_view fn_name;
  std::string_view name;
  u32 number;

  void debug_print() const;
};

class Instruction;
class Function;
class BasicBlock;
class IRContext;

class Type {
public:
  struct Hash {
    size_t operator()(const Type &type) const;
  };
  struct Equal {
    b32 operator()(const Type &lhs, const Type &rhs) const;
  };

public:
  enum DerivedType {
    NONE,
    POINTER,
    ARRAY,
  };

public:
  explicit Type(std::string_view name, u32 size = 0, u32 align = 4,
                DerivedType derived_type = NONE, Type *base_type = nullptr);

  static Type arr_of(Type *type, u32 size);
  static Type ptr_to(Type *type);

  const std::string_view &get_name() const;
  u32 get_size() const;
  u32 get_alignment() const;
  b32 is_primitive_type() const;
  DerivedType get_derived_type() const;
  Type *get_base_type() const;
  b32 operator==(const Type &other) const;
  b32 operator!=(const Type &other) const;

private:
  std::string_view name;
  u32 size;
  u32 alignment;

  DerivedType derived_type;
  Type *base_type;
};

class Value {
public:
  using ConstantValue = std::variant<i32, b32, f32>;

public:
  Value(std::string_view name, Type *type);
  Value(std::string_view name, Type *type, ConstantValue value);

  static Value int_value(i32 val);
  static Value bool_value(b32 val);

  const std::string_view get_name() const;
  void set_name(const std::string_view new_name);

  Type *get_type() const;
  void set_type(Type *new_type);

  Instruction *get_def_instr() const;
  void set_def_instr(Instruction *instr);

  void add_user(Instruction *user);
  const std::vector<Instruction *> &get_users() const;

  bool is_const_value() const;
  ConstantValue get_const_value();
  void set_const_value(ConstantValue value);

  void replace_all_uses_with(Value *value);

  void debug_print() const;

  b32 alive();
  void kill();

private:
  std::string_view name;
  Type *type;
  Instruction *def_instr;
  std::vector<Instruction *> users;
  std::optional<ConstantValue> constant_value;
  b32 dead;
};

enum InstructionOp {
  // arithmetic
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_NEG,

  // logical
  OP_NOT,
  OP_AND,
  OP_OR,

  // comparison
  OP_CEQ,
  OP_CNE,
  OP_CLT,
  OP_CGT,
  OP_CLE,
  OP_CGE,

  // control
  OP_JMP,
  OP_BR,
  OP_CALL,
  OP_RET,
  OP_PHI,

  // Memory
  OP_FREE,
  OP_ALLOCA,
  OP_LD,
  OP_STR,

  // misc
  OP_ID
};

const char *op2str(InstructionOp op);

class Instruction {
public:
  Instruction(BasicBlock *parent_block, InstructionOp op, Type *type);

  InstructionOp get_op() const;
  void set_op(InstructionOp new_op);
  b32 op_is_arithmetic() const;
  b32 op_is_comparison() const;
  b32 op_is_logical() const;

  std::vector<Value *> &get_operands();
  Value *get_operand(u32 idx);
  void add_operand(Value *operand);
  void replace_operand(Value *oldv, Value *newv);
  b32 has_const_operands() const;

  Function *get_function() const;
  void set_function(Function *function);

  BasicBlock *get_parent_block();

  const std::vector<Label> &get_labels() const;
  void add_label(Label label);

  void set_dest(Value *new_dest);
  Value *get_dest() const;
  bool has_dest() const;

  std::vector<Instruction *> get_users();
  void add_user(Instruction *user);

  Type *get_type() const;

  b32 alive();
  void kill();

  void debug_print() const;

private:
  InstructionOp op;
  Type *type;

  Value *dest;
  Function *function;
  std::vector<Label> labels;
  std::vector<Value *> operands;

  std::vector<Instruction *> users;
  BasicBlock *parent_block;
  b32 has_side_effects;
  b32 dead;
};

class BasicBlock {
private:
  struct UseDefInfo {
    std::unordered_set<Value *> uses;
    std::unordered_set<Value *> defs;
  };

public:
  BasicBlock(Function *parent, u32 number, std::string_view name = "");

  const std::string_view &get_name();

  Label label() const;

  Function *get_parent();

  Instruction *prepend_instr(InstructionOp op, Type *type);
  Instruction *prepend_instr(std::unique_ptr<Instruction> instr);

  Instruction *push_instr(InstructionOp op, Type *type);
  std::vector<std::unique_ptr<Instruction>> &get_instructions();

  void add_pred(BasicBlock *bb);
  const std::vector<BasicBlock *> &get_preds();
  void add_succ(BasicBlock *bb);
  const std::vector<BasicBlock *> &get_succs();

  void debug_print() const;

  void remove_dead_instrs();

  UseDefInfo get_uses_and_defs();

private:
  Function *parent;
  std::string_view name;
  std::vector<std::unique_ptr<Instruction>> instrs;
  std::vector<BasicBlock *> predecessors;
  std::vector<BasicBlock *> successors;
  u32 number;
};

class Function {
public:
  struct Arg {
    Type *type;
    std::string_view name;
  };

  Function();
  Function(std::string_view name, Type *ret_type);

  const std::string_view &get_name() const;
  const std::vector<Arg> &get_args();
  void add_arg(Arg arg);

  std::vector<std::unique_ptr<BasicBlock>> &get_blocks();
  BasicBlock *get_entry();
  BasicBlock *add_block(std::string_view name);
  BasicBlock *add_block(std::unique_ptr<BasicBlock> block);
  std::unique_ptr<BasicBlock> create_block(std::string_view name);

  b32 empty();

  void set_return_type(Type *rtype);
  Type *get_return_type();

  void remove_dead_instrs();

  void debug_print() const;

private:
  std::string_view name;
  Type *ret_type;
  std::vector<Arg> args;
  std::unordered_map<std::string_view, u32> block_numbers;
  std::vector<std::unique_ptr<BasicBlock>> basic_blocks;
};

class Program {
public:
  Program(IRContext &ctx, std::string_view name);

  void push_scope();
  void pop_scope();
  void new_symbol(std::string_view name, Value *value);
  Value *lookup_symbol(std::string_view var);
  const std::unordered_map<std::string_view, Value *> &get_symbol_table();

  Function *new_function(std::string_view name, Type *ret_type);
  Function *get_function(std::string_view name);
  std::unordered_map<std::string_view, Function> &get_functions();

  void debug_print() const;

private:
  std::string_view name;
  std::unordered_map<std::string_view, Function> functions;
  std::vector<std::unordered_map<std::string_view, Value *>> scopes;
};

class IRContext {
public:
  Type *get_type(const Type &type);

  Value *new_value(std::string_view name, Type *type);
  Value *new_value(std::string_view name, Value::ConstantValue value);

  Type *convert_ast_type_to_ir_type(parse::Ast &ast,
                                    parse::Ast::NodePtr type_ptr);

private:
  std::unordered_map<Type, std::unique_ptr<Type>, Type::Hash, Type::Equal>
      type_registry;
  std::vector<std::unique_ptr<Value>> values;
};

} // namespace ir
} // namespace neo
