#include <iostream>
#include <ir/ir.h>

#include <assert.h>
#include <memory>
#include <optional>
#include <string_view>
#include <unordered_set>
#include <variant>

namespace std {
template <> struct hash<neo::ir::Type *> {
  size_t operator()(const neo::ir::Type *type) const {
    return neo::ir::Type::Hash()(*type);
  }
};
} // namespace std

namespace neo {
namespace ir {
// Type
Type::Type(std::string_view name, u32 size, u32 align, DerivedType derived_type,
           Type *base_type)
    : name(name), size(size), alignment(align), derived_type(derived_type),
      base_type(base_type) {}

size_t Type::Hash::operator()(const Type &type) const {
  size_t hash = std::hash<int>()(type.derived_type);

  hash ^= std::hash<std::string_view>()(type.name) + +0x9e3779b9 + (hash << 6) +
          (hash >> 2);

  hash ^=
      std::hash<size_t>()(type.size) + 0x9e3779b9 + (hash << 6) + (hash >> 2);

  hash ^= std::hash<size_t>()(type.alignment) + 0x9e3779b9 + (hash << 6) +
          (hash >> 2);

  hash ^=
      std::hash<size_t>()(type.size) + 0x9e3779b9 + (hash << 6) + (hash >> 2);

  if (type.base_type) {
    hash ^= std::hash<const Type *>()(type.base_type) + 0x9e3779b9 +
            (hash << 6) + (hash >> 2);
  }
  return hash;
}

b32 Type::Equal::operator()(const Type &lhs, const Type &rhs) const {
  return lhs == rhs;
}

Type Type::arr_of(Type *type, u32 size) {
  return Type(type->get_name(), type->get_size() * size, type->get_alignment(),
              Type::DerivedType::ARRAY, type);
}

Type Type::ptr_to(Type *type) {
  return Type(type->get_name(), 8, 8, Type::DerivedType::POINTER, type);
}

const std::string_view &Type::get_name() const { return name; }

u32 Type::get_size() const { return size; }

u32 Type::get_alignment() const { return alignment; }

b32 Type::is_primitive_type() const { return derived_type == NONE; }

Type::DerivedType Type::get_derived_type() const { return derived_type; }
Type *Type::get_base_type() const { return base_type; }

b32 Type::operator==(const Type &other) const {
  return name == other.name && size == other.size &&
         alignment == other.alignment && derived_type == other.derived_type &&
         ((base_type == nullptr && other.base_type == nullptr) ||
          ((base_type && other.base_type) && (*base_type == *other.base_type)));
}

b32 Type::operator!=(const Type &other) const { return !(*this == other); }

// Value
Value::Value(std::string_view name, Type *type)
    : name(name), type(type), def_instr(nullptr), constant_value(std::nullopt),
      dead(false) {}

Value::Value(std::string_view name, Type *type, ConstantValue value)
    : name(name), type(type), def_instr(nullptr), constant_value(value),
      dead(false) {}

const std::string_view Value::get_name() const { return name; }

void Value::set_name(const std::string_view new_name) { name = new_name; }

Type *Value::get_type() const { return type; }

void Value::set_type(Type *new_type) { type = new_type; }

Instruction *Value::get_def_instr() const { return def_instr; }

void Value::set_def_instr(Instruction *instr) { def_instr = instr; }

void Value::add_user(Instruction *user) { users.insert(user); }

void Value::delete_user(Instruction *user) { users.erase(user); }

const std::unordered_set<Instruction *> &Value::get_users() const {
  return users;
}

bool Value::is_const_value() const { return constant_value.has_value(); }

Value::ConstantValue Value::get_const_value() { return constant_value.value(); }

void Value::set_const_value(ConstantValue value) { constant_value = value; }

void Value::kill() { dead = true; }

void Value::replace_all_uses_with(Value *value) {
  if (this == value)
    return;
  for (auto user : users)
    user->replace_operand(this, value);
  users.clear();
}

b32 Value::alive() { return !dead; }

void Value::debug_print() const {
  if (is_const_value()) {
    if (auto *v = std::get_if<i32>(&constant_value.value())) {
      std::cout << *v;
    } else if (auto *v = std::get_if<b32>(&constant_value.value())) {
      std::cout << *v;
    } else if (auto *v = std::get_if<f32>(&constant_value.value())) {
      std::cout << *v;
    }
  } else {
    std::cout << name;
  }
}

// Instruction
const char *op2str(InstructionOp op) {
  switch (op) {
  // Arithmetic
  case OP_ADD:
    return "add";
  case OP_SUB:
    return "sub";
  case OP_MUL:
    return "mul";
  case OP_DIV:
    return "div";
  case OP_NEG:
    return "neg";

  // Logical
  case OP_NOT:
    return "not";
  case OP_AND:
    return "and";
  case OP_OR:
    return "or";

  // Comparison
  case OP_CEQ:
    return "ceq"; // compare equal
  case OP_CNE:
    return "cne"; // compare not equal
  case OP_CLT:
    return "clt"; // compare less than
  case OP_CGT:
    return "cgt"; // compare greater than
  case OP_CLE:
    return "cle"; // compare less than or equal
  case OP_CGE:
    return "cge"; // compare greater than or equal

  // Control
  case OP_JMP:
    return "jmp"; // unconditional jump
  case OP_BR:
    return "br"; // branch (conditional jump)
  case OP_CALL:
    return "call";
  case OP_RET:
    return "ret";
  case OP_PHI:
    return "phi";

  // Memory
  case OP_FREE:
    return "free";
  case OP_ALLOCA:
    return "alloca";
  case OP_LD:
    return "ld"; // load
  case OP_STR:
    return "str"; // store

  // Misc
  case OP_MOV:
    return "mov";

  default:
    return "unknown";
  }
};

Instruction::Instruction(BasicBlock *parent, InstructionOp op, Type *type)
    : op(op), type(type), dest(nullptr), function(nullptr),
      parent_block(parent), side_effects(false), dead(false) {}

const std::vector<BasicBlock *> &Instruction::get_blocks() const {
  return blocks;
}

void Instruction::move(Instruction *dst, Instruction *src) {
  if (src->has_dest())
    src->get_dest()->set_def_instr(dst);

  for (auto op : src->get_operands())
    op->add_user(dst);

  *dst = *src;
  src->kill();
}

BasicBlock *Instruction::get_block(i32 block_idx) { return blocks[block_idx]; }

BasicBlock *Instruction::get_parent_block() { return parent_block; }
InstructionOp Instruction::get_op() const { return op; }

void Instruction::set_op(InstructionOp new_op) { op = new_op; }

std::vector<Value *> &Instruction::get_operands() { return operands; }

Value *Instruction::get_operand(u32 idx) { return operands[idx]; }

void Instruction::add_block(BasicBlock *block) { blocks.push_back(block); }

void Instruction::add_operand(Value *operand) { operands.push_back(operand); }

void Instruction::set_dest(Value *new_dest) { dest = new_dest; }

Value *Instruction::get_dest() const { return dest; }

bool Instruction::has_dest() const { return dest != nullptr; }

std::vector<Instruction *> &Instruction::get_users() { return users; }

Function *Instruction::get_function() const { return function; }

void Instruction::set_function(Function *fn) { function = fn; }

void Instruction::add_user(Instruction *user) { users.push_back(user); }

Type *Instruction::get_type() const { return type; }

void Instruction::replace_operand(Value *oldv, Value *newv) {
  for (Value *&operand : operands) {
    if (operand == oldv) {
      operand = newv;
      newv->add_user(this);
    }
  }
}

b32 Instruction::alive() { return !dead; }

void Instruction::kill() { dead = true; }

b32 Instruction::has_side_effects() const { return side_effects; }

b32 Instruction::has_const_operands() const {
  if (!op_is_arithmetic() && !op_is_logical())
    return false;

  for (auto op : operands) {
    if (!op->is_const_value())
      return false;
  }
  return true;
}

b32 Instruction::op_is_arithmetic() const {
  return op == OP_ADD || op == OP_SUB || op == OP_MUL || op == OP_DIV ||
         op == OP_NEG;
}

b32 Instruction::op_is_comparison() const {
  return op == OP_CEQ || op == OP_CNE || op == OP_CLT || op == OP_CGT ||
         op == OP_CLE || op == OP_CGE;
}

b32 Instruction::op_is_logical() const {
  return op == OP_NOT || op == OP_AND || op == OP_OR;
}

b32 Instruction::op_is_control() const { return op == OP_BR || op == OP_JMP; }

void Instruction::debug_print() const {
  std::cout << op2str(op) << " ";

  if (dest)
    std::cout << dest->get_name() << ", ";

  if (function != nullptr)
    std::cout << function->get_name() << ", ";

  for (auto &operand : operands) {
    operand->debug_print();
    std::cout << ", ";
  }

  for (auto &block : blocks) {
    block->label().debug_print();
    std::cout << ", ";
  }

  std::cout << std::endl;
}

// Label
void Label::debug_print() const {
  std::cout << fn_name << "_" << name << "_" << number;
}

// BasicBlock
BasicBlock::BasicBlock(Function *parent, u32 number, std::string_view name)
    : parent(parent), name(name), number(number) {}

const std::string_view &BasicBlock::get_name() { return name; }

Function *BasicBlock::get_parent() { return parent; }

Label BasicBlock::label() const {
  return Label{.fn_name = parent->get_name(), .name = name, .number = number};
}

std::vector<std::unique_ptr<Instruction>> &BasicBlock::get_instructions() {
  return instrs;
}

Instruction *BasicBlock::push_instr(InstructionOp op, Type *type) {
  instrs.push_back(std::make_unique<Instruction>(this, op, type));
  return instrs.back().get();
}

Instruction *BasicBlock::push_instr_before_end(InstructionOp op, Type *type) {
  auto end = std::move(instrs.back());
  instrs.pop_back();

  instrs.push_back(std::make_unique<Instruction>(this, op, type));
  auto res = instrs.back().get();

  instrs.push_back(std::move(end));

  return res;
}

Instruction *BasicBlock::prepend_instr(InstructionOp op, Type *type) {
  instrs.insert(instrs.begin(), std::make_unique<Instruction>(this, op, type));
  return instrs.front().get();
}

Instruction *BasicBlock::prepend_instr(std::unique_ptr<Instruction> instr) {
  instrs.insert(instrs.begin(), std::move(instr));
  return instrs.front().get();
}

const std::vector<BasicBlock *> &BasicBlock::get_preds() {
  return predecessors;
}
const std::vector<BasicBlock *> &BasicBlock::get_succs() { return successors; }

void BasicBlock::add_pred(BasicBlock *bb) { predecessors.push_back(bb); }

void BasicBlock::add_succ(BasicBlock *bb) { successors.push_back(bb); }

void BasicBlock::debug_print() const {
  std::cout << std::endl;
  label().debug_print();
  std::cout << ":" << std::endl;
  for (auto &instr : instrs)
    instr.get()->debug_print();
}

void BasicBlock::remove_dead_instrs() {
  for (auto it = instrs.begin(); it != instrs.end();) {
    auto instr = it->get();
    if (!instr->alive()) {
      for (auto op : instr->get_operands())
        op->delete_user(instr);
      it = instrs.erase(it);
    } else {
      it++;
    }
  }
}

BasicBlock::UseDefInfo BasicBlock::get_uses_and_defs() {
  UseDefInfo info;

  // operands that are used but not yetdefined in ths block
  // go to uses
  for (auto &instr : instrs) {
    for (auto op : instr->get_operands()) {
      if (!info.defs.contains(op))
        info.uses.insert(op);
    }
  }

  // results of instructions are definitions of new values
  for (auto &instr : instrs) {
    if (instr->has_dest()) {
      info.defs.insert(instr->get_dest());
    }
  }

  return info;
}

// Function
Function::Function() : name("") {}

Function::Function(std::string_view name, Type *ret_type)
    : name(name), ret_type(ret_type) {}

const std::string_view &Function::get_name() const { return name; }

void Function::set_formal_parameter_value(i32 param, Value *value) {
  params[param].value = value;
}

const std::vector<Function::Parameter> &Function::get_params() {
  return params;
}

void Function::add_param(std::string_view name, Type *type) {
  params.push_back({
      .type = type,
      .name = name,
  });
}

BasicBlock *Function::get_entry() { return basic_blocks.at(0).get(); }
std::vector<std::unique_ptr<BasicBlock>> &Function::get_blocks() {
  return basic_blocks;
}

BasicBlock *Function::add_block(std::string_view name) {
  if (!block_numbers.contains(name))
    block_numbers[name] = 0;
  basic_blocks.push_back(
      std::make_unique<BasicBlock>(this, block_numbers[name]++, name));
  return basic_blocks.back().get();
}

BasicBlock *Function::add_block(std::unique_ptr<BasicBlock> block) {
  basic_blocks.push_back(std::move(block));
  return basic_blocks.back().get();
}

std::unique_ptr<BasicBlock> Function::create_block(std::string_view name) {
  if (!block_numbers.contains(name))
    block_numbers[name] = 0;
  return std::make_unique<BasicBlock>(this, block_numbers[name]++, name);
}

b32 Function::empty() { return basic_blocks.empty(); }

void Function::set_return_type(Type *rtype) { ret_type = rtype; }

Type *Function::get_return_type() { return ret_type; };

void Function::debug_print() const {
  std::cout << std::endl;
  std::cout << "Function: " << name << std::endl;
  for (auto &bb : basic_blocks)
    bb.get()->debug_print();
}

void Function::remove_dead_instrs() {
  for (auto &bb : basic_blocks)
    bb->remove_dead_instrs();
}

// Program
Program::Program(IRContext &ctx, std::string_view name) : name(name) {
  // global scope
  push_scope();
}

std::unordered_map<std::string_view, Function> &Program::get_functions() {
  return functions;
}

Function *Program::get_function(std::string_view name) {
  if (functions.contains(name))
    return &functions[name];
  return nullptr;
}

Function *Program::new_function(std::string_view name, Type *ret_type) {
  functions[name] = Function(name, ret_type);
  return &functions[name];
}

void Program::push_scope() { scopes.push_back({}); }

void Program::pop_scope() {
  if (!scopes.empty())
    scopes.pop_back();
}

void Program::new_symbol(std::string_view name, Value *value) {
  scopes.back()[name] = value;
}

Value *Program::lookup_symbol(std::string_view var) {
  for (auto it = scopes.rbegin(); it != scopes.rend(); it++) {
    auto found = it->find(var);
    if (found != it->end()) {
      return found->second;
    }
  }
  return nullptr;
}

const std::unordered_map<std::string_view, Value *> &
Program::get_symbol_table() {
  return scopes.back();
}

void Program::debug_print() const {
  std::cout << "Program: " << name << std::endl;
  for (auto &[_, func] : functions)
    func.debug_print();
}

// Context
Type *IRContext::get_type(const Type &type) {
  auto it = type_registry.find(type);
  if (it != type_registry.end())
    return it->second.get();

  type_registry[type] = std::make_unique<Type>(type);
  return type_registry[type].get();
}

Value *IRContext::new_value(std::string_view name, Type *type) {
  values.push_back(std::make_unique<Value>(name, type));
  return values.back().get();
}

Value *IRContext::new_value(std::string_view name, Value::ConstantValue value) {
  if (auto *v = std::get_if<i32>(&value)) {
    values.push_back(
        std::make_unique<Value>(name, get_type(Type("int", 4, 4)), value));
  } else if (auto *v = std::get_if<b32>(&value)) {
    values.push_back(
        std::make_unique<Value>(name, get_type(Type("bool", 1, 1)), value));
  } else if (auto *v = std::get_if<f32>(&value)) {
    values.push_back(
        std::make_unique<Value>(name, get_type(Type("float", 4, 4)), value));
  }
  return values.back().get();
}

Type *IRContext::convert_ast_type_to_ir_type(parse::Ast &ast,
                                             parse::Ast::NodePtr type_ptr) {
  using AstKind = parse::Ast::Type::Kind;
  using AstPrim = parse::Ast::Type::PrimitiveType;

  auto ast_type = ast.get_type(type_ptr);
  switch (ast_type.kind) {
  case AstKind::PRIMITIVE: {
    switch (ast_type.primitive_type.value()) {
    case AstPrim::INT:
      return get_type(Type("int", 4, 4));
    case AstPrim::BOOL:
      return get_type(Type("bool", 1, 1));
    case AstPrim::FLOAT:
      return get_type(Type("float", 4, 4));
    default:
      return get_type(Type("void", 0, 0));
    }
  } break;
  case AstKind::POINTER: {
    auto el_type = convert_ast_type_to_ir_type(ast, ast_type.el_type);
    return get_type(Type::ptr_to(el_type));
  } break;
  case AstKind::ARRAY: {
    auto el_type = convert_ast_type_to_ir_type(ast, ast_type.el_type);
    return get_type(Type::arr_of(el_type, ast_type.array_size));
  } break;
  default:
    return get_type(Type("void", 0, 0));
  }
}

} // namespace ir
} // namespace neo
