#include <ir/ir.h>

#include <assert.h>
#include <memory>
#include <optional>
#include <string_view>
#include <variant>

namespace neo {
namespace ir {
// Type
Type::Type(std::string_view name, u32 size, u32 align, DerivedType derived_type,
           Type *base_type)
    : name(name), size(size), alignment(align), derived_type(derived_type),
      base_type(base_type) {}

Type Type::arr_of(Type *type, u32 size) {
  return Type(
      "array(" + std::string(type->get_name().begin(), type->get_name().end()) +
          ")",
      type->get_size() * size, type->get_alignment(), Type::DerivedType::ARRAY,
      type);
}

Type Type::ptr_to(Type *type) {
  return Type(
      "ptr(" + std::string(type->get_name().begin(), type->get_name().end()) +
          ")",
      8, 8, Type::DerivedType::POINTER, type);
}

const std::string_view &Type::get_name() const { return name; }

u32 Type::get_size() const { return size; }

u32 Type::get_alignment() const { return alignment; }

b32 Type::is_primitive_type() const { return derived_type == NONE; }

Type::DerivedType Type::get_derived_type() const { return derived_type; }
Type *Type::get_base_type() const { return base_type; }

b32 Type::operator==(const Type &other) const {
  b32 base_eq =
      (!base_type && !other.base_type) ||
      ((base_type && other.base_type) && (*base_type == *other.base_type));
  return name == other.name && size == other.size &&
         derived_type == other.derived_type && base_eq;
}
b32 Type::operator!=(const Type &other) const { return !(*this == other); }

// Value
Value::Value(std::string_view name, Type *type)
    : name(name), type(type), def_instr(nullptr), constant_value(std::nullopt) {
}

Value::Value(std::string_view name, Type *type, ConstantValue value)
    : name(name), type(type), def_instr(nullptr), constant_value(value) {}

const std::string_view Value::get_name() const { return name; }

void Value::set_name(const std::string_view new_name) { name = new_name; }

Type *Value::get_type() const { return type; }

void Value::set_type(Type *new_type) { type = new_type; }

Instruction *Value::get_def_instr() const { return def_instr; }

void Value::set_def_instr(Instruction *instr) { def_instr = instr; }

void Value::add_user(Instruction *user) { users.push_back(user); }

const std::vector<Instruction *> &Value::get_users() const { return users; }

bool Value::is_const_value() const { return constant_value.has_value(); }

Value::ConstantValue Value::get_const_value() { return constant_value.value(); }

void Value::set_const_value(ConstantValue value) { constant_value = value; }

// Instruction
Instruction::Instruction(InstructionOp op, Type *type)
    : op(op), type(type), dest(nullptr), has_side_effects(false) {}

InstructionOp Instruction::get_op() const { return op; }

void Instruction::set_op(InstructionOp new_op) { op = new_op; }

const std::vector<Value *> &Instruction::get_operands() const {
  return operands;
}

void Instruction::add_label(Label label) { labels.push_back(label); }

void Instruction::add_operand(Value *operand) { operands.push_back(operand); }

b32 Instruction::has_dest() const { return dest != nullptr; };

Value *Instruction::get_dest() const { return dest; }

void Instruction::set_dest(Value *new_dest) { dest = new_dest; }

const std::vector<Instruction *> Instruction::get_users() const {
  return users;
}

Function *Instruction::get_function() const { return function; }

void Instruction::set_function(Function *fn) { function = fn; }

void Instruction::add_user(Instruction *user) { users.push_back(user); }

Type *Instruction::get_type() const { return type; }

// BasicBlock
BasicBlock::BasicBlock(Function *parent, std::string_view name)
    : parent(parent), name(name) {}

const std::string_view &BasicBlock::get_name() { return name; }

Function *BasicBlock::get_parent() { return parent; }

std::vector<std::unique_ptr<Instruction>> &BasicBlock::get_instructions() {
  return instrs;
}

Instruction *BasicBlock::push_instr(InstructionOp op, Type *type) {
  instrs.push_back(std::make_unique<Instruction>(op, type));
  return instrs.back().get();
}

void BasicBlock::add_pred(BasicBlock *bb) { predecessors.push_back(bb); }
void BasicBlock::add_succ(BasicBlock *bb) { successors.push_back(bb); }

// Function
Function::Function() {}

Function::Function(std::string_view name) : name(name) {}

const std::vector<Function::Arg> &Function::get_args() { return args; }

void Function::add_arg(Function::Arg arg) { args.push_back(arg); }

const std::vector<std::unique_ptr<BasicBlock>> &Function::get_blocks() const {
  return basic_blocks;
}

BasicBlock *Function::add_block(std::string_view name) {
  basic_blocks.push_back(std::make_unique<BasicBlock>(this, name));
  return basic_blocks.back().get();
}

BasicBlock *Function::add_block(std::unique_ptr<BasicBlock> block) {
  basic_blocks.push_back(std::move(block));
  return basic_blocks.back().get();
}

b32 Function::empty() { return basic_blocks.empty(); }

void Function::set_return_type(Type *rtype) { ret_type = rtype; }

Type *Function::get_return_type() { return ret_type; };

// Program
Program::Program(IRContext &ctx, std::string_view name) : name(name) {}

const std::unordered_map<std::string_view, Function> &Program::get_functions() {
  return functions;
}

Function *Program::get_function(std::string_view name) {
  if (functions.contains(name))
    return &functions[name];
  return nullptr;
}

Function *Program::new_function(std::string_view name) {
  functions[name] = Function(name);
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

// Context
Type *IRContext::get_type(Type type) {
  if (type_registry.contains(type.get_name()))
    return type_registry[type.get_name()].get();

  type_registry[type.get_name()] = std::make_unique<Type>(type);

  return type_registry[type.get_name()].get();
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
        std::make_unique<Value>(name, get_type(Type("bool", 4, 4)), value));
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
      return get_type(Type("bool", 4, 4));
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
