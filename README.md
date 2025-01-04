# neo :seedling:: A compiler for learning compilers

## My Problem
I really wanted to get into compiler engineering and studied a bunch of literature and course work.
However, when I wanted to begin gaining experience working with compilers, I found it very hard
to find open-source projects that were easy to hack on. There are a plethora of amazing and fully-featured
compiler projects out there. The main issue I found was finding an easy way to contribute to them. Some have
great documentation and communities, but, as with most open-source projects, the "good first issue" tags are not very well maintained.

All that being said, I figured I'd just make my own compiler for my own language. While it won't be nearly as good or fully-featured as those
other compilers, it's essentially for me to a) get more experience, b) learn more about compilers, and c) have an artifact of
my work.

## neo
`neo` is a very simple, statically-typed, imperative language in the vein of C, Hare, Zig, etc. The language is designed to be 
simple and explicit. Rather than thinking of the design of the language first, and then designing a compiler around it, I have
opted for the opposite approach. Most, if not all, of `neo`'s design is based around the design of the compiler itself. Therefore,
a lot of the design choices in the language were created for ease of compilation. Here is an overview of the language:

### Constants
```
const a: int = 10;
const b = 10; # omit type
```

### Variables
```
var a = 10;
a += 5;
```

### Basic Operations
```
const a = 10;
const b = 20;
const add = a + b;
const sub = a - b;
const mul = a * b;
const div = a / b;
const eq = a == b;
const neq = a != b;
```

### Arrays
```
const a: [4]int = [4]int{1, 2, 3, 4};
const b = a[2]; # b = 3
const c = [4]int{1, 2, 3, 4};
const d = [4]int{}; # allocates array for 4 ints
const e = []int{1, 2, 3, 4}; # infers array size is 4
const f = [2][2]int{ []int{0,1}, []int{2,3} };
```

### Loops
```
# for-loop
for (var i = 0; i < 10; i += 1) {
  ...
}

# while-loop
const i = 0;
while (i < 10) {
  i += 1;
}
```

### Conditional
```
const a = 1;
const b = 5;

if (a > b) {
  ...
} 
else if (b > a) {
  ...
}
else {
  ...
}
```

### Functions
```
fn add(a: int, b: int) int {
  return a + b;
}
```

## neoIR:
`neoIR` is the intermediate representation used by `neo`. It is similar to, but much simpler than LLVM. It was actually designed 
to be closer to [Bril](), developed at Cornell. The main design philosophy behind `neoIR` was to keep it very, very 
lean and only support features and operations used by `neo`. This is in contrast to LLVM which must support a wide variety of types,
use cases, etc. The most prominent types in `neoIR` are `Value`s and `Instruction`s. `Value`s represent data within the 
IR (e.g. constants, results, etc). They keep track of their defining `Instruction` and all of the users (a list of `Instructions`).
All values in `neoIR` are typed. `Instruction`s represent operations that manipulate `Value`s. Unlike LLVM, `Instruction`s do not inherit from `Value`s, i.e. they cannot directly be used 
interchangeably. Instead, `Instruction`s have an optional destination `Value` that would hold the result of the instruction executing.
Similar to Bril, `Instruction`s also keep track of their opcode, any functions they may call, any labels they may reference, and any operands (`Value`s) they may use. 

Beyond `Value`s and `Instruction`s, `neoIR` also has classes for `Types`, `Function`s, `BasicBlock`s, `Label`s, and `Program`s.
The main relationship between the types is:
- `Programs` are composed of `Function`s
- `Function`s are composed of `BasicBlock`s
- `BasicBlock`s branch to `Label`s

The `neo` type system is implemented via the `neoIR` `Type` class. The main features of the type class center around creating named, sized, and aligned
types that can be registered and reused on multiple `Value`s. To support pointer and array types, a `Type` can also have a `DerivedType` of `POINTER` or `ARRAY`.
`neo`'s type system is very primitive and currently does not support type inference as seen in the examples (but this is something I'd like to implement).

To aid in lowering `neo` programs into `neoIR`, there is the `IRBuilder` and `IRGenerator` classes. The `IRGenerator` takes in the AST produced from
the `neo` frontend, and uses the `IRBuilder` to emit `neoIR` instructions. Post-generation, the `IRGenerator` returns to the user a `neoIR`
`Program`. Between the generation function being invoked and the result being returned to the user, the `IRGenerator` will also run any optimizations
requested by the user at invocation (WIP).

## Building and Testing neo
Building `neo` requires [meson](https://mesonbuild.com/SimpleStart.html) and [clang](https://clang.llvm.org/). The `neo` project also depends on the
[GoogleTest](https://google.github.io/googletest/) library for unit testing (this will be brought in by meson during build).
Once you have the required dependencies, you can run the following commands:
```
# compile project
cd bld/
meson compile # executable can be found at bld/neo

# run compiler
# from top-level dir...
./bld/neo input.neo

# test project
cd bld/
meson test # test output can be found in bld/meson-logs/testlog.json
```
