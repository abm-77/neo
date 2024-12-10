# Neo: A compiler for learning compilers

## My Problem
I really wanted to get into compiler engineering and studied a bunch of literature and course work.
However, when I wanted to begin gaining experience working with compilers, I found it very hard
to find open-source projects that were easy to hack on. There are a plethora of amazing and fully-featured
compiler projects out there. The main issue I found was finding an easy way to contribute to them. Some have
great documentation and communities, but it is still undeniably difficult to just jump into someone else's projects.
And, as with most open-source projects, the "good first issue" tags are not very well maintained. All that being said, 
I figured I'd just make my own compiler for my own language. While it won't be nearly as good or fully-featured as those
other compilers, it's essentially for me to a) get more experience, b) learn more about compilers, and c) have an artifact of
my work.

## Neo
Neo is a very simple, statically-typed, imperative language in the vein of C, Hare, Zig, etc. The language is designed to be 
simple and explicit. Rather than thinking of the design of the language first, and then designing a compiler around it, I have
opted for the opposite approach. Most, if not all, of Neo's design is based around the design of the compiler itself. Therefore,
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
```

### Arrays
```
const a: int[4] = [1, 2, 3, 4];
const b: int = a[2]; # b = 3
```

