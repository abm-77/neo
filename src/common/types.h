#pragma once
#include <stdint.h>

using b32 = bool;

using f32 = float;
using f64 = double;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

struct SourceLocation {
  const char *source_name;
  u32 byte_offset;
  u32 line;
  u32 col;
};

struct SourceSpan {
  SourceLocation start;
  SourceLocation end;
};
