#pragma once
#include <assert.h>
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

template <typename T> class Span {
public:
  using iterator = T *;
  using const_iterator = const T *;

  Span(T *data, u32 size) : data_(data), size_(size) {}
  u32 size() const { return size_; }
  bool empty() const { return size_ == 0; }

  T &operator[](u32 index) {
    assert(index < size_);
    return data_[index];
  }

  const T &operator[](u32 index) const {
    assert(index < size_);
    return data_[index];
  }

  T &back() const { return data_[size_ - 1]; }

  iterator begin() { return data_; }
  iterator end() { return data_ + size_; }

  const_iterator begin() const { return data_; }
  const_iterator end() const { return data_ + size_; }

  const_iterator cbegin() const { return data_; }
  const_iterator cend() const { return data_ + size_; }

private:
  T *data_;
  u32 size_;
};
