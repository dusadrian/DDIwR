#define R_NO_REMAP
#include <R.h>
#include <ctype.h>

// Scalar operators -------------------------------------------------------

// IEEE 754 defines binary64 as
// * 1  bit : sign
// * 11 bits: exponent
// * 52 bits: significand
//
// R stores the value "1954" in the last 32 bits: this payload marks
// the value as a NA, not a regular NaN.
//
// (Note that this discussion like most discussion of FP on the web, assumes
// a big-endian architecture - in little endian the sign bit is the last
// bit)

typedef union {
  double value;           // 8 bytes
  char byte[8];           // 8 * 1 bytes
} ieee_double;


#ifdef WORDS_BIGENDIAN
// First two bytes are sign & expoonent
// Last four bytes are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif

double make_tagged_na(char x) {
  ieee_double y;

  y.value = NA_REAL;
  y.byte[TAG_BYTE] = x;

  return y.value;
}

char tagged_na_value(double x) {
  ieee_double y;
  y.value = x;

  return y.byte[TAG_BYTE];
}
