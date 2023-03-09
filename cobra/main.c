#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint64_t SNAKEVAL;
const uint64_t BOOL_TAG = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE = 0xFFFFFFFFFFFFFFFF;
const SNAKEVAL BOOL_FALSE = 0x7FFFFFFFFFFFFFFF;

extern uint64_t our_code_starts_here() asm("our_code_starts_here");
extern uint64_t print(uint64_t val) asm("print");
extern uint64_t printstack(uint64_t rsp, uint64_t val) asm("printstack");
extern void error(int errCode, uint64_t val) asm("error");

uint64_t print(uint64_t val) {
  if ((val & BOOL_TAG) == 0) {
    printf("%lld\n", ((int64_t)(val)) >> 1);
  } else if (val == BOOL_TRUE) {
    printf("true\n");
  } else if (val == BOOL_FALSE) {
    printf("false\n");
  } else {
    fprintf(stderr, "Unknown value: %018llX\n", val);
  }
  return val;
}

uint64_t printstack(uint64_t rsp, uint64_t val) {
  uint64_t our_rsp;
  asm("movq %%rsp, %0;"
       :"=r"(our_rsp)
  );
  printf("our rsp = %#018lx, input rsp = %#018lx, delta = %ld\n", our_rsp, rsp, rsp-our_rsp);
  return val;
}

void error(int errCode, uint64_t val) {
  if (errCode == 2) {
    // fprintf(stderr, "arithmetic expected a number, but got %#018lX\n", val);
    fprintf(stderr, "arithmetic expected a number, but got %#018llX\n", val);
  } else if (errCode == 1) {
    fprintf(stderr, "comparison expected a number, but got %#018llX\n", val);
  } else if (errCode == 4) {
    fprintf(stderr, "if expected a boolean, but got %#018llX\n", val);
  } else if (errCode == 3) {
    fprintf(stderr, "logic expected a boolean, but got %#018llX\n", val);
  } else if (errCode == 5) {
    fprintf(stderr, "overflowed with value %#018llX\n", val);
  } else {
    fprintf(stderr, "Unknown error code: %d", errCode);
  }
  exit(errCode);
}

int main(int argc, char** argv) {
  uint64_t result = our_code_starts_here();
  print(result);
  return 0;
}
