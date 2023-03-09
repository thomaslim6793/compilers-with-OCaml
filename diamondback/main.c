#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint64_t SNAKEVAL;
const uint64_t BOOL_TAG = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE = 0xFFFFFFFFFFFFFFFF;
const SNAKEVAL BOOL_FALSE = 0x7FFFFFFFFFFFFFFF;
uint64_t* STACK_BOTTOM;

extern uint64_t our_code_starts_here() asm("our_code_starts_here");
extern uint64_t print(uint64_t val) asm("print");
extern uint64_t printstack(uint64_t val, uint64_t num_args) asm("printstack");
extern void error(int errCode, uint64_t val) asm("error");
extern uint64_t* STACK_BOTTOM asm("STACK_BOTTOM");
extern uint64_t set_stack_bottom(uint64_t stk, uint64_t val) asm("set_stack_bottom");

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

uint64_t set_stack_bottom(uint64_t stk, uint64_t val) {
  STACK_BOTTOM = (uint64_t*)stk;
  return val;
}

uint64_t print_helper(uint64_t val) {
  if ((val & BOOL_TAG) == 0) {
    printf("%lld\n", ((int64_t)(val)) >> 1);
  } else if (val == BOOL_TRUE) {
    printf("true\n");
  } else if (val == BOOL_FALSE) {
    printf("false\n");
  } else {
    printf("Unknown value: %018llX\n", val);
  }
  return val;
}

// printstack must be implemented as EPrim1 and not as an EApp 
// because the function call is calling back into the runtime. EPrim1 and EApp have seperate
// environments in our implementation. Our runtime environment provides the tools to print to the console.
// Otherwise, we would have to build out the tools in assembly by using syscalls from scratch.
// Also, it is worth noting that the runtime (in our case) has a different calling convention.
uint64_t printstack(uint64_t val, uint64_t num_args) {
  void* our_rsp;
  uint64_t* our_rbp;
  asm("movq %%rsp, %0;"
       :"=r"(our_rsp)
  );
  asm("movq %%rbp, %0;"
       :"=r"(our_rbp)
  );
  printf("RSP: %p: %#018lx\n", our_rsp, *(uint64_t*)our_rsp);
  printf("RBP: %p: %#018lx\n", our_rbp, *our_rbp);
  printf("Requested return val: %#018lx   ==> ", val);
  print_helper(val);
  printf("Stack Bottom: %#018lx\n", STACK_BOTTOM);
  printf("Num args: %lld\n", ((int64_t)(num_args)));

  while (*our_rbp <= STACK_BOTTOM) {
    void* local;
    void* new_rbp = (void*)*our_rbp;
    for (local = ((void*) our_rbp) + 0x10; local < new_rbp; local = local + 0x8) {
      printf("    %p: %#018lx  ==> ", local, *(uint64_t*)local);
      print_helper(*(uint64_t*)local);
    }
    if (*our_rbp == STACK_BOTTOM) {
      printf("BOT ");
    } else {
      printf("RBP ");
    }
    printf("%p: %#018lx  ==> ", local, *(uint64_t*)local);
    printf("old RBP\n");
    printf("    %p: %#018lx  ==> ", local + 0x8, *(uint64_t*)(local + 0x8));
    printf("saved ret\n");
    our_rbp = (uint64_t*)new_rbp;
  }
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

// main should remain unchanged
int main(int argc, char** argv) {
  SNAKEVAL result = our_code_starts_here();
  print(result);
  return 0;
}
