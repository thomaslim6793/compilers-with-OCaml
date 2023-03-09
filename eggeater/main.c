#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint64_t SNAKEVAL;
const uint64_t BOOL_TAG = 0x0000000000000007;
const uint64_t BOOL_PAIR_MASK = 0x0000000000000007;
const uint64_t PAIR_TAG = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE = 0xFFFFFFFFFFFFFFFF;
const SNAKEVAL BOOL_FALSE = 0x7FFFFFFFFFFFFFFF;
const int64_t MAX_SNAKE_INT = 4611686018427387903;
const int64_t MIN_SNAKE_INT = -4611686018427387904;
uint64_t* STACK_BOTTOM;


extern SNAKEVAL our_code_starts_here(uint64_t* HEAP, int size) asm("our_code_starts_here");
extern void error(uint64_t errCode, SNAKEVAL val) asm("error");
extern SNAKEVAL print(SNAKEVAL val) asm("print");
extern SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) asm("equal");
extern SNAKEVAL input() asm("input");
extern SNAKEVAL printStack(SNAKEVAL val, uint64_t num_args) asm("printstack");
extern uint64_t* STACK_BOTTOM asm("STACK_BOTTOM");

uint64_t* HEAP;



int equalHelp(uint64_t val1, uint64_t val2) {
  if (val1 == val2) {
    return 1;
  }
  if ((val1 & BOOL_PAIR_MASK) == PAIR_TAG && (val2 & BOOL_PAIR_MASK) == PAIR_TAG) {
    uint64_t* untagged1 = (uint64_t*)(val1 - PAIR_TAG);
    uint64_t* untagged2 = (uint64_t*)(val2 - PAIR_TAG);
    if (untagged1 == NULL || untagged2 == NULL) {
      return 0;
    }
    int64_t n1 = ((int64_t)*untagged1) >> 1;
    int64_t n2 = ((int64_t)*untagged2) >> 1;
    if (n1 != n2) {
      return 0;
    }
    int i;
    int all[n1];
    for (i = 1; i <= n1; i++) {
      all[i-1] = equalHelp(*(untagged2 + i), *(untagged1 + i));
    }
    for (i = 0; i < n1; i++) {
      if (all[i] == 0) {
        return 0;
      }
    }
    return 1;
  }
  return 0;
}

SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) {
  if (equalHelp((uint64_t)val1, (uint64_t)val2)) {
    return BOOL_TRUE;
  } else {
    return BOOL_FALSE;
  }
}

SNAKEVAL input() {
  char line[60];
  char *end;
  size_t len = 0;

  scanf("%58s", line);

  int64_t num = strtol(line, &end, 10);
  if (*end != '\0') {
    if (strcmp(end, "false") == 0) {
      return BOOL_FALSE;
    }
    else if (strcmp(end, "true") == 0) {
      return BOOL_TRUE;
    } else {
      fprintf(stderr, "Invalid input to program %c\n", *end);
      exit(1);
    }
  }
  if (num > MAX_SNAKE_INT) {
    // fprintf(stderr, "Number %ld is not supported is this language, over max", num);
    error(5, (SNAKEVAL)(num << 1));
  } else if (num < MIN_SNAKE_INT) {
    // fprintf(stderr, "Number %ld is not supported is this language, under min", num);
    error(5, (SNAKEVAL)(num << 1));
  }
  // printf("%ld", num);
  return (SNAKEVAL)(num << 1);
}

void printHelp(FILE *out, SNAKEVAL val) {
  if ((val & PAIR_TAG) == 0) {
    fprintf(out, "%lld", ((int64_t)(val)) >> 1);
  } else if (val == BOOL_TRUE) {
    fprintf(out, "true");
  } else if (val == BOOL_FALSE) {
    fprintf(out, "false");
  } else if ((val & BOOL_PAIR_MASK) == PAIR_TAG) {
    uint64_t* untagged = (uint64_t*)(val - PAIR_TAG);
    if (untagged == NULL) {
      fprintf(out, "nil");
      return;
    }
    int64_t n = ((int64_t)*untagged) >> 1;
    int i;
    fprintf(out, "(");
    for (i = 1; i <= n; i++) {
      printHelp(out, *(untagged + i));
      if (i != n) {
        fprintf(out, ", ");
      }
    }
    if (n == 1) {
        fprintf(out, ",");
    }
    fprintf(out, ")");
  } else {
    fprintf(out, "Unknown value: %018llX", val);
  }
}

SNAKEVAL print(SNAKEVAL val) {
  printHelp(stdout, val);
  printf("\n");
  fflush(stdout);
  return val;
}

uint64_t set_stack_bottom(uint64_t stk, uint64_t val) {
  STACK_BOTTOM = (uint64_t*)stk;
  return val;
}

// printstack must be implemented as EPrim1 and not as an EApp 
// because the function call is calling back into the runtime. EPrim1 and EApp have seperate
// environments in our implementation. Our runtime environment provides the tools to print to the console.
// Otherwise, we would have to build out the tools in assembly by using syscalls from scratch.
// Also, it is worth noting that the runtime (in our case) has a different calling convention.
uint64_t printStack(SNAKEVAL val, uint64_t num_args) {
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
  printHelp(stdout, val);
  printf("Stack Bottom: %#018lx\n", STACK_BOTTOM);
  printf("Num args: %lld\n", ((int64_t)(num_args)));

  while (*our_rbp <= STACK_BOTTOM) {
    void* local;
    void* new_rbp = (void*)*our_rbp;
    for (local = ((void*) our_rbp) + 0x10; local < new_rbp; local = local + 0x8) {
      printf("    %p: %#018lx  ==> ", local, *(uint64_t*)local);
      printHelp(stdout, *(uint64_t*)local);
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

void error(uint64_t errCode, SNAKEVAL val) {
  if (errCode == 2) {
    fprintf(stderr, "arithmetic expected a number, but got %#018lx\n", val);
  } else if (errCode == 1) {
    fprintf(stderr, "comparison expected a number, but got %#018lx\n", val);
  } else if (errCode == 4) {
    fprintf(stderr, "if expected a boolean, but got %#018lx\n", val);
  } else if (errCode == 3) {
    fprintf(stderr, "logic expected a boolean, but got %#018lx\n", val);
  } else if (errCode == 5) {
    fprintf(stderr, "overflowed with value %#018lx\n", val);
  } else if (errCode == 6) {
    fprintf(stderr, "expected tuple: %#018lx\n", val);
  } else if (errCode == 7) {
    fprintf(stderr, "index too small: %#018lx\n", val);
  } else if (errCode == 8) {
    fprintf(stderr, "index too large: %#018lx\n", val);
  } else if (errCode == 9) {
    fprintf(stderr, "access component of nil: %#018lx\n", val);
  } else if (errCode == 10) {
    fprintf(stderr, "index is not a number: %#018lx\n", val);
  } else {
    fprintf(stderr, "Unknown error code: %d", errCode);
  }
  exit(errCode);
}

// main should remain unchanged
// You can pass in a numeric argument to your program when you run it,
// to specify the size of the available heap.  You may find this useful
// for debugging...
int main(int argc, char** argv) {
  int size = 100000;
  if (argc > 1) { size = atoi(argv[1]); }
  if (size < 0 || size > 1000000) { size = 0; }
  HEAP = calloc(size, sizeof (int));

  SNAKEVAL result = our_code_starts_here(HEAP, size);
  print(result);
  free(HEAP);
  return 0;
}
