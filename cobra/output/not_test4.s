section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 32
  mov RAX, 2
  mov [RBP-8], RAX
  mov RAX, 4
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov RAX, [RBP-8]
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov R11, [RBP-16]
  cmp RAX, R11
  mov RAX, QWORD 0xffffffffffffffff
  jle less_eq#9
  mov RAX, QWORD 0x7fffffffffffffff
less_eq#9:
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  test RAX, QWORD 0x1
  jz err_logic_not_bool
  mov R11, QWORD 0x8000000000000000
  xor RAX, R11
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  mov RSP, RBP
  pop RBP
  ret
  ;; Start of error functions
err_arith_not_num:
  mov RDI, 2
  mov RSI, RAX
  call error
err_comp_not_num:
  mov RDI, 1
  mov RSI, RAX
  call error
err_if_not_bool:
  mov RDI, 4
  mov RSI, RAX
  call error
err_logic_not_bool:
  mov RDI, 3
  mov RSI, RAX
  call error
err_overflow:
  mov RDI, 5
  mov RSI, RAX
  call error