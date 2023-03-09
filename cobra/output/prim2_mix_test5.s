section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 32
  mov RAX, 8
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, 12
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, 8
  sub RAX, R11
  jo err_overflow
  mov [RBP-8], RAX
  mov RAX, -6
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, 4
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, -6
  sub RAX, R11
  jo err_overflow
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-8]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  sar RAX, 1
  mov R11, [RBP-16]
  imul RAX, R11
  jo err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
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
