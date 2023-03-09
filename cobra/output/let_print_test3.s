section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 144
  mov RAX, 20
  mov [RBP-8], RAX
  mov RAX, 10
  mov [RBP-16], RAX
  mov RAX, 2
  mov [RBP-24], RAX
  mov RAX, 22
  mov [RBP-32], RAX
  mov RAX, 14
  mov [RBP-40], RAX
  mov RAX, 4
  mov [RBP-48], RAX
  mov RAX, 18
  mov [RBP-56], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-8]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-16]
  add RAX, R11
  jo err_overflow
  mov [RBP-64], RAX
  mov RAX, [RBP-24]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-64]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-24]
  add RAX, R11
  jo err_overflow
  mov [RBP-72], RAX
  mov RAX, [RBP-32]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-72]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-32]
  add RAX, R11
  jo err_overflow
  mov [RBP-80], RAX
  mov RAX, [RBP-40]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-80]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-40]
  add RAX, R11
  jo err_overflow
  mov [RBP-88], RAX
  mov RAX, [RBP-48]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-88]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-48]
  add RAX, R11
  jo err_overflow
  mov [RBP-96], RAX
  mov RAX, [RBP-56]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-96]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-56]
  add RAX, R11
  jo err_overflow
  mov [RBP-104], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  add RAX, 2
  jo err_overflow
  mov [RBP-112], RAX
  mov RAX, [RBP-112]
  mov RDI, RAX
  call print
  mov [RBP-120], RAX
  mov RAX, [RBP-120]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  add RAX, 2
  jo err_overflow
  mov [RBP-128], RAX
  mov RAX, [RBP-128]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-104]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  mov R11, [RBP-128]
  add RAX, R11
  jo err_overflow
  mov [RBP-136], RAX
  mov RAX, [RBP-136]
  mov RDI, RAX
  call print
  mov [RBP-144], RAX
  mov RAX, [RBP-144]
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
