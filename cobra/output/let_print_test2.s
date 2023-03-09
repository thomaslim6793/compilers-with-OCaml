section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 48
  mov RAX, 20
  mov [RBP-8], RAX
  mov RAX, 10
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  add RAX, 2
  jo err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  mov RDI, RAX
  call print
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, QWORD 0x1
  jnz err_arith_not_num
  add RAX, 2
  jo err_overflow
  mov [RBP-40], RAX
  mov RAX, [RBP-40]
  mov RDI, RAX
  call print
  mov [RBP-48], RAX
  mov RAX, [RBP-48]
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
