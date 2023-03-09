section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
test:
  push RBP
  mov RBP, RSP
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
test_body:
  mov RAX, [RBP+64]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+72]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+64]
  add RAX, R11
  jo err_overflow
  mov [RBP-8], RAX
  mov RAX, [RBP+56]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-8]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+56]
  add RAX, R11
  jo err_overflow
  mov [RBP-16], RAX
  mov RAX, [RBP+48]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+48]
  add RAX, R11
  jo err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP+40]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+40]
  add RAX, R11
  jo err_overflow
  mov [RBP-32], RAX
  mov RAX, [RBP+32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+32]
  add RAX, R11
  jo err_overflow
  mov [RBP-40], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-40]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+24]
  add RAX, R11
  jo err_overflow
  mov [RBP-48], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-48]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+16]
  add RAX, R11
  jo err_overflow
  mov RSP, RBP
  pop RBP
  ret
our_code_starts_here:
  push RBP
  mov RBP, RSP
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  mov RAX, 2
  mov [RBP-8], RAX
  mov RAX, 4
  mov [RBP-16], RAX
  mov RAX, 6
  mov [RBP-24], RAX
  mov RAX, 8
  mov [RBP-32], RAX
  mov RAX, 10
  mov [RBP-40], RAX
  mov RAX, 12
  mov [RBP-48], RAX
  mov RAX, 14
  mov [RBP-56], RAX
  mov RAX, 16
  mov [RBP-64], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP-8]
  push R10
  mov R10, [RBP-16]
  push R10
  mov R10, [RBP-24]
  push R10
  mov R10, [RBP-32]
  push R10
  mov R10, [RBP-40]
  push R10
  mov R10, [RBP-48]
  push R10
  mov R10, [RBP-56]
  push R10
  mov R10, [RBP-64]
  push R10
  call test
  pop R10
  pop R10
  pop R10
  pop R10
  pop R10
  pop R10
  pop R10
  pop R10
  ;; End of CApp-----------------------------
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
