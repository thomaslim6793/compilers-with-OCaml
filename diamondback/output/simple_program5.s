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
  mov RAX, [RBP+56]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+64]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+56]
  add RAX, R11
  jo err_overflow
  mov [RBP-8], RAX
  mov RAX, [RBP+48]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-8]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+48]
  add RAX, R11
  jo err_overflow
  mov [RBP-16], RAX
  mov RAX, [RBP+40]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+40]
  add RAX, R11
  jo err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP+32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+32]
  add RAX, R11
  jo err_overflow
  mov [RBP-32], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+24]
  add RAX, R11
  jo err_overflow
  mov [RBP-40], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-40]
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
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  ;; Start of CApp----------------------------
  mov R10, 2
  push R10
  mov R10, 4
  push R10
  mov R10, 6
  push R10
  mov R10, 8
  push R10
  mov R10, 10
  push R10
  mov R10, 12
  push R10
  mov R10, 14
  push R10
  call test
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
