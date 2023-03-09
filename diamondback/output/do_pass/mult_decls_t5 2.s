section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
f1:
  push RBP
  mov RBP, RSP
  push 0
  push 0
f1_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_comp_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_comp_not_num
  mov R11, [RBP+16]
  cmp RAX, R11
  mov RAX, 0xffffffffffffffff
  jg greater#36
  mov RAX, 0x7fffffffffffffff
greater#36:
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_28
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  add RAX, R11
  jo err_overflow
  jmp done_28
if_false_28:
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  add RAX, R11
  jo err_overflow
done_28:
  mov RSP, RBP
  pop RBP
  ret
f2:
  push RBP
  mov RBP, RSP
  push 0
  push 0
  push 0
  push 0
f2_body:
  mov RAX, 10
  mov [RBP-8], RAX
  mov RAX, 6
  mov [RBP-16], RAX
  mov RAX, 6
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 6
  add RAX, R11
  jo err_overflow
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP-24]
  sub RAX, R11
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
  ;; Start of CApp----------------------------
  mov R10, [RBP-8]
  push R10
  mov R10, [RBP-24]
  push R10
  call f1
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-32], RAX
  mov RAX, 8
  mov [RBP-40], RAX
  mov RAX, 22
  mov [RBP-48], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP-40]
  push R10
  mov R10, [RBP-48]
  push R10
  call f2
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-56], RAX
  mov RAX, [RBP-56]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP-56]
  sar RAX, 1
  imul RAX, R11
  jo err_overflow
  mov [RBP-64], RAX
  mov RAX, [RBP-64]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP-64]
  add RAX, R11
  jo err_overflow
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
