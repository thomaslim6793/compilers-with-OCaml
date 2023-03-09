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
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+32]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+24]
  add RAX, R11
  jo err_overflow
  mov [RBP-8], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-8]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+16]
  add RAX, R11
  jo err_overflow
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
  push 0
  push 0
f2_body:
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  add RAX, R11
  jo err_overflow
  mov [RBP-8], RAX
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  add RAX, R11
  jo err_overflow
  mov [RBP-16], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP+24]
  push R10
  mov R10, [RBP+16]
  push R10
  mov R10, [RBP-16]
  push R10
  call f1
  pop R10
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-24], RAX
  mov RAX, 2
  test RAX, 0x1
  jnz err_comp_not_num
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_comp_not_num
  mov R11, 2
  cmp RAX, R11
  mov RAX, 0xffffffffffffffff
  jl less#37
  mov RAX, 0x7fffffffffffffff
less#37:
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_17
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+16]
  add RAX, R11
  jo err_overflow
  mov [RBP-40], RAX
  mov RAX, [RBP-8]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-40]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP-8]
  add RAX, R11
  jo err_overflow
  jmp done_17
if_false_17:
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  sub RAX, R11
  jo err_overflow
  mov [RBP-40], RAX
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  sub RAX, R11
  jo err_overflow
  mov [RBP-48], RAX
  ;; Start of Tail CApp----------------------------
  mov RAX, [RBP-40]
  push RAX
  mov RAX, [RBP-48]
  push RAX
  pop RAX
  mov [RBP+16], RAX
  pop RAX
  mov [RBP+24], RAX
  jmp f2_body
  ;; End of CApp-----------------------------
done_17:
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
  mov R10, 20
  push R10
  mov R10, 20
  push R10
  call f2
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
