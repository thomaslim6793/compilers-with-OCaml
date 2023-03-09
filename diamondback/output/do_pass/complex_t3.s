section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
f1:
  push RBP
  mov RBP, RSP
f1_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  mov RAX, 0xffffffffffffffff
  jz is_num_true#10
  mov RAX, 0x7fffffffffffffff
is_num_true#10:
  mov RSP, RBP
  pop RBP
  ret
f2:
  push RBP
  mov RBP, RSP
f2_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_13
  mov RAX, 2
  jmp done_13
if_false_13:
  mov RAX, 4
done_13:
  mov RSP, RBP
  pop RBP
  ret
f3:
  push RBP
  mov RBP, RSP
  push 0
  push 0
f3_body:
  mov RAX, 20
  mov [RBP-8], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_comp_not_num
  mov RAX, [RBP-8]
  test RAX, 0x1
  jnz err_comp_not_num
  mov R11, [RBP+16]
  cmp RAX, R11
  mov RAX, 0xffffffffffffffff
  jg greater#19
  mov RAX, 0x7fffffffffffffff
greater#19:
  mov RSP, RBP
  pop RBP
  ret
our_code_starts_here:
  push RBP
  mov RBP, RSP
  push 0
  push 0
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  ;; Start of CApp----------------------------
  mov R10, 2
  push R10
  call f1
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-8], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP-8]
  push R10
  call f2
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-16], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP-16]
  push R10
  call f3
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
