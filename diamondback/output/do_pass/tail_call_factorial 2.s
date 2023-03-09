section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
fact:
  push RBP
  mov RBP, RSP
  push 0
  push 0
  push 0
  push 0
fact_body:
  mov RAX, 2
  test RAX, 0x1
  jnz err_comp_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_comp_not_num
  mov R11, 2
  cmp RAX, R11
  mov RAX, 0xffffffffffffffff
  jl less#20
  mov RAX, 0x7fffffffffffffff
less#20:
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_6
  mov RAX, [RBP+16]
  jmp done_6
if_false_6:
  mov RAX, 2
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, 2
  sub RAX, R11
  jo err_overflow
  mov [RBP-16], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+24]
  sar RAX, 1
  imul RAX, R11
  jo err_overflow
  mov [RBP-24], RAX
  ;; Start of Tail CApp----------------------------
  mov RAX, [RBP-16]
  push RAX
  mov RAX, [RBP-24]
  push RAX
  pop RAX
  mov [RBP+16], RAX
  pop RAX
  mov [RBP+24], RAX
  jmp fact_body
  ;; End of CApp-----------------------------
done_6:
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
  mov R10, 8
  push R10
  mov R10, 2
  push R10
  call fact
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
