section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
square:
  push RBP
  mov RBP, RSP
square_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP+16]
  sar RAX, 1
  imul RAX, R11
  jo err_overflow
  mov RSP, RBP
  pop RBP
  ret
max:
  push RBP
  mov RBP, RSP
  push 0
  push 0
max_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_comp_not_num
  mov RAX, [RBP+24]
  test RAX, 0x1
  jnz err_comp_not_num
  mov R11, [RBP+16]
  cmp RAX, R11
  mov RAX, 0xffffffffffffffff
  jge greater_eq#27
  mov RAX, 0x7fffffffffffffff
greater_eq#27:
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_21
  mov RAX, [RBP+24]
  jmp done_21
if_false_21:
  ;; Start of Tail CApp----------------------------
  mov RAX, [RBP+16]
  push RAX
  mov RAX, [RBP+24]
  push RAX
  pop RAX
  mov [RBP+16], RAX
  pop RAX
  mov [RBP+24], RAX
  jmp max_body
  ;; End of CApp-----------------------------
done_21:
  mov RSP, RBP
  pop RBP
  ret
composite:
  push RBP
  mov RBP, RSP
  push 0
  push 0
composite_body:
  ;; Start of CApp----------------------------
  mov R10, [RBP+24]
  push R10
  call square
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-8], RAX
  ;; Start of CApp----------------------------
  mov R10, [RBP+16]
  push R10
  call square
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-16], RAX
  ;; Start of Tail CApp----------------------------
  mov RAX, [RBP-8]
  push RAX
  mov RAX, [RBP-16]
  push RAX
  pop RAX
  mov [RBP+16], RAX
  pop RAX
  mov [RBP+24], RAX
  jmp max_body
  ;; End of CApp-----------------------------
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
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  ;; Start of CApp----------------------------
  mov R10, -6
  push R10
  mov R10, 6
  push R10
  call composite
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  mov RDI, RAX
  call print
  mov [RBP-16], RAX
  ;; Start of CApp----------------------------
  mov R10, 8
  push R10
  mov R10, 10
  push R10
  call composite
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP-16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov R11, [RBP-24]
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
