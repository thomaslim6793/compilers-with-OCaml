section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
test:
  push RBP
  mov RBP, RSP
test_body:
  mov RAX, [RBP+16]
  test RAX, 0x1
  jnz err_arith_not_num
  mov RAX, [RBP+24]
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
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  mov RAX, 4
  mov [RBP-8], RAX
  ;; Start of CApp----------------------------
  mov R10, 2
  push R10
  mov R10, 4
  push R10
  call test
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
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
