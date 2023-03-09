section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 16
  mov RAX, 9223372036854775802
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov RAX, 9223372036854775804
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov R11, 9223372036854775802
  cmp RAX, R11
  mov RAX, QWORD 0xffffffffffffffff
  jl less#3
  mov RAX, QWORD 0x7fffffffffffffff
less#3:
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
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
