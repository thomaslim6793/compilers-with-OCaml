section .text
extern error
extern print
extern printstack
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 48
  mov RAX, 2
  mov [RBP-8], RAX
  mov RAX, 4
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov RAX, [RBP-8]
  test RAX, QWORD 0x1
  jnz err_comp_not_num
  mov R11, [RBP-16]
  cmp RAX, R11
  mov RAX, QWORD 0xffffffffffffffff
  jl less#9
  mov RAX, QWORD 0x7fffffffffffffff
less#9:
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  test RAX, QWORD 0x1
  jz err_if_not_bool
  mov R11, QWORD 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_14
  mov RAX, QWORD 0xffffffffffffffff
  jmp done_14
if_false_14:
  mov RAX, QWORD 0x7fffffffffffffff
done_14:
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, QWORD 0x1
  mov RAX, QWORD 0xffffffffffffffff
  jz is_num_true#20
  mov RAX, QWORD 0x7fffffffffffffff
is_num_true#20:
  mov [RBP-40], RAX
  mov RAX, [RBP-40]
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
