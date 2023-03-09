section .text
extern error
extern print
extern printstack
extern set_stack_bottom
global our_code_starts_here
dnf_instance:
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
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
  push 0
dnf_instance_body:
  mov RAX, [RBP+32]
  test RAX, 0x1
  mov RAX, 0x7fffffffffffffff
  jz is_bool_false#101
  mov RAX, 0xffffffffffffffff
is_bool_false#101:
  mov [RBP-8], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  mov RAX, 0x7fffffffffffffff
  jz is_bool_false#99
  mov RAX, 0xffffffffffffffff
is_bool_false#99:
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-8]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-16]
  and RAX, R11
  mov [RBP-24], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  mov RAX, 0x7fffffffffffffff
  jz is_bool_false#94
  mov RAX, 0xffffffffffffffff
is_bool_false#94:
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-24]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-32]
  and RAX, R11
  mov [RBP-40], RAX
  mov RAX, [RBP-40]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_45
  mov RAX, [RBP+32]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-48], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-48]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP+24]
  and RAX, R11
  mov [RBP-56], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-56]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP+16]
  and RAX, R11
  mov [RBP-64], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-72], RAX
  mov RAX, [RBP-72]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP+32]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-72]
  and RAX, R11
  mov [RBP-80], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-88], RAX
  mov RAX, [RBP-88]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-80]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-88]
  and RAX, R11
  mov [RBP-96], RAX
  mov RAX, [RBP-96]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-64]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-96]
  or RAX, R11
  mov [RBP-104], RAX
  mov RAX, [RBP+24]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-112], RAX
  mov RAX, [RBP-112]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP+32]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-112]
  and RAX, R11
  mov [RBP-120], RAX
  mov RAX, [RBP+16]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-120]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP+16]
  and RAX, R11
  mov [RBP-128], RAX
  mov RAX, [RBP-128]
  test RAX, 0x1
  jz err_logic_not_bool
  mov RAX, [RBP-104]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, [RBP-128]
  or RAX, R11
  jmp done_45
if_false_45:
  mov RAX, 0x7fffffffffffffff
done_45:
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
  mov RDI, RBP
  mov RSI, RAX
  call set_stack_bottom
  ;; Start of CApp----------------------------
  mov R10, 0x7fffffffffffffff
  push R10
  mov R10, 0xffffffffffffffff
  push R10
  mov R10, 0xffffffffffffffff
  push R10
  call dnf_instance
  pop R10
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-8], RAX
  mov RAX, [RBP-8]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-16], RAX
  mov RAX, [RBP-16]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_20
  mov RAX, 0x7fffffffffffffff
  jmp done_20
if_false_20:
  ;; Start of CApp----------------------------
  mov R10, 0xffffffffffffffff
  push R10
  mov R10, 0x7fffffffffffffff
  push R10
  mov R10, 0x7fffffffffffffff
  push R10
  call dnf_instance
  pop R10
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-40], RAX
  mov RAX, [RBP-40]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
done_20:
  mov [RBP-24], RAX
  mov RAX, [RBP-24]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-32], RAX
  mov RAX, [RBP-32]
  test RAX, 0x1
  jz err_if_not_bool
  mov R11, 0x7fffffffffffffff
  cmp RAX, R11
  je if_false_5
  mov RAX, 0x7fffffffffffffff
  jmp done_5
if_false_5:
  ;; Start of CApp----------------------------
  mov R10, 0xffffffffffffffff
  push R10
  mov R10, 0x7fffffffffffffff
  push R10
  mov R10, 0xffffffffffffffff
  push R10
  call dnf_instance
  pop R10
  pop R10
  pop R10
  ;; End of CApp-----------------------------
  mov [RBP-40], RAX
  mov RAX, [RBP-40]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
  mov [RBP-48], RAX
  mov RAX, [RBP-48]
  test RAX, 0x1
  jz err_logic_not_bool
  mov R11, 0x8000000000000000
  xor RAX, R11
done_5:
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
