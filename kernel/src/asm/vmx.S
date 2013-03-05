.global vmx_exit_handler_pointer

.data
vmx_exit_handler_pointer:
	.quad vmx_exit_handler

.text
vmx_exit_handler:
	push %r15
	push %r14
	push %r13
	push %r12
	push %r11
	push %r10
	push %rbp
	push %rbx
	push %rax
	call handle_vmx_exit
