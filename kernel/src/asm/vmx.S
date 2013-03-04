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
	push %r9
	push %r8
	push %rdi
	push %rsi
	push %rbp
	push %rax
	push %rbx
	push %rcx
	push %rdx
	call handle_vmx_exit
	pop %rdx
	pop %rcx
	pop %rbx
	pop %rax
	pop %rbp
	pop %rsi
	pop %rdi
	pop %r8
	pop %r9
	pop %r10
	pop %r11
	pop %r12
	pop %r13
	pop %r14
	pop %r15
	vmresume
