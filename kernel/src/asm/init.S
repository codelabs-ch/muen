#include "policy.h"

.global _start

#define IA32_VMX_BASIC 0x480
#define IA32_EFER      0xc0000080

#define TRAMPOLINE_ADDR 0x0007e000

#define CR0_NE_BIT   5
#define CR0_WP_BIT   16
#define CR0_PG_BIT   31
#define CR4_PAE_BIT  5
#define EFER_LME_BIT 8
#define EFER_NXE_BIT 11

// clear pagecount number of 4k memory starting at given address
.macro clear address pagecount
	movl $\address, %ebx
	movl $\pagecount, %eax
	movl $0x400, %ecx
	mull %ecx
	movl %eax, %ecx
1:
	movl $0, (%ebx)
	add $4, %ebx
	loop 1b
.endm

.text
.code32

// see "Initializing IA-32e Mode" in the Intel SDM, Volume 3A, Chapter 9.8.5

_start:

	// copy AP trampoline code to low-memory

	mov $trampoline_start, %esi
	mov $TRAMPOLINE_ADDR, %edi
	cld
	mov $trampoline_end, %ecx
	sub $trampoline_start, %ecx
	rep movsb

	// initialize VMX regions (one VMXON + one VMCS per subject)

	clear VMXON_ADDRESS 1
	clear VMCS_ADDRESS SUBJECT_COUNT

	movl $IA32_VMX_BASIC, %ecx
	rdmsr

	movl %eax, VMXON_ADDRESS

	movl $SUBJECT_COUNT, %ecx
	movl $VMCS_ADDRESS, %edx
set_rev:
	movl %eax, (%edx)
	add $0x1000, %edx
	loop set_rev

	// enable PAE

	movl %cr4, %eax
	btsl $CR4_PAE_BIT, %eax
	movl %eax, %cr4

	// load CR3 with PML4

	movl $KERNEL_PML4, %eax
	movl %eax, %cr3

	// enable IA-32e mode and execute-disable

	movl $IA32_EFER, %ecx
	rdmsr
	btsl $EFER_LME_BIT, %eax
	btsl $EFER_NXE_BIT, %eax
	wrmsr

	// enable paging, write protection and native FPU error reporting

	movl %cr0, %eax
	btsl $CR0_PG_BIT, %eax
	btsl $CR0_WP_BIT, %eax
	btsl $CR0_NE_BIT, %eax
	movl %eax, %cr0

	// set up GDT

	lgdt gdt_ptr

	// indirect long jump to 64-bit code

	ljmp $8, $start64

.code64
start64: .align 4096

	// set up kernel stack

	movq $KERNEL_STACK, %rsp

	// set up kernel segment selectors

	mov $0x10, %ecx
	mov %ecx, %ds
	mov %ecx, %es
	mov %ecx, %fs
	mov %ecx, %gs
	mov %ecx, %ss

	// initialize Ada runtime

	call adainit

	// call kernel main

	call kmain

	cli

freeze:
	hlt
	jmp freeze

trampoline_start:
	cli
	hlt
trampoline_end:
