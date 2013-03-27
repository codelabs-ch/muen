#include "policy.h"

.global _start
.global kernel_stack_ptr

#define IA32_VMX_BASIC 0x480
#define IA32_EFER      0xc0000080

#define CR0_NE_BIT   5
#define CR0_WP_BIT   16
#define CR0_PG_BIT   31
#define CR4_PAE_BIT  5
#define EFER_LME_BIT 8
#define EFER_NXE_BIT 11

#define MBOOT_PAGEALIGN  1 <<  0
#define MBOOT_MEMINFO    1 <<  1
#define MBOOT_AOUTKLUDGE 1 << 16
#define MBOOT_FLAGS      MBOOT_PAGEALIGN | MBOOT_MEMINFO | MBOOT_AOUTKLUDGE
#define MBOOT_MAGIC      0x1BADB002
#define MBOOT_CHECKSUM   -(MBOOT_MAGIC + MBOOT_FLAGS)

.section .multiboot
	.align 4
multiboot_header:
	.long MBOOT_MAGIC
	.long MBOOT_FLAGS
	.long MBOOT_CHECKSUM
	.long multiboot_header
	.long image_start
	.long 0
	.long image_end
	.long _start

.text
.code32

// See "Initializing IA-32e Mode" in the Intel SDM, Volume 3A, Chapter 9.8.5.

_start:

	// (0) initialize VMX regions

	movl $IA32_VMX_BASIC, %ecx
	rdmsr
	movl %eax, 0

	// (1) enable PAE

	movl %cr4, %eax
	btsl $CR4_PAE_BIT, %eax
	movl %eax, %cr4

	// (2) load CR3 with PML4

	movl $KERNEL_PML4, %eax
	movl %eax, %cr3

	// (3) enable IA-32e mode and execute-disable

	movl $IA32_EFER, %ecx
	rdmsr
	btsl $EFER_LME_BIT, %eax
	btsl $EFER_NXE_BIT, %eax
	wrmsr

	// (4) enable paging, write protection and native FPU error reporting

	movl %cr0, %eax
	btsl $CR0_PG_BIT, %eax
	btsl $CR0_WP_BIT, %eax
	btsl $CR0_NE_BIT, %eax
	movl %eax, %cr0

	// (5) set up GDT

	lgdt gdt_ptr

	// (6) indirect long jump to 64-bit code

	ljmp $8, $start64

.code64
start64: .align 4096

	// set up kernel stack

	movq (kernel_stack_ptr), %rsp

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

.data
kernel_stack_ptr:
	.quad kernel_stack
