#include "gdt.h"

#define CR0_PE_BIT 1

.section .trampoline, "a"
.code16

ap_start:
	cli

	lgdt ap_gdt_ptr

	// enable protected mode

	movl %cr0, %eax
	or $CR0_PE_BIT, %eax
	movl %eax, %cr0

	ljmp $8, $ap_start32

.code32
ap_start32:
	movl $0x10, %eax
	mov %eax, %ds
	mov %eax, %ss

	ljmp $8, $ap_entry_point

	.align 4
	.space 2

ap_gdt_ptr:
	.word ap_gdt_end - ap_gdt_start - 1 // limit
	.long ap_gdt_start                  // base address

	.align 4
ap_gdt_start:
	// null descriptor
	.quad 0
	// 32-bit code segment descriptor, index 1
	.long 0xffff
	.long (GDTE_32BIT | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM | GDTE_GRANULAR_4K)
	// 32-bit data segment descriptor, index 2
	.long 0xffff
	.long (GDTE_32BIT | GDTE_PRESENT | GDTE_TYPE_DATA_A | GDTE_TYPE_DATA_W | GDTE_NON_SYSTEM | GDTE_GRANULAR_4K)
ap_gdt_end:
