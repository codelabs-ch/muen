#include "gdt.h"

.global gdt_ptr

.section .gdt

	.align 4
	.space 2

// segment descriptor tables: See Intel SDM 3A, chapter 3.5.1
gdt_ptr:
	.word gdt_end - gdt_start - 1 // limit
	.long gdt_start               // base address

	.align 8
gdt_start:
	// null descriptor
	.quad 0x0000000000000000
	// 64-bit code segment descriptor, index 1
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM)
	// 64-bit data segment descriptor, index 2
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_TYPE_DATA_A | GDTE_TYPE_DATA_W | GDTE_NON_SYSTEM)
	// task segement descriptor, index 3
	.long 0
	.long (GDTE_PRESENT | GDTE_SYS_TSS)
gdt_end:
