// Segment descriptors: See Intel SDM 3A, chapter 3.4.5.

#define GDTE_CODE       (1 << 11)
#define GDTE_NON_SYSTEM (1 << 12)
#define GDTE_PRESENT    (1 << 15)
#define GDTE_LONG       (1 << 21)

.global gdt_pointer

.section .gdt

	.align 4
	.space 2

// Segment descriptor tables: See Intel SDM 3A, chapter 3.5.1.
gdt_pointer:
	.word gdt_end - gdt_start - 1 // limit
	.long gdt_start               // base address

	.align 8
gdt_start:
	// Null descriptor
	.quad 0x0000000000000000
	// 64-bit code segment descriptor, index 8
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM)
gdt_end:
