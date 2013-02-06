.global _start

#define MSR_EFER 0xc0000080

#define CR0_PG_BIT   31
#define CR4_PAE_BIT  5
#define EFER_LME_BIT 8

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

	// (1) enable PAE

	movl %cr4, %eax
	btsl $CR4_PAE_BIT, %eax
	movl %eax, %cr4

	// (2) load CR3 with PML4

	movl kernel_pml4_ptr, %eax
	movl %eax, %cr3

	// (3) enable IA-32e mode

	movl $MSR_EFER, %ecx
	rdmsr
	btsl $EFER_LME_BIT, %eax
	wrmsr

	// (4) enable paging

	movl %cr0, %eax
	btsl $CR0_PG_BIT, %eax
	movl %eax, %cr0

	movl $0x07690748, 0xB80F0

	cli

freeze:
	hlt
	jmp freeze
