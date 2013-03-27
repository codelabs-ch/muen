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
