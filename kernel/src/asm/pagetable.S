.global kernel_pml4_ptr

.section .pagetable

// PML4 is located at 0x110000
	.align 4096
kernel_pml4:
	.quad 0x11100f
	.fill 511, 8, 0x0

// PDP is located at 0x111000
	.align 4096
kernel_pdp:
	.quad 0x11200f
	.fill 511, 8, 0x0

// PD is located at 0x112000
	.align 4096
kernel_pd:
	.quad 0x00018f
	.fill 511, 8, 0x0

kernel_pml4_ptr:
	.long kernel_pml4

.section .subject_pt, "a"
	.align 4096
subject_pml4:
	.quad 0x1f100f
	.fill 511, 8, 0x0

	.align 4096
subject_pdp:
	.quad 0x1f200f
	.fill 511, 8, 0x0

	.align 4096
subject_pd:
	.quad 0x00018f
	.fill 511, 8, 0x0
