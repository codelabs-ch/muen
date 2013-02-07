.global kernel_pml4_ptr

.section .pagetable

// PML4 is located at 0x110000

kernel_pml4: .align 4096
	.quad 0x11100f
	.fill 511, 8, 0x0

// PDP is located at 0x111000

kernel_pdp: .align 4096
	.quad 0x11200f
	.fill 511, 8, 0x0

// PD is located at 0x112000

kernel_pd: .align 4096
	.quad 0x00018f
	.fill 511, 8, 0x0

kernel_pml4_ptr:
	.long	kernel_pml4
