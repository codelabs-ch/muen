.global kernel_pml4_ptr
.global subjects_pt_ptr

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
subjects_pt_start:
subject0_pml4:
	.quad 0x1f100f
	.fill 511, 8, 0x0

	.align 4096
subject0_pdp:
	.quad 0x1f200f
	.fill 511, 8, 0x0

	.align 4096
subject0_pd:
	.quad 0x1f300f
	.fill 511, 8, 0x0

	.align 4096
subject0_pte:
	.quad 0x20018f
	.quad 0x20118f
	.quad 0x20218f
	.fill 181, 8, 0x0
	// PTE for video memory b8000
	.quad 0x0b818f
	.fill 327, 8, 0x0

	.align 4096
subject1_pml4:
	.quad 0x1f500f
	.fill 511, 8, 0x0

	.align 4096
subject1_pdp:
	.quad 0x1f600f
	.fill 511, 8, 0x0

	.align 4096
subject1_pd:
	.quad 0x1f700f
	.fill 511, 8, 0x0

	.align 4096
subject1_pte:
	.quad 0x21018f
	.quad 0x21118f
	.quad 0x21218f
	.fill 181, 8, 0x0
	// PTE for video memory b8000
	.quad 0x0b818f
	.fill 327, 8, 0x0

subjects_pt_ptr:
	.quad subjects_pt_start
