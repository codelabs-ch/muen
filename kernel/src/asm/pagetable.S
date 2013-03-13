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
	// PD for 2MB kernel memory
	.quad 0x00018f

	.quad 0x11300f
	.fill 510, 8, 0x0

	.align 4096
kernel_pte:
	// PTE for τ0/kernel interface
	.quad 0x800000000020018d
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
	.quad 0x000000000024008d
	.quad 0x800000000024108d
	.quad 0x800000000024208f
	.quad 0x800000000024308f
	.quad 0x800000000020018f
	.fill 179, 8, 0x0
	// PTE for video memory b8000
	.quad 0x80000000000b818f
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
	.quad 0x000000000025008d
	.quad 0x800000000025108d
	.quad 0x800000000025208f
	.quad 0x800000000025308f
	.fill 180, 8, 0x0
	// PTE for video memory b8000
	.quad 0x80000000000b818f
	.fill 327, 8, 0x0

subjects_pt_ptr:
	.quad subjects_pt_start
