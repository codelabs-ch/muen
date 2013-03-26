.data

.global vmcs_ptr

vmcs_ptr:
	.quad vmcs_start

	.align 4096

// VMCS region, see Intel SDM 3C, chapter 24.2.

vmcs_start:
	.fill 12288
