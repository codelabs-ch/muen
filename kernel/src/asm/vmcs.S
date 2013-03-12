.data

.global vmxon_ptr
.global vmcs_ptr

vmxon_ptr:
	.quad vmxon_start

vmcs_ptr:
	.quad vmcs_start

	.align 4096

// VMXON region, see Intel SDM 3C, chapter 24.10.5.

vmxon_start:
	.fill 4096

// VMCS region, see Intel SDM 3C, chapter 24.2.

vmcs_start:
	.fill 8192
