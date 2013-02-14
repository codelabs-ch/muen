.data

.global vmxon_pointer
.global vmcs_pointer

vmxon_pointer:
	.quad vmxon_start

vmcs_pointer:
	.quad vmcs_start

	.align 4096

// VMXON region, see Intel SDM 3C, chapter 24.10.5.

vmxon_start:
	.fill 4096

// VMCS region, see Intel SDM 3C, chapter 24.2.

vmcs_start:
	.fill 4096
