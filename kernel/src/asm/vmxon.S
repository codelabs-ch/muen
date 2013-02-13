// VMXON region, see Intel SDM vol. 3C, chapter 24.10.5.

.data

.global vmxon_pointer

vmxon_pointer:
	.quad vmxon_start

	.align 4096

vmxon_start:
	.fill 4096
