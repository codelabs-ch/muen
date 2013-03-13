.global tau0kernel_iface_ptr

.data

tau0kernel_iface_ptr:
	.quad iface_start

.section .interfaces, "a"

	.align 4096

// τ0 -> kernel interface page

iface_start:
	.fill 4096
