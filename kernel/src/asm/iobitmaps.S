.section .rodata

	.align 4096
io_bitmap_start:
io_bitmap_a:
	.fill 122, 1, 0xff
	// Allow ports 0x03d4 and 0x03d5
	.byte 0xcf
	.fill 3973, 1, 0xff

	.align 4096
io_bitmap_b:
	.fill 4096, 1, 0xff

tau0_io_bitmap_a:
	.fill 2582, 1, 0xff
	// Allow ports 0x50b0 and 0x50b5
	.byte 0xde
	.fill 1513, 1, 0xff

	.align 4096
tau0_io_bitmap_b:
	.fill 4096, 1, 0xff
