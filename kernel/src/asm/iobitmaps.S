.global io_bitmap_ptr

.section .data

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

io_bitmap_ptr:
	.quad io_bitmap_start
