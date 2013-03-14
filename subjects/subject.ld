ENTRY(main)

SECTIONS
{
	.text :	{ *(.text) }

	. = ALIGN (4096);

	.rodata : { *(.rodata) }

	. = ALIGN (4096);

	.data :	{ *(.data) }

	. = ALIGN (4096);

	.bss :
	{
		. += 0x1000;
		stack = .;
	}

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
