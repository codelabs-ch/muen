ENTRY(main)

SECTIONS
{
	.text :	{ *(.text) }
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
