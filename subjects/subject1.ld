ENTRY(main)

SECTIONS
{
	. = 0x001a0000;

	.text :	{ *(.text) }
	.data :	{ *(.data) }
	.bss  : { *(.bss)  }

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
