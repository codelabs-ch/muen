SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		*(.multiboot)
		*(.text)
	}

	.data 0x110000 :
	{
		*(.*data*)
	}
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
