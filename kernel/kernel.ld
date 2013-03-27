SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		multiboot.o(.multiboot)
		*(.text)
	}

	.data 0x00110000 :
	{
		*(.gdt)
		*(.data)
	}

	.rodata :
	{
		*(.rodata)
	}

	. = ALIGN (4096);

	.bss :
	{
		. += 0x1000;
		kernel_stack = .;
	}

	.interfaces 0x001ff000 :
	{
		*(.interfaces)
	}

	. = 0x00300000;
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
