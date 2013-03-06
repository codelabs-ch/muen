SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		*(.multiboot)
		*(.text)
	}

	.data 0x00110000 :
	{
		*(.pagetable)
		*(.gdt)
		*(.data)
	}

	. = ALIGN (4096);

	.bss :
	{
		. += 0x1000;
		kernel_stack = .;
		. += 0x2000;
		guest_stack = .;
	}

	. = 0x001f0000;
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
