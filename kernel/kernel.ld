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

	.subjects_pt 0x001f0000 :
	{
		*(.subject_pt)
	}

	.interfaces 0x00200000 :
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
