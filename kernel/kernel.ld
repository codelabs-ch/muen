SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		multiboot.o(.multiboot)
		*(.text)

		trampoline_src = .;
	}

	.trampoline 0x00000000 : AT (ADDR (.text) + SIZEOF (.text))
	{
		trampoline_start = .;
		trampoline.o(.trampoline)
		trampoline_end = .;
	}

	.data 0x00110000 : AT (0x00110000)
	{
		*(.gdt)
		*(.data)
	}

	.bss 0x00111000 :
	{
		. += 0x4000;
	}

	.rodata :
	{
		*(.rodata)
	}

	.interfaces 0x001ff000 :
	{
		*(.interfaces)
	}

	. = 0x00400000;
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
