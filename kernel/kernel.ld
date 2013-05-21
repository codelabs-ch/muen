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
		*(.bss)
	}

	.stack 0x00113000 :
	{
		. += 0x4000;
	}

	.cpu_local 0x00117000 :
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

	. = 0x00600000;
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
