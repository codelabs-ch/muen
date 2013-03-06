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

	. = ALIGN (4096);

	.subject0 :
	{
		../../subjects/obj/subject_0.o(.text)
		../../subjects/obj/subject_0.o(.data)
		../../subjects/obj/subject_0.o(.bss)
	}

	. = ALIGN (4096);

	.subject1 :
	{
		../../subjects/obj/subject_1.o(.text)
		../../subjects/obj/subject_1.o(.data)
		../../subjects/obj/subject_1.o(.bss)
	}

	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
