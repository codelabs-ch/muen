.global isr_handler_ptr

.text
handle_all:
	call dispatch_interrupt
	iretq

.data
isr_handler_ptr:
	.quad handle_all

