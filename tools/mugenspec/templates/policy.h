#define CPU_COUNT     __cpu_count__
#define KERNEL_STACK  0x__stack_addr__
#define VMXON_ADDRESS 0x__vmxon_addr__
#define PAT_HIGH      0x00070405
#define PAT_LOW       0x00010406

.global kernel_pml4_start

.section .rodata
    .align 4
kernel_pml4_start:__kernel_pml4_addrs__

kernel_cpu_ids:
    .byte __kernel_cpu_ids__
