serial.out;Booting Muen kernel
serial.out;MCE: IA32_MCG_CAP 16#00000c09#
serial.out;XCR0: 16#0000000000000007#
serial.out;I/O APIC RTE 16#01#: Routing IRQ 16#01# as vector 16#21# to CPU with APIC ID 16#00#, VT-d IRT index 16#01#
serial.out;I/O APIC RTE 16#0c#: Routing IRQ 16#0c# as vector 16#2c# to CPU with APIC ID 16#00#, VT-d IRT index 16#0c#
serial.out;IOMMU 01: TES 01, RTPS 01, FLS 00, AFLS 00, WBFS 00, QIES 00, IRES 01, IRTPS 01, CFIS 00
serial.out;IOMMU 02: TES 01, RTPS 01, FLS 00, AFLS 00, WBFS 00, QIES 00, IRES 01, IRTPS 01, CFIS 00
serial.out;Waiting for AP wakeup event
serial.out;AP wakeup event received
serial.out;ttyS1 at I/O 0x2f8 (irq = 5, base_baud = 115200)
serial.out;pci 0000:00:19.0: \[8086:1502\] type 00 class 0x020000
serial.out;pci 0000:00:19.0: reg 0x10: \[mem 0xf7f00000-0xf7f1ffff\]
serial.out;pci 0000:00:19.0: reg 0x14: \[mem 0xf7f39000-0xf7f39fff\]
serial.out;pci 0000:00:19.0: reg 0x18: \[io  0xf080-0xf09f\]
serial.out;pci 0000:00:19.0: BAR 0: reserving \[mem 0xf7f00000-0xf7f1ffff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:19.0: BAR 1: reserving \[mem 0xf7f39000-0xf7f39fff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:19.0: BAR 2: reserving \[io  0xf080-0xf09f flags 0x40101\] (d=0, p=0)
serial.out;smpboot: Max logical packages: 3
serial.out;muen-smp: Setup timer for CPU#0:.*
serial.out;muen-smp: Setup timer for CPU#1:.*
serial.out;muen-smp: Setup timer for CPU#2:.*
serial.out;smp: Brought up 1 node, 3 CPUs
serial.out;smpboot: Total of 3 processors activated (13800.00 BogoMIPS)
serial.out;e1000e 0000:00:19.0 eth0: MAC: 10, PHY: 11, PBA No: FFFFFF-0FF
serial.out;e1000e 0000:00:1f.0 eth1: MAC: 3, PHY: 8, PBA No: FFFFFF-0FF
serial.out;00:19.0 Ethernet controller: Intel Corporation 82579LM Gigabit Network Connection
serial.out;00:1f.0 Ethernet controller: Intel Corporation 82574L Gigabit Network Connection
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/0
serial.out;e1000e 0000:00:1e.0: IRQ 17 for MSI-X
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/1
serial.out;e1000e 0000:00:1e.0: IRQ 18 for MSI-X
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/2
serial.out;e1000e 0000:00:1e.0: IRQ 19 for MSI-X
serial.out;xhci_hcd 0000:00:01.0: Programming MSI address 0xfee00338 with IRTE handle 25/0
serial.out;xhci_hcd 0000:00:01.0: IRQ 7 for MSI
serial.out;Pciconf 16#00f8#: Initializing device
serial.out;Pciconf 16#00f8#: MSI(X) cap ID 16#05# @ offset 16#d0#
serial.out;Pciconf 16#00f8#: MSI(X) cap ID 16#11# @ offset 16#a0#
serial.out;Pciconf 16#0008#: Initializing device
serial.out;Pciconf 16#0008#: MSI(X) cap ID 16#05# @ offset 16#80#
serial.out;Pciconf 16#0008#: Registering xHCI handoff quirk for vendor 16#8086# device 16#1e31# class 16#000c0330#
serial.out;64 bytes from 192.168.254.1: seq=.* ttl=.* time=.* ms
serial.out;MUENBLOCKINTEGPASSED
serial.out;example: SMART Status: OK!
serial.out;example: Wrote 16#00040000# bytes in.* ticks
serial.out;example: Read 16#00040000# bytes in.* ticks
serial.out;example: Muenblock example done
serial.out;*** '.*/integtest_kt' DONE
serial.out;x86/fpu: Supporting XSAVE feature 0x001: 'x87 floating point registers'
serial.out;x86/fpu: Supporting XSAVE feature 0x002: 'SSE registers'
serial.out;x86/fpu: Enabled xstate features 0x3, context size is 576 bytes, using 'standard' format.
