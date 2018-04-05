serial.out;Booting Muen kernel
serial.out;MCE: IA32_MCG_CAP 16#00000c09#
serial.out;XCR0: 16#0000000000000007#
serial.out;I/O APIC RTE 16#01#: Routing IRQ 16#01# as vector 16#21# to CPU with APIC ID 16#00#, VT-d IRT index 16#01#
serial.out;I/O APIC RTE 16#0c#: Routing IRQ 16#0c# as vector 16#2c# to CPU with APIC ID 16#00#, VT-d IRT index 16#0c#
serial.out;IOMMU 01: TES 01, RTPS 01, FLS 00, AFLS 00, WBFS 00, QIES 00, IRES 01, IRTPS 01, CFIS 00
serial.out;IOMMU 02: TES 01, RTPS 01, FLS 00, AFLS 00, WBFS 00, QIES 00, IRES 01, IRTPS 01, CFIS 00
serial.out;ttyS1 at I/O 0x2f8 (irq = 5, base_baud = 115200)
serial.out;pci 0000:00:19.0: \[8086:1502\] type 00 class 0x020000
serial.out;pci 0000:00:19.0: reg 0x10: \[mem 0xf7f00000-0xf7f1ffff\]
serial.out;pci 0000:00:19.0: reg 0x14: \[mem 0xf7f39000-0xf7f39fff\]
serial.out;pci 0000:00:19.0: reg 0x18: \[io  0xf080-0xf09f\]
serial.out;pci 0000:00:1f.0: \[8086:10d3\] type 00 class 0x020000
serial.out;pci 0000:00:1f.0: reg 0x10: \[mem 0xf7e00000-0xf7e1ffff\]
serial.out;pci 0000:00:1f.0: reg 0x18: \[io  0xe000-0xe01f\]
serial.out;pci 0000:00:1f.0: reg 0x1c: \[mem 0xf7e20000-0xf7e23fff\]
serial.out;pci 0000:00:1f.0: \[8086:1e03\] type 00 class 0x010601
serial.out;pci 0000:00:1f.0: reg 0x10: \[io  0xf0d0-0xf0d7\]
serial.out;pci 0000:00:1f.0: reg 0x14: \[io  0xf0c0-0xf0c3\]
serial.out;pci 0000:00:1f.0: reg 0x18: \[io  0xf0b0-0xf0b7\]
serial.out;pci 0000:00:1f.0: reg 0x1c: \[io  0xf0a0-0xf0a3\]
serial.out;pci 0000:00:1f.0: reg 0x20: \[io  0xf060-0xf07f\]
serial.out;pci 0000:00:1f.0: reg 0x24: \[mem 0xf7f36000-0xf7f367ff\]
serial.out;pci 0000:00:19.0: BAR 0: reserving \[mem 0xf7f00000-0xf7f1ffff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:19.0: BAR 1: reserving \[mem 0xf7f39000-0xf7f39fff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:19.0: BAR 2: reserving \[io  0xf080-0xf09f flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 0: reserving \[mem 0xf7e00000-0xf7e1ffff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 2: reserving \[io  0xe000-0xe01f flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 3: reserving \[mem 0xf7e20000-0xf7e23fff flags 0x40200\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 0: reserving \[io  0xf0d0-0xf0d7 flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 1: reserving \[io  0xf0c0-0xf0c3 flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 2: reserving \[io  0xf0b0-0xf0b7 flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 3: reserving \[io  0xf0a0-0xf0a3 flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 4: reserving \[io  0xf060-0xf07f flags 0x40101\] (d=0, p=0)
serial.out;pci 0000:00:1f.0: BAR 5: reserving \[mem 0xf7f36000-0xf7f367ff flags 0x40200\] (d=0, p=0)
serial.out;e1000e 0000:00:19.0 eth0: MAC: 10, PHY: 11, PBA No: FFFFFF-0FF
serial.out;e1000e 0000:00:1f.0 eth1: MAC: 3, PHY: 8, PBA No: FFFFFF-0FF
serial.out;00:19.0 Ethernet controller: Intel Corporation 82579LM Gigabit Network Connection
serial.out;00:1f.0 Ethernet controller: Intel Corporation 82574L Gigabit Network Connection
serial.out;ahci 0000:00:1f.0: version 3.0
serial.out;PCI: setting IRQ 11 as level-triggered
serial.out;ahci 0000:00:1f.0: Programming MSI address 0xfee004b8 with IRTE handle 37/0
serial.out;ahci 0000:00:1f.0: IRQ 11 for MSI
serial.out;ahci 0000:00:1f.0: AHCI 0001.0300 32 slots 6 ports 6 Gbps
serial.out;ahci 0000:00:1f.0: flags: 64bit ncq pm led clo pio slum part ems apst
serial.out;SATA max UDMA/133 abar m2048@0xf7f36000 port 0xf7f36
serial.out;SATA link up 3.0 Gbps (SStatus 123 SControl 300)
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/0
serial.out;e1000e 0000:00:1e.0: IRQ 40 for MSI-X
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/1
serial.out;e1000e 0000:00:1e.0: IRQ 41 for MSI-X
serial.out;e1000e 0000:00:1e.0: Programming MSI address 0xfee00578 with IRTE handle 43/2
serial.out;e1000e 0000:00:1e.0: IRQ 42 for MSI-X
serial.out;xhci_hcd 0000:00:01.0: Programming MSI address 0xfee00338 with IRTE handle 25/0
serial.out;xhci_hcd 0000:00:01.0: IRQ 7 for MSI
serial.out;0:0:0: \[sda\] Attached SCSI disk
serial.out;Pciconf 16#00f8#: Initializing device
serial.out;Pciconf 16#00f8#: MSI(X) cap ID 16#05# @ offset 16#d0#
serial.out;Pciconf 16#00f8#: MSI(X) cap ID 16#11# @ offset 16#a0#
serial.out;Pciconf 16#0008#: Initializing device
serial.out;Pciconf 16#0008#: MSI(X) cap ID 16#05# @ offset 16#80#
serial.out;Pciconf 16#0008#: Registering xHCI handoff quirk for vendor 16#8086# device 16#1e31# class 16#000c0330#
serial.out;new full-speed USB device number 2 using xhci_hcd
serial.out;64 bytes from 192.168.254.1: seq=.* ttl=.* time=.* ms
serial.out;*** '/usr/bin/integtest_kt' DONE
