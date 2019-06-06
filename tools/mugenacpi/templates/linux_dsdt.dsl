DefinitionBlock ("DSDT.aml", "DSDT", 2, "Muen  ", "Homebrew", 0x00000000)
{
    Device (BORD)
    {
        Name (_HID, EisaId ("PNP0C01"))
        Name (_UID, "Muen Board")
        Method (_CRS)
        {
            Return (ResourceTemplate () {
__pci_config_space__})
        }
    }

    Scope (_SB)
    {
        Device (PCI0)
        {
            Name (_HID, EisaId ("PNP0A08"))
            Name (_CID, EisaId ("PNP0A03"))
            Name (_ADR, Zero)
            Name (_BBN, Zero)
            Method (_CBA, 0, Serialized)
            {
                /* Point to MMConf region */
                Return (0x__config_base_address__)
            }
            Method (_CRS, 0, Serialized)
            {
                Name (MCRS, ResourceTemplate ()
                {
                    /* 16 buses */
                    WordBusNumber (ResourceProducer, MinFixed, MaxFixed, PosDecode,
                        0x0000,             // Granularity
                        0x0000,             // Range Minimum
                        0x000f,             // Range Maximum
                        0x0000,             // Translation Offset
                        0x0010,             // Length
                        ,, )
                    /* All i/o ports */
                    DWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                        0x00000000,         // Granularity
                        0x00000000,         // Range Minimum
                        0x0000FFFF,         // Range Maximum
                        0x00000000,         // Translation Offset
                        0x00010000,         // Length
                        ,, , TypeStatic)
__reserved_memory__})
                Return (MCRS)
            }

            Method (_PRT, 0, NotSerialized)
            {
                Return (Package (__interrupt_count__)
                {
__pci_routing_table__})
            }

            Method (_OSC, 4, NotSerialized)
            {
                CreateDWordField (Arg3, Zero, CDW1)
                CreateDWordField (Arg3, One,  CDW2)
                CreateDWordField (Arg3, 0x2,  CDW3)

                If (LEqual (Arg0, ToUUID ("33db4d5b-1ff7-401c-9657-7441c03dd766")))
                {
                    If (LNotEqual (Arg1, 1))
                    {
                        /* Unknown Revision */
                        Or (CDW1, 0x08, CDW1)
                    }
                    /* Clear all capabilities */
                    Store (Zero, CDW2)
                    Store (Zero, CDW3)
                    Return (Arg3)
                }
                Else
                {
                    /* Unknown UUID */
                    Or (CDW1, 0x04, CDW1)
                    Return (Arg3)
                }
            }
            Device (ISA)
            {
                Name (_HID, EisaId ("PNP0A00"))
                Device (SER0)
                {
                    Name (_HID, EisaId ("PNP0501"))
                    Name (_UID, "serial_port_sm")
                    Method (_STA) { Return (0x0f) }
                    Method (_CRS)
                    {
                        Return (ResourceTemplate () {
                            IO (Decode16, 0x3f8, 0x3f8, 0x08, 0x8,)
                            IRQNoFlags () { 4 }
                            DMA (Compatibility, NotBusMaster, Transfer8, ) {}
                        })
                    }
                }
__legacy_devices__}
        }
    }

    Name (_S0, Package (0x04)
    {
        Zero,
        Zero,
        Zero,
        Zero
    })

    Name (_S5, Package (0x04)
    {
        Zero,
        Zero,
        Zero,
        Zero
    })
}

