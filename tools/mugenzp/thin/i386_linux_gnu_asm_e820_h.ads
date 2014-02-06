with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package i386_linux_gnu_asm_e820_h is

   --  unsupported macro: E820MAP 0x2d0
   --  unsupported macro: E820MAX 128
   --  unsupported macro: E820_X_MAX E820MAX
   --  unsupported macro: E820NR 0x1e8
   --  unsupported macro: E820_RAM 1
   --  unsupported macro: E820_RESERVED 2
   --  unsupported macro: E820_ACPI 3
   --  unsupported macro: E820_NVS 4
   --  unsupported macro: E820_UNUSABLE 5
   --  unsupported macro: E820_RESERVED_KERN 128
   --  unsupported macro: ISA_START_ADDRESS 0xa0000
   --  unsupported macro: ISA_END_ADDRESS 0x100000
   --  unsupported macro: BIOS_BEGIN 0x000a0000
   --  unsupported macro: BIOS_END 0x00100000
   --  unsupported macro: BIOS_ROM_BASE 0xffe00000
   --  unsupported macro: BIOS_ROM_END 0xffffffff
   type e820entry is record
      addr : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/i386-linux-gnu/asm/e820.h:51
      size : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/i386-linux-gnu/asm/e820.h:52
      c_type : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/e820.h:53
   end record;
   pragma Convention (C_Pass_By_Copy, e820entry);  -- /usr/include/i386-linux-gnu/asm/e820.h:50

   type e820map_map_array is array (0 .. 127) of aliased e820entry;
   type e820map is record
      nr_map : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/e820.h:57
      map : aliased e820map_map_array;  -- /usr/include/i386-linux-gnu/asm/e820.h:58
   end record;
   pragma Convention (C_Pass_By_Copy, e820map);  -- /usr/include/i386-linux-gnu/asm/e820.h:56

end i386_linux_gnu_asm_e820_h;
