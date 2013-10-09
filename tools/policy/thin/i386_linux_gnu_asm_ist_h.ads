with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package i386_linux_gnu_asm_ist_h is

   type ist_info is record
      signature : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/ist.h:23
      command : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/ist.h:24
      event : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/ist.h:25
      perf_level : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/i386-linux-gnu/asm/ist.h:26
   end record;
   pragma Convention (C_Pass_By_Copy, ist_info);  -- /usr/include/i386-linux-gnu/asm/ist.h:22

end i386_linux_gnu_asm_ist_h;
