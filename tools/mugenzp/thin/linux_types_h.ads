with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_types_h is

   subtype uu_le16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:27

   subtype uu_be16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:28

   subtype uu_le32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:29

   subtype uu_be32 is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:30

   subtype uu_le64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:31

   subtype uu_be64 is asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/types.h:32

   subtype uu_sum16 is asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/types.h:34

   subtype uu_wsum is asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/types.h:35

end linux_types_h;
