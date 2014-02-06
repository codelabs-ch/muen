with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package asm_generic_int_ll64_h is

   subtype uu_s8 is signed_char;  -- /usr/include/asm-generic/int-ll64.h:19

   subtype uu_u8 is unsigned_char;  -- /usr/include/asm-generic/int-ll64.h:20

   subtype uu_s16 is short;  -- /usr/include/asm-generic/int-ll64.h:22

   subtype uu_u16 is unsigned_short;  -- /usr/include/asm-generic/int-ll64.h:23

   subtype uu_s32 is int;  -- /usr/include/asm-generic/int-ll64.h:25

   subtype uu_u32 is unsigned;  -- /usr/include/asm-generic/int-ll64.h:26

   subtype uu_s64 is Long_Long_Integer;  -- /usr/include/asm-generic/int-ll64.h:29

   subtype uu_u64 is Extensions.unsigned_long_long;  -- /usr/include/asm-generic/int-ll64.h:30

end asm_generic_int_ll64_h;
