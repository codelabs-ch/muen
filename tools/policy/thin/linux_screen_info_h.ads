with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_screen_info_h is

   --  unsupported macro: VIDEO_TYPE_MDA 0x10
   --  unsupported macro: VIDEO_TYPE_CGA 0x11
   --  unsupported macro: VIDEO_TYPE_EGAM 0x20
   --  unsupported macro: VIDEO_TYPE_EGAC 0x21
   --  unsupported macro: VIDEO_TYPE_VGAC 0x22
   --  unsupported macro: VIDEO_TYPE_VLFB 0x23
   --  unsupported macro: VIDEO_TYPE_PICA_S3 0x30
   --  unsupported macro: VIDEO_TYPE_MIPS_G364 0x31
   --  unsupported macro: VIDEO_TYPE_SGI 0x33
   --  unsupported macro: VIDEO_TYPE_TGAC 0x40
   --  unsupported macro: VIDEO_TYPE_SUN 0x50
   --  unsupported macro: VIDEO_TYPE_SUNPCI 0x51
   --  unsupported macro: VIDEO_TYPE_PMAC 0x60
   --  unsupported macro: VIDEO_TYPE_EFI 0x70
   --  unsupported macro: VIDEO_FLAGS_NOCURSOR (1 << 0)
   --  unsupported macro: VIDEO_CAPABILITY_SKIP_QUIRKS (1 << 0)
   type screen_info_u_reserved_array is array (0 .. 5) of aliased asm_generic_int_ll64_h.uu_u8;
   type screen_info is record
      orig_x : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:11
      orig_y : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:12
      ext_mem_k : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:13
      orig_video_page : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:14
      orig_video_mode : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:15
      orig_video_cols : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:16
      flags : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:17
      unused2 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:18
      orig_video_ega_bx : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:19
      unused3 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:20
      orig_video_lines : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:21
      orig_video_isVGA : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:22
      orig_video_points : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:23
      lfb_width : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:26
      lfb_height : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:27
      lfb_depth : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:28
      lfb_base : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/screen_info.h:29
      lfb_size : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/screen_info.h:30
      cl_magic : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:31
      cl_offset : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:31
      lfb_linelength : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:32
      red_size : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:33
      red_pos : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:34
      green_size : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:35
      green_pos : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:36
      blue_size : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:37
      blue_pos : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:38
      rsvd_size : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:39
      rsvd_pos : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/screen_info.h:40
      vesapm_seg : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:41
      vesapm_off : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:42
      pages : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:43
      vesa_attributes : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/screen_info.h:44
      capabilities : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/screen_info.h:45
      u_reserved : aliased screen_info_u_reserved_array;  -- /usr/include/linux/screen_info.h:46
   end record;
   pragma Convention (C_Pass_By_Copy, screen_info);  -- /usr/include/linux/screen_info.h:10

end linux_screen_info_h;
