with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;
with i386_linux_gnu_asm_e820_h;
with linux_edd_h;
with linux_screen_info_h;
with linux_apm_bios_h;
with i386_linux_gnu_asm_ist_h;
with video_edid_h;

package bootparam_h is

   --  unsupported macro: SETUP_NONE 0
   --  unsupported macro: SETUP_E820_EXT 1
   --  unsupported macro: SETUP_DTB 2
   --  unsupported macro: SETUP_PCI 3
   --  unsupported macro: RAMDISK_IMAGE_START_MASK 0x07FF
   --  unsupported macro: RAMDISK_PROMPT_FLAG 0x8000
   --  unsupported macro: RAMDISK_LOAD_FLAG 0x4000
   --  unsupported macro: LOADED_HIGH (1<<0)
   --  unsupported macro: QUIET_FLAG (1<<5)
   --  unsupported macro: KEEP_SEGMENTS (1<<6)
   --  unsupported macro: CAN_USE_HEAP (1<<7)
   --  unsupported macro: XLF_KERNEL_64 (1<<0)
   --  unsupported macro: XLF_CAN_BE_LOADED_ABOVE_4G (1<<1)
   --  unsupported macro: XLF_EFI_HANDOVER_32 (1<<2)
   --  unsupported macro: XLF_EFI_HANDOVER_64 (1<<3)
   type setup_data_data_array is array (0 .. -1) of aliased asm_generic_int_ll64_h.uu_u8;
   type setup_data is record
      next : aliased asm_generic_int_ll64_h.uu_u64;  -- bootparam.h:39
      c_type : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:40
      len : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:41
      data : aliased setup_data_data_array;  -- bootparam.h:42
   end record;
   pragma Convention (C_Pass_By_Copy, setup_data);  -- bootparam.h:38

   type setup_header is record
      setup_sects : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:46
      root_flags : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:47
      syssize : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:48
      ram_size : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:49
      vid_mode : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:50
      root_dev : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:51
      boot_flag : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:52
      jump : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:53
      header : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:54
      version : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:55
      realmode_swtch : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:56
      start_sys : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:57
      kernel_version : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:58
      type_of_loader : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:59
      loadflags : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:60
      setup_move_size : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:61
      code32_start : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:62
      ramdisk_image : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:63
      ramdisk_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:64
      bootsect_kludge : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:65
      heap_end_ptr : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:66
      ext_loader_ver : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:67
      ext_loader_type : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:68
      cmd_line_ptr : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:69
      initrd_addr_max : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:70
      kernel_alignment : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:71
      relocatable_kernel : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:72
      min_alignment : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:73
      xloadflags : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:74
      cmdline_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:75
      hardware_subarch : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:76
      hardware_subarch_data : aliased asm_generic_int_ll64_h.uu_u64;  -- bootparam.h:77
      payload_offset : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:78
      payload_length : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:79
      setup_data : aliased asm_generic_int_ll64_h.uu_u64;  -- bootparam.h:80
      pref_address : aliased asm_generic_int_ll64_h.uu_u64;  -- bootparam.h:81
      init_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:82
      handover_offset : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:83
   end record;
   pragma Convention (C_Pass_By_Copy, setup_header);  -- bootparam.h:45

   type sys_desc_table_table_array is array (0 .. 13) of aliased asm_generic_int_ll64_h.uu_u8;
   type sys_desc_table is record
      length : aliased asm_generic_int_ll64_h.uu_u16;  -- bootparam.h:87
      table : aliased sys_desc_table_table_array;  -- bootparam.h:88
   end record;
   pragma Convention (C_Pass_By_Copy, sys_desc_table);  -- bootparam.h:86

   type olpc_ofw_header is record
      ofw_magic : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:93
      ofw_version : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:94
      cif_handler : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:95
      irq_desc_table : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:96
   end record;
   pragma Convention (C_Pass_By_Copy, olpc_ofw_header);  -- bootparam.h:92

   type efi_info is record
      efi_loader_signature : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:100
      efi_systab : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:101
      efi_memdesc_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:102
      efi_memdesc_version : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:103
      efi_memmap : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:104
      efi_memmap_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:105
      efi_systab_hi : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:106
      efi_memmap_hi : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:107
   end record;
   pragma Convention (C_Pass_By_Copy, efi_info);  -- bootparam.h:99

   type boot_params_u_pad2_array is array (0 .. 3) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_u_pad3_array is array (0 .. 15) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_hd0_info_array is array (0 .. 15) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_hd1_info_array is array (0 .. 15) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_u_pad4_array is array (0 .. 115) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_u_pad5_array is array (0 .. 2) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_u_pad6_array is array (0 .. 0) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_u_pad7_array is array (0 .. 39) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_edd_mbr_sig_buffer_array is array (0 .. 15) of aliased asm_generic_int_ll64_h.uu_u32;
   type boot_params_e820_map_array is array (0 .. 127) of aliased i386_linux_gnu_asm_e820_h.e820entry;
   type boot_params_u_pad8_array is array (0 .. 47) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params_eddbuf_array is array (0 .. 5) of aliased linux_edd_h.edd_info;
   type boot_params_u_pad9_array is array (0 .. 275) of aliased asm_generic_int_ll64_h.uu_u8;
   type boot_params is record
      the_screen_info : aliased linux_screen_info_h.screen_info;  -- bootparam.h:112
      the_apm_bios_info : aliased linux_apm_bios_h.apm_bios_info;  -- bootparam.h:113
      u_pad2 : aliased boot_params_u_pad2_array;  -- bootparam.h:114
      tboot_addr : aliased asm_generic_int_ll64_h.uu_u64;  -- bootparam.h:115
      the_ist_info : aliased i386_linux_gnu_asm_ist_h.ist_info;  -- bootparam.h:116
      u_pad3 : aliased boot_params_u_pad3_array;  -- bootparam.h:117
      hd0_info : aliased boot_params_hd0_info_array;  -- bootparam.h:118
      hd1_info : aliased boot_params_hd1_info_array;  -- bootparam.h:119
      the_sys_desc_table : aliased sys_desc_table;  -- bootparam.h:120
      the_olpc_ofw_header : aliased olpc_ofw_header;  -- bootparam.h:121
      ext_ramdisk_image : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:122
      ext_ramdisk_size : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:123
      ext_cmd_line_ptr : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:124
      u_pad4 : aliased boot_params_u_pad4_array;  -- bootparam.h:125
      the_edid_info : aliased video_edid_h.edid_info;  -- bootparam.h:126
      the_efi_info : aliased efi_info;  -- bootparam.h:127
      alt_mem_k : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:128
      scratch : aliased asm_generic_int_ll64_h.uu_u32;  -- bootparam.h:129
      e820_entries : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:130
      eddbuf_entries : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:131
      edd_mbr_sig_buf_entries : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:132
      kbd_status : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:133
      u_pad5 : aliased boot_params_u_pad5_array;  -- bootparam.h:134
      sentinel : aliased asm_generic_int_ll64_h.uu_u8;  -- bootparam.h:146
      u_pad6 : aliased boot_params_u_pad6_array;  -- bootparam.h:147
      hdr : aliased setup_header;  -- bootparam.h:148
      u_pad7 : aliased boot_params_u_pad7_array;  -- bootparam.h:149
      edd_mbr_sig_buffer : aliased boot_params_edd_mbr_sig_buffer_array;  -- bootparam.h:150
      e820_map : aliased boot_params_e820_map_array;  -- bootparam.h:151
      u_pad8 : aliased boot_params_u_pad8_array;  -- bootparam.h:152
      eddbuf : aliased boot_params_eddbuf_array;  -- bootparam.h:153
      u_pad9 : aliased boot_params_u_pad9_array;  -- bootparam.h:154
   end record;
   pragma Convention (C_Pass_By_Copy, boot_params);  -- bootparam.h:111

end bootparam_h;
