with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_edd_h is

   --  unsupported macro: EDDNR 0x1e9
   --  unsupported macro: EDDBUF 0xd00
   --  unsupported macro: EDDMAXNR 6
   --  unsupported macro: EDDEXTSIZE 8
   --  unsupported macro: EDDPARMSIZE 74
   --  unsupported macro: CHECKEXTENSIONSPRESENT 0x41
   --  unsupported macro: GETDEVICEPARAMETERS 0x48
   --  unsupported macro: LEGACYGETDEVICEPARAMETERS 0x08
   --  unsupported macro: EDDMAGIC1 0x55AA
   --  unsupported macro: EDDMAGIC2 0xAA55
   --  unsupported macro: READ_SECTORS 0x02
   --  unsupported macro: EDD_MBR_SIG_OFFSET 0x1B8
   --  unsupported macro: EDD_MBR_SIG_BUF 0x290
   --  unsupported macro: EDD_MBR_SIG_MAX 16
   --  unsupported macro: EDD_MBR_SIG_NR_BUF 0x1ea
   --  unsupported macro: EDD_EXT_FIXED_DISK_ACCESS (1 << 0)
   --  unsupported macro: EDD_EXT_DEVICE_LOCKING_AND_EJECTING (1 << 1)
   --  unsupported macro: EDD_EXT_ENHANCED_DISK_DRIVE_SUPPORT (1 << 2)
   --  unsupported macro: EDD_EXT_64BIT_EXTENSIONS (1 << 3)
   --  unsupported macro: EDD_INFO_DMA_BOUNDARY_ERROR_TRANSPARENT (1 << 0)
   --  unsupported macro: EDD_INFO_GEOMETRY_VALID (1 << 1)
   --  unsupported macro: EDD_INFO_REMOVABLE (1 << 2)
   --  unsupported macro: EDD_INFO_WRITE_VERIFY (1 << 3)
   --  unsupported macro: EDD_INFO_MEDIA_CHANGE_NOTIFICATION (1 << 4)
   --  unsupported macro: EDD_INFO_LOCKABLE (1 << 5)
   --  unsupported macro: EDD_INFO_NO_MEDIA_PRESENT (1 << 6)
   --  unsupported macro: EDD_INFO_USE_INT13_FN50 (1 << 7)
   type edd_device_params_host_bus_type_array is array (0 .. 3) of aliased asm_generic_int_ll64_h.uu_u8;
   type edd_device_params_interface_type_array is array (0 .. 7) of aliased asm_generic_int_ll64_h.uu_u8;
   type anon_3 is record
      base_address : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:88
      reserved1 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:89
      reserved2 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, anon_3);
   type anon_4 is record
      bus : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:93
      slot : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:94
      c_function : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:95
      channel : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:96
      reserved : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:97
   end record;
   pragma Convention (C_Pass_By_Copy, anon_4);
   type anon_5 is record
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:101
   end record;
   pragma Convention (C_Pass_By_Copy, anon_5);
   type anon_6 is record
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:104
   end record;
   pragma Convention (C_Pass_By_Copy, anon_6);
   type anon_7 is record
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:107
   end record;
   pragma Convention (C_Pass_By_Copy, anon_7);
   type anon_8 is record
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:110
   end record;
   pragma Convention (C_Pass_By_Copy, anon_8);
   type anon_10 is record
      device : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:115
      reserved1 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:116
      reserved2 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:117
      reserved3 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:118
      reserved4 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:119
   end record;
   pragma Convention (C_Pass_By_Copy, anon_10);
   type anon_11 is record
      device : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:122
      lun : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:123
      reserved1 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:124
      reserved2 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:125
      reserved3 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:126
      reserved4 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:127
   end record;
   pragma Convention (C_Pass_By_Copy, anon_11);
   type anon_12 is record
      id : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:130
      lun : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:131
      reserved1 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:132
      reserved2 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:133
   end record;
   pragma Convention (C_Pass_By_Copy, anon_12);
   type anon_13 is record
      serial_number : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:136
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:137
   end record;
   pragma Convention (C_Pass_By_Copy, anon_13);
   type anon_14 is record
      eui : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:140
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:141
   end record;
   pragma Convention (C_Pass_By_Copy, anon_14);
   type anon_15 is record
      wwid : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:144
      lun : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, anon_15);
   type anon_16 is record
      identity_tag : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:148
      reserved : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:149
   end record;
   pragma Convention (C_Pass_By_Copy, anon_16);
   type anon_17 is record
      array_number : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:152
      reserved1 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:153
      reserved2 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:154
   end record;
   pragma Convention (C_Pass_By_Copy, anon_17);
   type anon_18 is record
      device : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:157
      reserved1 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:158
      reserved2 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:159
      reserved3 : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:160
      reserved4 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:161
   end record;
   pragma Convention (C_Pass_By_Copy, anon_18);
   type anon_19 is record
      reserved1 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:164
      reserved2 : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:165
   end record;
   pragma Convention (C_Pass_By_Copy, anon_19);
   type edd_device_params is record
      length : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:72
      info_flags : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:73
      num_default_cylinders : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:74
      num_default_heads : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:75
      sectors_per_track : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:76
      number_of_sectors : aliased asm_generic_int_ll64_h.uu_u64;  -- /usr/include/linux/edd.h:77
      bytes_per_sector : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:78
      dpte_ptr : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/edd.h:79
      key : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:80
      device_path_info_length : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:81
      reserved2 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:82
      reserved3 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:83
      host_bus_type : aliased edd_device_params_host_bus_type_array;  -- /usr/include/linux/edd.h:84
      interface_type : aliased edd_device_params_interface_type_array;  -- /usr/include/linux/edd.h:85
      interface_path : anon_3;  -- /usr/include/linux/edd.h:112
      device_path : anon_10;  -- /usr/include/linux/edd.h:167
      reserved4 : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:168
      checksum : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:169
   end record;
   pragma Convention (C_Pass_By_Copy, edd_device_params);  -- /usr/include/linux/edd.h:71

   type edd_info is record
      device : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:173
      version : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:174
      interface_support : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:175
      legacy_max_cylinder : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/edd.h:176
      legacy_max_head : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:177
      legacy_sectors_per_track : aliased asm_generic_int_ll64_h.uu_u8;  -- /usr/include/linux/edd.h:178
      params : aliased edd_device_params;  -- /usr/include/linux/edd.h:179
   end record;
   pragma Convention (C_Pass_By_Copy, edd_info);  -- /usr/include/linux/edd.h:172

   type edd_mbr_signature_array is array (0 .. 15) of aliased unsigned;
   type edd_edd_info_array is array (0 .. 5) of aliased edd_info;
   type edd is record
      mbr_signature : aliased edd_mbr_signature_array;  -- /usr/include/linux/edd.h:183
      edd_info : aliased edd_edd_info_array;  -- /usr/include/linux/edd.h:184
      mbr_signature_nr : aliased unsigned_char;  -- /usr/include/linux/edd.h:185
      edd_info_nr : aliased unsigned_char;  -- /usr/include/linux/edd.h:186
   end record;
   pragma Convention (C_Pass_By_Copy, edd);  -- /usr/include/linux/edd.h:182

end linux_edd_h;
