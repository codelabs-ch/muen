with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package linux_apm_bios_h is

   --  unsupported macro: APM_STATE_READY 0x0000
   --  unsupported macro: APM_STATE_STANDBY 0x0001
   --  unsupported macro: APM_STATE_SUSPEND 0x0002
   --  unsupported macro: APM_STATE_OFF 0x0003
   --  unsupported macro: APM_STATE_BUSY 0x0004
   --  unsupported macro: APM_STATE_REJECT 0x0005
   --  unsupported macro: APM_STATE_OEM_SYS 0x0020
   --  unsupported macro: APM_STATE_OEM_DEV 0x0040
   --  unsupported macro: APM_STATE_DISABLE 0x0000
   --  unsupported macro: APM_STATE_ENABLE 0x0001
   --  unsupported macro: APM_STATE_DISENGAGE 0x0000
   --  unsupported macro: APM_STATE_ENGAGE 0x0001
   --  unsupported macro: APM_SYS_STANDBY 0x0001
   --  unsupported macro: APM_SYS_SUSPEND 0x0002
   --  unsupported macro: APM_NORMAL_RESUME 0x0003
   --  unsupported macro: APM_CRITICAL_RESUME 0x0004
   --  unsupported macro: APM_LOW_BATTERY 0x0005
   --  unsupported macro: APM_POWER_STATUS_CHANGE 0x0006
   --  unsupported macro: APM_UPDATE_TIME 0x0007
   --  unsupported macro: APM_CRITICAL_SUSPEND 0x0008
   --  unsupported macro: APM_USER_STANDBY 0x0009
   --  unsupported macro: APM_USER_SUSPEND 0x000a
   --  unsupported macro: APM_STANDBY_RESUME 0x000b
   --  unsupported macro: APM_CAPABILITY_CHANGE 0x000c
   --  unsupported macro: APM_SUCCESS 0x00
   --  unsupported macro: APM_DISABLED 0x01
   --  unsupported macro: APM_CONNECTED 0x02
   --  unsupported macro: APM_NOT_CONNECTED 0x03
   --  unsupported macro: APM_16_CONNECTED 0x05
   --  unsupported macro: APM_16_UNSUPPORTED 0x06
   --  unsupported macro: APM_32_CONNECTED 0x07
   --  unsupported macro: APM_32_UNSUPPORTED 0x08
   --  unsupported macro: APM_BAD_DEVICE 0x09
   --  unsupported macro: APM_BAD_PARAM 0x0a
   --  unsupported macro: APM_NOT_ENGAGED 0x0b
   --  unsupported macro: APM_BAD_FUNCTION 0x0c
   --  unsupported macro: APM_RESUME_DISABLED 0x0d
   --  unsupported macro: APM_NO_ERROR 0x53
   --  unsupported macro: APM_BAD_STATE 0x60
   --  unsupported macro: APM_NO_EVENTS 0x80
   --  unsupported macro: APM_NOT_PRESENT 0x86
   --  unsupported macro: APM_DEVICE_BIOS 0x0000
   --  unsupported macro: APM_DEVICE_ALL 0x0001
   --  unsupported macro: APM_DEVICE_DISPLAY 0x0100
   --  unsupported macro: APM_DEVICE_STORAGE 0x0200
   --  unsupported macro: APM_DEVICE_PARALLEL 0x0300
   --  unsupported macro: APM_DEVICE_SERIAL 0x0400
   --  unsupported macro: APM_DEVICE_NETWORK 0x0500
   --  unsupported macro: APM_DEVICE_PCMCIA 0x0600
   --  unsupported macro: APM_DEVICE_BATTERY 0x8000
   --  unsupported macro: APM_DEVICE_OEM 0xe000
   --  unsupported macro: APM_DEVICE_OLD_ALL 0xffff
   --  unsupported macro: APM_DEVICE_CLASS 0x00ff
   --  unsupported macro: APM_DEVICE_MASK 0xff00
   --  unsupported macro: APM_MAX_BATTERIES 2
   --  unsupported macro: APM_CAP_GLOBAL_STANDBY 0x0001
   --  unsupported macro: APM_CAP_GLOBAL_SUSPEND 0x0002
   --  unsupported macro: APM_CAP_RESUME_STANDBY_TIMER 0x0004
   --  unsupported macro: APM_CAP_RESUME_SUSPEND_TIMER 0x0008
   --  unsupported macro: APM_CAP_RESUME_STANDBY_RING 0x0010
   --  unsupported macro: APM_CAP_RESUME_SUSPEND_RING 0x0020
   --  unsupported macro: APM_CAP_RESUME_STANDBY_PCMCIA 0x0040
   --  unsupported macro: APM_CAP_RESUME_SUSPEND_PCMCIA 0x0080
   --  unsupported macro: APM_IOC_STANDBY _IO('A', 1)
   --  unsupported macro: APM_IOC_SUSPEND _IO('A', 2)
   subtype apm_event_t is unsigned_short;  -- /usr/include/linux/apm_bios.h:21

   subtype apm_eventinfo_t is unsigned_short;  -- /usr/include/linux/apm_bios.h:22

   type apm_bios_info is record
      version : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:25
      cseg : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:26
      offset : aliased asm_generic_int_ll64_h.uu_u32;  -- /usr/include/linux/apm_bios.h:27
      cseg_16 : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:28
      dseg : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:29
      flags : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:30
      cseg_len : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:31
      cseg_16_len : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:32
      dseg_len : aliased asm_generic_int_ll64_h.uu_u16;  -- /usr/include/linux/apm_bios.h:33
   end record;
   pragma Convention (C_Pass_By_Copy, apm_bios_info);  -- /usr/include/linux/apm_bios.h:24

end linux_apm_bios_h;
