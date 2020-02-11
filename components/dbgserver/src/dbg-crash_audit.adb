--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with System;

with Interfaces;

with SK.Strings;
with SK.Crash_Audit_Types;
with SK.Dumper;

with Dbgserver_Component.Memory;

with Dbg.Channels;
with Dbg.Byte_Queue.Format;

package body Dbg.Crash_Audit
with SPARK_Mode => Off
is

   use SK.Strings;

   package Cspecs renames Dbgserver_Component.Memory;

   pragma Warnings
     (GNAT, Off, "* bits of ""Instance"" unused",
      Reason => "We only care if the region is too small");
   Instance : SK.Crash_Audit_Types.Dump_Type
   with
      Volatile,
      Import,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Cspecs.Crash_Audit_Address),
      Size    => Cspecs.Crash_Audit_Size * 8;
   pragma Warnings (GNAT, On, "* bits of ""Instance"" unused");

   --  Append line to output of all debug interfaces.
   procedure Append_Line (Item : String);

   --  Append string to output of all debug interfaces.
   procedure Append_String (Item : String);

   --  Append version of crashing kernel to output of all debug interfaces.
   procedure Append_Version (Item : SK.Crash_Audit_Types.Version_String_Type);

   --  Append init context to output of all debug interfaces.
   procedure Append_Init_Context
     (Ctx : SK.Crash_Audit_Types.Init_Context_Type);

   --  Append new line to output of all debug interfaces.
   procedure New_Line;

   package D is new SK.Dumper
     (New_Line   => New_Line,
      Put_Line   => Append_Line,
      Put_String => Append_String);

   -------------------------------------------------------------------------

   procedure Append_Init_Context
     (Ctx : SK.Crash_Audit_Types.Init_Context_Type)
   is
      --  Append status item.
      procedure Append_Item
        (Queue  : in out Byte_Queue.Queue_Type;
         Str    :        String;
         Status :        Boolean);

      ----------------------------------------------------------------------

      procedure Append_Item
        (Queue  : in out Byte_Queue.Queue_Type;
         Str    :        String;
         Status :        Boolean)
      is
      begin
         Byte_Queue.Format.Append_String
           (Queue => Queue,
            Item  => Str);
         Byte_Queue.Format.Append_Bool
           (Queue => Queue,
            Item  => Status);
         Byte_Queue.Format.Append_New_Line (Queue => Queue);
      end Append_Item;
   begin
      for Iface of Channels.Instance loop
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "= System Context");
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- VMX support              : ",
                      Status => Ctx.Sys_Ctx.VMX_Support);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- VMX not disabled locked  : ",
                      Status => Ctx.Sys_Ctx.Not_VMX_Disabled_Locked);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- IA32e mode enabled       : ",
                      Status => Ctx.Sys_Ctx.IA_32e_Mode);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Support for local x2APIC : ",
                      Status => Ctx.Sys_Ctx.Apic_Support);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- CR0 register valid       : ",
                      Status => Ctx.Sys_Ctx.CR0_Valid);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- CR4 register valid       : ",
                      Status => Ctx.Sys_Ctx.CR4_Valid);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Not in virtual-8086 mode : ",
                      Status => Ctx.Sys_Ctx.Not_Virtual_8086);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Invariant TSC support    : ",
                      Status => Ctx.Sys_Ctx.Invariant_TSC);
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "= FPU Context");
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- XSAVE support            : ",
                      Status => Ctx.FPU_Ctx.XSAVE_Support);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Area size OK             : ",
                      Status => Ctx.FPU_Ctx.Area_Size);
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "= MCE/MCA Context");
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Support for MCE          : ",
                      Status => Ctx.MCE_Ctx.MCE_Support);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Support for MCA          : ",
                      Status => Ctx.MCE_Ctx.MCA_Support);
         Append_Item (Queue  => Iface.Output,
                      Str    => "- Bank count OK            : ",
                      Status => Ctx.MCE_Ctx.Bank_Count_OK);
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "= VT-d Context");
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "- IOMMU count              : ");
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => Img_Nobase (Item => Ctx.VTd_Ctx.IOMMU_Count));
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);

         for I in 1 .. Integer (Ctx.VTd_Ctx.IOMMU_Count) loop
            Byte_Queue.Format.Append_String
              (Queue => Iface.Output,
               Item  => "- IOMMU                    : ");
            Byte_Queue.Format.Append_String
              (Queue => Iface.Output,
               Item  => Img_Nobase (Item => SK.Byte (I)));
            Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- Version support          : ",
                         Status => Ctx.VTd_Ctx.Status (I).Version_Support);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- Supported domain count   : ",
                         Status => Ctx.VTd_Ctx.Status (I).Nr_Domains_OK);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- AGAW support             : ",
                         Status => Ctx.VTd_Ctx.Status (I).AGAW_Support);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- IR support               : ",
                         Status => Ctx.VTd_Ctx.Status (I).IR_Support);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- EIM support              : ",
                         Status => Ctx.VTd_Ctx.Status (I).EIM_Support);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- NFR match                : ",
                         Status => Ctx.VTd_Ctx.Status (I).NFR_Match);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- FR offset match          : ",
                         Status => Ctx.VTd_Ctx.Status (I).FR_Offset_Match);
            Append_Item (Queue  => Iface.Output,
                         Str    => "- IOTLB inv. offset match  : ",
                         Status => Ctx.VTd_Ctx.Status
                           (I).IOTLB_Inv_Offset_Match);
         end loop;
      end loop;
   end Append_Init_Context;

   -------------------------------------------------------------------------

   procedure Append_Line (Item : String)
   is
   begin
      for Iface of Channels.Instance loop
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => Item);
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
      end loop;
   end Append_Line;

   -------------------------------------------------------------------------

   procedure Append_String (Item : String)
   is
   begin
      for Iface of Channels.Instance loop
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => Item);
      end loop;
   end Append_String;

   -------------------------------------------------------------------------

   procedure Append_Version (Item : SK.Crash_Audit_Types.Version_String_Type)
   is
   begin
      for Iface of Channels.Instance loop
         Byte_Queue.Format.Append_String
           (Queue => Iface.Output,
            Item  => "Kernel Version : ");
         for Char of Item loop
            exit when Char = ASCII.NUL;
            Byte_Queue.Format.Append_Character
              (Queue => Iface.Output,
               Item  => Char);
         end loop;
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
      end loop;
   end Append_Version;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      for Iface of Channels.Instance loop
         Byte_Queue.Format.Append_New_Line (Queue => Iface.Output);
      end loop;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Process
   is
      package IFA renames Interfaces;

      use type Interfaces.Unsigned_64;
   begin
      if Instance.Header.Version_Magic = SK.Crash_Audit_Types.Crash_Magic
        and then Instance.Header.Boot_Count = Instance.Header.Generation
      then
         New_Line;
         Append_Line
           (Item => "[Active CRASH AUDIT detected @ "
            & Img (IFA.Unsigned_64'(Cspecs.Crash_Audit_Address)) & "]");
         Append_Line (Item => "Records        : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Dump_Count)));
         Append_Line (Item => "Boot Count     : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Boot_Count)));
         Append_Line (Item => "Crash Count    : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Crash_Count)));
         declare
            Version_Str : constant SK.Crash_Audit_Types.Version_String_Type
              := Instance.Header.Version_String;
         begin
            Append_Version (Item => Version_Str);
         end;

         for I in 1 .. Instance.Header.Dump_Count loop
            New_Line;
            Append_Line
              (Item => "* Record " & Img (IFA.Unsigned_8 (I))
               & ", APIC ID " & Img (Instance.Data (I).APIC_ID)
               & " @ TSC " & Img (Instance.Data (I).TSC_Value)
               & " - Reason : " & Img (IFA.Unsigned_64
                 (Instance.Data (I).Reason)));
            if Instance.Data (I).Field_Validity.Ex_Context then
               declare
                  Ex_Ctx : constant SK.Crash_Audit_Types.Exception_Context_Type
                    := Instance.Data (I).Exception_Context;
               begin
                  D.Output_ISR_State
                    (Context => Ex_Ctx,
                     APIC_ID => Instance.Data (I).APIC_ID);
               end;
            end if;
            if Instance.Data (I).Field_Validity.MCE_Context then
               declare
                  MCE_Ctx : constant SK.Crash_Audit_Types.MCE_Context_Type
                    := Instance.Data (I).MCE_Context;
               begin
                  D.Output_MCE_State (Context => MCE_Ctx);
               end;
            end if;
            if Instance.Data (I).Field_Validity.Subj_Context then
               declare
                  Subj_Ctx : constant SK.Crash_Audit_Types.Subj_Context_Type
                    := Instance.Data (I).Subject_Context;
               begin
                  D.Output_Subj_State (Context => Subj_Ctx);
               end;
            end if;
            if Instance.Data (I).Field_Validity.Init_Context then
               declare
                  Init_Ctx : constant SK.Crash_Audit_Types.Init_Context_Type
                    := Instance.Data (I).Init_Context;
               begin
                  Append_Init_Context (Ctx => Init_Ctx);
               end;
            end if;
            if Instance.Data (I).Field_Validity.VTx_Context then
               declare
                  VTx_Ctx : constant SK.Crash_Audit_Types.VTx_Context_Type
                    := Instance.Data (I).VTx_Context;
               begin
                  D.Output_VMX_Error
                    (Reason  => Instance.Data (I).Reason,
                     Context => VTx_Ctx);
               end;
            end if;
         end loop;
      end if;
   end Process;

end Dbg.Crash_Audit;
