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
     (Output_New_Line   => New_Line,
      Output_Put_Line   => Append_Line,
      Output_Put_String => Append_String);

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
                      Status => Ctx.Sys_Ctx.VMX_Not_Dislocked);
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
            & Img (IFA.Unsigned_64' (Cspecs.Crash_Audit_Address)) & "]");
         Append_Line (Item => "Records        : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Dump_Count)));
         Append_Line (Item => "Boot Count     : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Boot_Count)));
         Append_Line (Item => "Crash Count    : "
                      & Img (IFA.Unsigned_8 (Instance.Header.Crash_Count)));
         Append_Version (Item => Instance.Header.Version_String);

         for I in 1 .. Instance.Header.Dump_Count loop
            New_Line;
            Append_Line
              (Item => "* Record " & Img (IFA.Unsigned_8 (I))
               & ", APIC ID " & Img (Instance.Data (I).APIC_ID)
               & " @ TSC " & Img (Instance.Data (I).TSC_Value)
               & " - Reason : " & Img (IFA.Unsigned_64
                 (Instance.Data (I).Reason)));
            case Instance.Data (I).Reason is
               when SK.Crash_Audit_Types.Hardware_Exception =>
                  if Instance.Data (I).Field_Validity.Ex_Context then
                     D.Output_ISR_State
                       (Context => Instance.Data (I).Exception_Context,
                        APIC_ID => Instance.Data (I).APIC_ID);
                  else
                     Append_Line (Item => "!!! ISR context not valid");
                  end if;
               when SK.Crash_Audit_Types.Subj_Reason_Range =>
                  if Instance.Data (I).Field_Validity.Subj_Context then
                     D.Output_Subj_State
                       (Context => Instance.Data (I).Subject_Context);
                  else
                     Append_Line (Item => "!!! Subject context not valid");
                  end if;
               when SK.Crash_Audit_Types.System_Init_Failure =>
                  if Instance.Data (I).Field_Validity.Init_Context then
                     Append_Init_Context
                       (Ctx => Instance.Data (I).Init_Context);
                  else
                     Append_Line (Item => "!!! Init context not valid");
                  end if;
               when others =>
                  Append_Line (Item => "!!! Unknown crash reason");
            end case;
         end loop;
      end if;
   end Process;

end Dbg.Crash_Audit;
