--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Exceptions;

with SK.Utils;

with Skp.Paging.EPT;
with Skp.IO_Ports;
with Skp.MSRs;
with Skp.Templates;
with Skp.Constants;
with Skp.Writers.Packer_Config;
with Skp.Writers.Zero_Page;

package body Skp.Writers
is

   use Ada.Strings.Unbounded;
   use type SK.Word32;

   Policy_File   : constant String := "policy.h";
   Policy_Topdir : constant String := "policy";

   --  A PDPT entry maps a 1 GB page.
   PDPT_Page_Size : constant := 1 * 2 ** 30;
   --  A PD entry maps a 2 MB page.
   PD_Page_Size   : constant := 2 * 2 ** 20;

   --  Create paging structures from given memory layout and write them to the
   --  specified file. The PML4 address parameter specifies the physical start
   --  address of the PML4 paging structure. Depending on the given profile
   --  IA32e or EPT page tables will be generated.
   procedure Write
     (Mem_Layout   : Memory_Layout_Type;
      Pml4_Address : SK.Word64;
      Filename     : String;
      Profile      : Subject_Profile_Type := Native);

   --  Create I/O bitmap from given ports and write them to the specified file.
   procedure Write
     (Ports    : IO_Ports_Type;
      Filename : String);

   --  Create MSR bitmap from given MSRs and write them to the specified file.
   procedure Write
     (MSR_List : MSRs_Type;
      Filename : String);

   --  Write per-CPU kernel pagetables to specified directory.
   procedure Write_Kernel_Pagetables
     (Dir_Name  : String;
      CPU_Count : Positive;
      Kernel    : Kernel_Type);

   --  Returns ID of CPU which executes the given subject in specified major
   --  frames.
   function Get_Executing_CPU
     (Subject_Id : Natural;
      Majors     : Major_Frames_Type)
      return Natural;

   type Profile_Conf_Type is record
      Exec_Pin     : SK.Word32;
      Exec_Proc    : SK.Word32;
      Exec_Proc2   : SK.Word32;
      Exit_Ctrls   : SK.Word32;
      Entry_Ctrls  : SK.Word32;
      CR0_Value    : SK.Word64;
      CR0_Mask     : SK.Word64;
      CR4_Value    : SK.Word64;
      CR4_Mask     : SK.Word64;
      CS_Access    : SK.Word32;
      Exception_Bm : SK.Word32;
   end record;

   Profile_Mapping : constant array (Subject_Profile_Type) of Profile_Conf_Type
     := (Native =>
           (Exec_Pin     => Constants.VM_CTRL_EXT_INT_EXITING
            or Constants.VM_CTRL_NMI_EXITING
            or Constants.VM_CTRL_PREEMPT_TIMER,
            Exec_Proc    => Constants.VM_CTRL_IO_BITMAPS
            or Constants.VM_CTRL_SECONDARY_PROC
            or Constants.VM_CTRL_EXIT_INVLPG
            or Constants.VM_CTRL_EXIT_MWAIT
            or Constants.VM_CTRL_EXIT_RDPMC
            or Constants.VM_CTRL_EXIT_RDTSC
            or Constants.VM_CTRL_EXIT_CR3_LOAD
            or Constants.VM_CTRL_EXIT_CR3_STORE
            or Constants.VM_CTRL_EXIT_CR8_LOAD
            or Constants.VM_CTRL_EXIT_CR8_STORE
            or Constants.VM_CTRL_EXIT_MOV_DR
            or Constants.VM_CTRL_MSR_BITMAPS
            or Constants.VM_CTRL_EXIT_MONITOR,
            Exec_Proc2   => Constants.VM_CTRL_EXIT_WBINVD,
            Exit_Ctrls   => Constants.VM_CTRL_EXIT_IA32E_MODE
            or Constants.VM_CTRL_EXIT_ACK_INT
            or Constants.VM_CTRL_EXIT_SAVE_TIMER,
            Entry_Ctrls  => Constants.VM_CTRL_ENTR_IA32E_MODE,
            CR0_Value    => 16#8001_0035#,
            CR0_Mask     => 16#ffff_ffff#,
            CR4_Value    => 16#0000_2020#,
            CR4_Mask     => 16#ffff_ffff#,
            CS_Access    => 16#0000_a09b#,
            Exception_Bm => 16#ffff_ffff#),
         Vm     =>
           (Exec_Pin     => Constants.VM_CTRL_EXT_INT_EXITING
            or Constants.VM_CTRL_NMI_EXITING
            or Constants.VM_CTRL_PREEMPT_TIMER,
            Exec_Proc    => Constants.VM_CTRL_IO_BITMAPS
            or Constants.VM_CTRL_SECONDARY_PROC
            or Constants.VM_CTRL_EXIT_INVLPG
            or Constants.VM_CTRL_EXIT_MWAIT
            or Constants.VM_CTRL_EXIT_RDPMC
            or Constants.VM_CTRL_EXIT_RDTSC
            or Constants.VM_CTRL_EXIT_CR8_LOAD
            or Constants.VM_CTRL_EXIT_CR8_STORE
            or Constants.VM_CTRL_EXIT_MOV_DR
            or Constants.VM_CTRL_MSR_BITMAPS
            or Constants.VM_CTRL_EXIT_MONITOR,
            Exec_Proc2   => Constants.VM_CTRL_ENABLE_EPT
            or Constants.VM_CTRL_EXIT_WBINVD
            or Constants.VM_CTRL_UNRESTRICTED,
            Exit_Ctrls   => Constants.VM_CTRL_EXIT_IA32E_MODE
            or Constants.VM_CTRL_EXIT_ACK_INT
            or Constants.VM_CTRL_EXIT_SAVE_EFER
            or Constants.VM_CTRL_EXIT_LOAD_EFER
            or Constants.VM_CTRL_EXIT_SAVE_TIMER,
            Entry_Ctrls  => Constants.VM_CTRL_ENTR_LOAD_EFER,
            CR0_Value    => 16#0000_0035#,
            CR0_Mask     => 16#7ffa_ffc0#,
            CR4_Value    => 16#0000_2000#,
            CR4_Mask     => 16#ffff_ffef#,
            CS_Access    => 16#0000_c09b#,
            Exception_Bm => 16#0000_0000#));

   -------------------------------------------------------------------------

   function Capitalize (Str : String) return String
   is
      Result : String := Ada.Characters.Handling.To_Lower (Item => Str);
   begin
      Result (Result'First) := Ada.Characters.Handling.To_Upper
        (Item => Result (Result'First));
      return Result;
   end Capitalize;

   -------------------------------------------------------------------------

   function Get_Executing_CPU
     (Subject_Id : Natural;
      Majors     : Major_Frames_Type)
      return Natural
   is
      CPU : Integer := -1;

      --  Check major frame.
      procedure Check_Major (Pos : Major_Frames_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Check_Major (Pos : Major_Frames_Package.Cursor)
      is
         Current_CPU : Natural := 0;

         --  Check CPU element.
         procedure Check_CPU (Pos : CPU_Package.Cursor);

         -------------------------------------------------------------------

         procedure Check_CPU (Pos : CPU_Package.Cursor)
         is
            --  Check minor frame.
            procedure Check_Minor (Pos : Minor_Frames_Package.Cursor);

            ----------------------------------------------------------------

            procedure Check_Minor (Pos : Minor_Frames_Package.Cursor)
            is
               Min : constant Minor_Frame_Type := Minor_Frames_Package.Element
                 (Position => Pos);
            begin
               if Min.Subject_Id = Subject_Id then
                  CPU := Current_CPU;
               end if;
            end Check_Minor;
         begin
            CPU_Package.Element (Position => Pos).Iterate
              (Process => Check_Minor'Access);
            Current_CPU := Current_CPU + 1;
         end Check_CPU;
      begin
         Major_Frames_Package.Element (Position => Pos).Iterate
           (Process => Check_CPU'Access);
      end Check_Major;
   begin
      Majors.Iterate (Process => Check_Major'Access);
      return CPU;
   end Get_Executing_CPU;

   -------------------------------------------------------------------------

   function Indent (N : Positive := 1) return String
   is
      Indent : constant String := "   ";
      Result : Unbounded_String;
   begin
      for I in Positive range 1 .. N loop
         Result := Result & Indent;
      end loop;

      return To_String (Result);
   end Indent;

   -------------------------------------------------------------------------

   procedure Open
     (Filename :     String;
      File     : out Ada.Streams.Stream_IO.File_Type)
   is
   begin
      if Ada.Directories.Exists (Name => Filename) then
         Ada.Streams.Stream_IO.Open
           (File => File,
            Mode => Ada.Streams.Stream_IO.Out_File,
            Name => Filename);
      else
         Ada.Streams.Stream_IO.Create
           (File => File,
            Mode => Ada.Streams.Stream_IO.Out_File,
            Name => Filename);
      end if;

   exception
      when E : others =>
         raise IO_Error with "Unable to open file '" & Filename & "' - "
           & Ada.Exceptions.Exception_Message (X => E);
   end Open;

   -------------------------------------------------------------------------

   procedure Write
     (Mem_Layout   : Memory_Layout_Type;
      Pml4_Address : SK.Word64;
      Filename     : String;
      Profile      : Subject_Profile_Type := Native)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
      PML4 : Paging.PML4_Table_Type := Paging.Null_PML4_Table;
      PDPT : Paging.PDP_Table_Type  := Paging.Null_PDP_Table;
      PD   : Paging.PD_Table_Type   := Paging.Null_PD_Table;
      PT   : Paging.Page_Table_Type := Paging.Null_Page_Table;

      --  Add given memory region to pagetable.
      procedure Add_Memory_Region (C : Memregion_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Add_Memory_Region (C : Memregion_Package.Cursor)
      is
         use type SK.Word64;
         use type Paging.PML4_Entry_Type;
         use type Paging.PDPT_Entry_Type;
         use type Paging.PD_Entry_Type;
         use type Paging.PT_Entry_Type;
         use type Paging.Table_Range;

         R : constant Memory_Region_Type := Memregion_Package.Element
           (Position => C);

         PML4_Idx_Start, PML4_Idx_End : Paging.Table_Range;
         PDPT_Idx_Start, PDPT_Idx_End : Paging.Table_Range;
         PD_Idx_Start, PD_Idx_End     : Paging.Table_Range;
         PT_Idx_Start, PT_Idx_End     : Paging.Table_Range;

         --  Physical start address of PDPT paging structure(s).
         PDPT_Addr : SK.Word64;
         --  Physical start address of PD paging structure(s).
         PD_Addr   : SK.Word64;
         --  Physical start address of PT paging structure(s).
         PT_Addr   : SK.Word64;

         Physical_Addr : SK.Word64          := R.Physical_Address;
         Virt_Start    : constant SK.Word64 := R.Virtual_Address;
         Virt_End      : constant SK.Word64 := Virt_Start + R.Size - 1;
         Is_PDPT_Page  : constant Boolean   := R.Alignment = PDPT_Page_Size;
         Is_PD_Page    : constant Boolean   := R.Alignment = PD_Page_Size;
      begin
         Paging.Get_Indexes (Address    => Virt_Start,
                             PML4_Index => PML4_Idx_Start,
                             PDPT_Index => PDPT_Idx_Start,
                             PD_Index   => PD_Idx_Start,
                             PT_Index   => PT_Idx_Start);
         Paging.Get_Indexes (Address    => Virt_End,
                             PML4_Index => PML4_Idx_End,
                             PDPT_Index => PDPT_Idx_End,
                             PD_Index   => PD_Idx_End,
                             PT_Index   => PT_Idx_End);

         PDPT_Addr := Pml4_Address + (SK.Word64 (PML4_Idx_End) + 1)
           * SK.Page_Size;
         PD_Addr   := PDPT_Addr + (SK.Word64 (PDPT_Idx_End) + 1)
           * SK.Page_Size;
         PT_Addr   := PD_Addr + (SK.Word64 (PD_Idx_End) + 1) * SK.Page_Size;

         for Idx in Paging.Table_Range range PML4_Idx_Start .. PML4_Idx_End
         loop
            if PML4 (Idx) = Paging.PML4_Null_Entry then
               case Profile is
                  when Native =>
                     PML4 (Idx) := Paging.Create_PML4_Entry
                       (Address       => PDPT_Addr +
                          SK.Word64 (Idx) * SK.Page_Size,
                        Writable      => True,
                        User_Access   => True,
                        Writethrough  => True,
                        Cache_Disable => False,
                        Exec_Disable  => False);
                  when Vm =>
                     PML4 (Idx) := Paging.EPT.Create_PML4_Entry
                       (Address    => PDPT_Addr +
                          SK.Word64 (Idx) * SK.Page_Size,
                        Readable   => True,
                        Writable   => True,
                        Executable => True);
               end case;
            end if;
         end loop;

         for Idx in Paging.Table_Range range PDPT_Idx_Start .. PDPT_Idx_End
         loop
            if Is_PDPT_Page then
               PD_Addr := R.Physical_Address
                 + SK.Word64 (Idx - PDPT_Idx_Start) * PDPT_Page_Size;
            else
               PD_Addr := PD_Addr
                 + SK.Word64 (Idx - PDPT_Idx_Start) * SK.Page_Size;
            end if;

            if PDPT (Idx) = Paging.PDPT_Null_Entry then
               case Profile is
                  when Native =>
                     PDPT (Idx) := Paging.Create_PDPT_Entry
                       (Address      => PD_Addr,
                        Writable     => not Is_PDPT_Page or R.Writable,
                        User_Access  => True,
                        Map_Page     => Is_PDPT_Page,
                        Global       => False,
                        Memory_Type  => R.Memory_Type,
                        Exec_Disable => Is_PDPT_Page and not R.Executable);
                  when Vm =>
                     PDPT (Idx) := Paging.EPT.Create_PDPT_Entry
                       (Address     => PD_Addr,
                        Readable    => True,
                        Writable    => not Is_PDPT_Page or R.Writable,
                        Executable  => not Is_PDPT_Page or R.Executable,
                        Map_Page    => Is_PDPT_Page,
                        Ignore_PAT  => True,
                        Memory_Type => R.Memory_Type);
               end case;
            end if;
         end loop;

         if Is_PDPT_Page then
            return;
         end if;

         for Idx in Paging.Table_Range range PD_Idx_Start .. PD_Idx_End loop
            if Is_PD_Page then
               PT_Addr := R.Physical_Address
                 + SK.Word64 (Idx - PD_Idx_Start) * PD_Page_Size;
            else
               PT_Addr := PT_Addr
                 + SK.Word64 (Idx - PD_Idx_Start) * SK.Page_Size;
            end if;

            if PD (Idx) = Paging.PD_Null_Entry then
               case Profile is
                  when Native =>
                     PD (Idx) := Paging.Create_PD_Entry
                       (Address      => PT_Addr,
                        Writable     => not Is_PD_Page or R.Writable,
                        User_Access  => True,
                        Map_Page     => Is_PD_Page,
                        Global       => False,
                        Memory_Type  => R.Memory_Type,
                        Exec_Disable => Is_PD_Page and not R.Executable);
                  when Vm =>
                     PD (Idx) := Paging.EPT.Create_PD_Entry
                       (Address     => PT_Addr,
                        Readable    => True,
                        Writable    => not Is_PD_Page or R.Writable,
                        Executable  => not Is_PD_Page or R.Executable,
                        Map_Page    => Is_PD_Page,
                        Ignore_PAT  => True,
                        Memory_Type => R.Memory_Type);
               end case;
            end if;
         end loop;

         if Is_PD_Page then
            return;
         end if;

         for Idx in Paging.Table_Range range PT_Idx_Start .. PT_Idx_End loop
            if PT (Idx) = Paging.PT_Null_Entry then
               case Profile is
                  when Native =>
                     PT (Idx) := Paging.Create_PT_Entry
                       (Address      => Physical_Addr,
                        Writable     => R.Writable,
                        User_Access  => True,
                        Global       => False,
                        Memory_Type  => R.Memory_Type,
                        Exec_Disable => not R.Executable);
                  when Vm =>
                     PT (Idx) := Paging.EPT.Create_PT_Entry
                       (Address     => Physical_Addr,
                        Readable    => True,
                        Writable    => R.Writable,
                        Executable  => R.Executable,
                        Map_Page    => True,
                        Ignore_PAT  => True,
                        Memory_Type => R.Memory_Type);
               end case;
            end if;

            Physical_Addr := Physical_Addr + SK.Page_Size;
         end loop;
      end Add_Memory_Region;
   begin
      Mem_Layout.Iterate (Process => Add_Memory_Region'Access);

      Open (Filename => Filename,
            File     => File);
      Paging.PML4_Table_Type'Write (Stream (File => File), PML4);
      Paging.PDP_Table_Type'Write  (Stream (File => File), PDPT);
      Paging.PD_Table_Type'Write   (Stream (File => File), PD);
      Paging.Page_Table_Type'Write (Stream (File => File), PT);
      Close (File => File);
   end Write;

   -------------------------------------------------------------------------

   procedure Write
     (Ports    : IO_Ports_Type;
      Filename : String)
   is
      File   : Ada.Streams.Stream_IO.File_Type;
      Bitmap : IO_Ports.IO_Bitmap_Type := IO_Ports.Null_IO_Bitmap;

      --  Add given I/O port range to I/O bitmap.
      procedure Add_Port_Range (C : Ports_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Add_Port_Range (C : Ports_Package.Cursor)
      is
         R : constant IO_Port_Range := Ports_Package.Element
           (Position => C);
      begin
         IO_Ports.Allow_Ports
           (B          => Bitmap,
            Start_Port => R.Start_Port,
            End_Port   => R.End_Port);
      end Add_Port_Range;
   begin
      Ports.Iterate (Process => Add_Port_Range'Access);

      Open (Filename => Filename,
            File     => File);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => IO_Ports.To_Stream (B => Bitmap));
      Ada.Streams.Stream_IO.Close (File => File);
   end Write;

   -------------------------------------------------------------------------

   procedure Write
     (MSR_List : MSRs_Type;
      Filename : String)
   is
      File   : Ada.Streams.Stream_IO.File_Type;
      Bitmap : MSRs.MSR_Bitmap_Type := MSRs.Null_MSR_Bitmap;

      --  Add given MSR address range to bitmap.
      procedure Add_MSR (C : MSRs_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Add_MSR (C : MSRs_Package.Cursor)
      is
         M : constant MSR_Type := MSRs_Package.Element (Position => C);
      begin
         MSRs.Allow_MSRs (Bitmap     => Bitmap,
                          Start_Addr => M.Start_Addr,
                          End_Addr   => M.End_Addr,
                          Mode       => M.Mode);
      end Add_MSR;
   begin
      MSR_List.Iterate (Process => Add_MSR'Access);

      Open (Filename => Filename,
            File     => File);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => MSRs.To_Stream (Bitmap => Bitmap));
      Ada.Streams.Stream_IO.Close (File => File);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Hardware
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Buffer : Unbounded_String;
      Tmpl   : Templates.Template_Type;

      --  Write device constants to hardware spec.
      procedure Write_Device (Pos : Devices_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Write_Device (Pos : Devices_Package.Cursor)
      is
         Dev      : constant Device_Type
           := Devices_Package.Element (Position => Pos);
         Port_Idx : Ports_Package.Cursor := Dev.IO_Ports.First;
         Dev_Name : constant String      := Capitalize
           (Str => To_String (Dev.Name));
      begin
         if Dev.IO_Ports.Is_Empty then
            return;
         end if;

         Buffer := Buffer & ASCII.LF;
         for P in 1 .. Dev.IO_Ports.Length loop
            declare
               Port    : constant IO_Port_Range
                 := Ports_Package.Element (Position => Port_Idx);
               Port_Nr : constant String := Ada.Strings.Fixed.Trim
                 (Source => P'Img,
                  Side   => Ada.Strings.Left);
            begin
               Buffer := Buffer & Indent & Dev_Name & "_Port" & Port_Nr
                 & "_Start : constant := 16#"
                 & SK.Utils.To_Hex (Item => Port.Start_Port) & "#;"
                 & ASCII.LF
                 & Indent & Dev_Name & "_Port" & Port_Nr
                 & "_End   : constant := 16#"
                 & SK.Utils.To_Hex (Item => Port.End_Port) & "#;"
                 & ASCII.LF;
            end;

            Ports_Package.Next (Position => Port_Idx);
         end loop;
      end Write_Device;
   begin
      Tmpl := Templates.Load (Filename => "skp-hardware.ads");

      Policy.Hardware.Devices.Iterate (Process => Write_Device'Access);

      Templates.Replace (Template => Tmpl,
                         Pattern  => "__devices__",
                         Content  => To_String (Buffer));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-hardware.ads");
   end Write_Hardware;

   -------------------------------------------------------------------------

   procedure Write_Interrupts
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      --  IRQ to host Vector offset.
      Vector_Offset : constant := 32;

      IRQ_Count, Current        : Natural := 0;
      IRQ_Buffer, Vector_Buffer : Unbounded_String;

      --  Calculate total number configured IRQs.
      procedure Calc_IRQ_Count;

      --  Write device IRQs to interrupts spec.
      procedure Write_Device (Pos : Devices_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Calc_IRQ_Count
      is
         Pos : Devices_Package.Cursor := Policy.Hardware.Devices.First;
         Dev : Device_Type;
      begin
         while Devices_Package.Has_Element (Position => Pos) loop
            Dev := Devices_Package.Element (Position => Pos);
            if Dev.IRQ /= -1 and then not Dev.Owners.Is_Empty then
               IRQ_Count := IRQ_Count + 1;
            end if;
            Devices_Package.Next (Position => Pos);
         end loop;
      end Calc_IRQ_Count;

      ----------------------------------------------------------------------

      procedure Write_Device (Pos : Devices_Package.Cursor)
      is
         CPU : Natural;
         Dev : constant Device_Type := Devices_Package.Element
           (Position => Pos);
      begin
         if Dev.Owners.Is_Empty or else Dev.IRQ = -1 then
            return;
         end if;

         --  Lookup CPU on which subject is scheduled.

         CPU := Get_Executing_CPU
           (Subject_Id => Dev.Owners.First_Element,
            Majors     => Policy.Scheduling.Major_Frames);
         Current := Current + 1;

         --  IRQ routing table.

         IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
           & Current'Img & " => IRQ_Route_Type'("
           & ASCII.LF
           & Indent (N => 3) & "CPU    =>" & CPU'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "IRQ    =>" & Dev.IRQ'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "Vector =>"
           & Positive'Image (Vector_Offset + Dev.IRQ) & ")";

         --  Vector -> subject routing table.

         Vector_Buffer := Vector_Buffer & Indent (N => 2)
           & Positive'Image (Vector_Offset + Dev.IRQ) & " =>"
           & Dev.Owners.First_Element'Img;

         if Current /= IRQ_Count then
            IRQ_Buffer    := IRQ_Buffer    & "," & ASCII.LF;
            Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
         end if;
      end Write_Device;

      Tmpl : Templates.Template_Type;
   begin
      Calc_IRQ_Count;

      Tmpl := Templates.Load (Filename => "skp-interrupts.ads");

      Policy.Hardware.Devices.Iterate (Process => Write_Device'Access);
      Vector_Buffer := Vector_Buffer & ","
        & ASCII.LF & Indent (N => 2) & " others => Skp.Invalid_Subject";

      Templates.Replace (Template => Tmpl,
                         Pattern  => "__routing_range__",
                         Content  => "1 .." & IRQ_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__irq_routing_table__",
                         Content  => To_String (IRQ_Buffer));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vector_routing_table__",
                         Content  => To_String (Vector_Buffer));

      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-interrupts.ads");
   end Write_Interrupts;

   -------------------------------------------------------------------------

   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Tmpl : Templates.Template_Type;

      --  Replace kernel patterns with actual values.
      procedure Replace_Kernel_Patterns;

      ----------------------------------------------------------------------

      procedure Replace_Kernel_Patterns
      is
      begin
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => SK.Utils.To_Hex (Item => Policy.Kernel.Stack_Address));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__kpml4_addr__",
            Content  => SK.Utils.To_Hex (Item => Policy.Kernel.Pml4_Address));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_store_addr__",
            Content  => SK.Utils.To_Hex
              (Item => Policy.Kernel.CPU_Page_Address));
      end Replace_Kernel_Patterns;
   begin
      Tmpl := Templates.Load (Filename => Policy_File);
      Replace_Kernel_Patterns;
      Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subj_count__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Policy.Subjects.Length'Img,
            Side   => Ada.Strings.Left));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/" & Policy_File);

      Tmpl := Templates.Load (Filename => "skp-kernel.ads");
      Replace_Kernel_Patterns;
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-kernel.ads");

      Write_Kernel_Pagetables
        (Dir_Name  => Dir_Name,
         CPU_Count => Policy.Hardware.Processor.Logical_CPUs,
         Kernel    => Policy.Kernel);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Kernel_Pagetables
     (Dir_Name  : String;
      CPU_Count : Positive;
      Kernel    : Kernel_Type)
   is
      use type SK.Word64;

      Pml4_Address : SK.Word64;
   begin
      for I in Natural range 0 .. CPU_Count - 1 loop
         declare
            Phys_Stack_Addr : constant SK.Word64
              := Kernel.Stack_Address - SK.Page_Size
                + SK.Word64 (I) * SK.Page_Size;
            Virt_Stack_Addr : constant SK.Word64
              := Kernel.Stack_Address - SK.Page_Size;
            Mem_Layout      : Memory_Layout_Type := Kernel.Memory_Layout;
            Stack_Page      : constant Memory_Region_Type
              := (Physical_Address => Phys_Stack_Addr,
                  Virtual_Address  => Virt_Stack_Addr,
                  Size             => 16#1000#,
                  Alignment        => 16#1000#,
                  Writable         => True,
                  Executable       => False,
                  Memory_Type      => UC);
            CPU_Page        : constant Memory_Region_Type
              := (Physical_Address => Kernel.CPU_Page_Address
                  + SK.Word64 (I) * SK.Page_Size,
                  Virtual_Address  => Kernel.CPU_Page_Address,
                  Size             => 16#1000#,
                  Alignment        => 16#1000#,
                  Writable         => True,
                  Executable       => False,
                  Memory_Type      => UC);
            CPU_Number      : constant String
              := Ada.Strings.Fixed.Trim (Source => I'Img,
                                         Side   => Ada.Strings.Left);
         begin

            --  Per-CPU stack page.

            Mem_Layout.Insert (Before   => Mem_Layout.First,
                               New_Item => Stack_Page);

            --  Per-CPU global storage page.

            Mem_Layout.Insert (Before   => Mem_Layout.First,
                               New_Item => CPU_Page);

            Pml4_Address := Kernel.Pml4_Address
              + SK.Word64 (I) * (4 * SK.Page_Size);
            Write (Mem_Layout   => Mem_Layout,
                   Pml4_Address => Pml4_Address,
                   Filename     => Dir_Name & "/kernel_pt_" & CPU_Number);
            Packer_Config.Add_File
              (Filename => Policy_Topdir & "/" & Dir_Name & "/kernel_pt_"
               & CPU_Number,
               Address  => Pml4_Address,
               Kind     => Packer_Config.Pagetable);
         end;
      end loop;
   end Write_Kernel_Pagetables;

   -------------------------------------------------------------------------

   procedure Write_Packer_Config
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Format_Map : constant array (Binary_Format) of Packer_Config.File_Kind
        := (Elf => Packer_Config.Elfbinary,
            Raw => Packer_Config.Rawbinary);

      --  Add subject binary to packer config.
      procedure Add_Subject_Bin (C : Subjects_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Add_Subject_Bin (C : Subjects_Package.Cursor)
      is
         S : constant Subject_Type := Subjects_Package.Element (Position => C);
         B : constant Binary_Type  := Policy.Binaries.Element
           (Key => S.Binary.Name);
      begin
         Packer_Config.Add_File
           (Filename => To_String (B.Path),
            Address  => S.Binary.Physical_Address,
            Kind     => Format_Map (B.Format));
      end Add_Subject_Bin;
   begin
      Policy.Subjects.Iterate (Process => Add_Subject_Bin'Access);
      Packer_Config.Write (Dir_Name => Dir_Name);
   end Write_Packer_Config;

   -------------------------------------------------------------------------

   procedure Write_Scheduling
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      CPU_Speed_Hz    : constant Long_Integer
        := Long_Integer (Policy.Hardware.Processor.Speed) * 1_000_000;
      Timer_Rate      : constant Long_Integer
        := 2 ** Policy.Hardware.Processor.VMX_Timer_Rate;
      Timer_Factor    : constant Long_Integer := CPU_Speed_Hz /
        (Timer_Rate * Long_Integer (Policy.Scheduling.Tick_Rate));
      Major_Count     : constant Positive     := Positive
        (Policy.Scheduling.Major_Frames.Length);
      Last_CPU        : constant Natural      :=
        Policy.Hardware.Processor.Logical_CPUs - 1;
      Max_Minor_Count : Positive              := 1;
      Cur_CPU         : Natural               := 0;
      Cur_Major       : Natural;
      Buffer          : Unbounded_String;
      Tmpl            : Templates.Template_Type;

      --  Determine maximum count of minor frames.
      procedure Calc_Max_Minor_Count (C : Major_Frames_Package.Cursor);

      --  Write major frame to buffer.
      procedure Write_Major_Frame (C : Major_Frames_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Calc_Max_Minor_Count (C : Major_Frames_Package.Cursor)
      is
         Major   : constant Major_Frame_Type
           := Major_Frames_Package.Element (Position => C);
         CPU_Pos : CPU_Package.Cursor := Major.First;
         CPU     : CPU_Type;
      begin
         while CPU_Package.Has_Element (Position => CPU_Pos) loop
            CPU := CPU_Package.Element (Position => CPU_Pos);
            if Positive (CPU.Length) > Max_Minor_Count then
               Max_Minor_Count := Positive (CPU.Length);
            end if;
            CPU_Package.Next (Position => CPU_Pos);
         end loop;
      end Calc_Max_Minor_Count;

      ----------------------------------------------------------------------

      procedure Write_Major_Frame (C : Major_Frames_Package.Cursor)
      is
         Major       : constant Major_Frame_Type
           := Major_Frames_Package.Element (C);
         CPU         : constant CPU_Type := Major.Element (Index => Cur_CPU);
         Minor_Count : constant Positive := Positive (CPU.Length);
         Cur_Minor   : Natural           := 1;

         --  Write minor frame to buffer.
         procedure Write_Minor_Frame (C : Minor_Frames_Package.Cursor);

         -------------------------------------------------------------------

         procedure Write_Minor_Frame (C : Minor_Frames_Package.Cursor)
         is
            Ticks : Long_Integer;
            Minor : constant Minor_Frame_Type
              := Minor_Frames_Package.Element (Position => C);
         begin
            Ticks := Long_Integer (Minor.Ticks) * Timer_Factor;
            Buffer := Buffer & Indent (N => 4) & Cur_Minor'Img
              & " => Minor_Frame_Type'(Subject_Id =>" & Minor.Subject_Id'Img
              & ", Ticks =>" & Ticks'Img & ")";

            if Cur_Minor /= Minor_Count then
               Buffer := Buffer & "," & ASCII.LF;
            end if;
            Cur_Minor := Cur_Minor + 1;
         end Write_Minor_Frame;
      begin
         Buffer := Buffer & Indent (N => 2)
           & Cur_Major'Img & " => Major_Frame_Type'"
           & ASCII.LF & Indent (N => 3)
           & "(Length       =>" & CPU.Length'Img & ","
           & ASCII.LF & Indent (N => 3)
           & " Minor_Frames => Minor_Frame_Array'("
           & ASCII.LF;

         CPU.Iterate (Process => Write_Minor_Frame'Access);

         if Positive (CPU.Length) < Max_Minor_Count then
            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & Indent & " others => Null_Minor_Frame";
         end if;

         Buffer := Buffer & "))";

         if Cur_Major /= Major_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
         Cur_Major := Cur_Major + 1;
      end Write_Major_Frame;
   begin
      Policy.Scheduling.Major_Frames.Iterate
        (Process => Calc_Max_Minor_Count'Access);

      for CPU in Natural range 0 .. Last_CPU loop
         Cur_Major := 0;
         Buffer    := Buffer & Indent
           & " " & Cur_CPU'Img & " => Major_Frame_Array'("
           & ASCII.LF;
         Policy.Scheduling.Major_Frames.Iterate
           (Process => Write_Major_Frame'Access);

         Buffer := Buffer & ")";

         if Cur_CPU /= Last_CPU then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
         Cur_CPU := Cur_CPU + 1;
      end loop;

      Tmpl := Templates.Load (Filename  => "skp-scheduling.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__minor_range__",
                         Content  => "1 .." & Max_Minor_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__major_range__",
                         Content  => "0 .." & Natural'Image (Major_Count - 1));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__scheduling_plans__",
                         Content  => To_String (Buffer));

      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-scheduling.ads");
   end Write_Scheduling;

   -------------------------------------------------------------------------

   procedure Write_Subjects
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      S_Count : constant Positive := Positive (Policy.Subjects.Length);
      Current : Natural           := 0;
      Buffer  : Unbounded_String;
      Tmpl    : Templates.Template_Type;

      --  Write subject specs and pagetable.
      procedure Write_Subject (C : Subjects_Package.Cursor);

      --  Write specification of given subject.
      procedure Write_Subject_Spec (Subject : Subject_Type);

      ----------------------------------------------------------------------

      procedure Write_Subject (C : Subjects_Package.Cursor)
      is
         use type SK.Word64;

         S : constant Subject_Type := Subjects_Package.Element (Position => C);

         PT_File  : constant String := Dir_Name & "/" & To_String (S.Name)
           & "_pt";
         IO_File  : constant String := Dir_Name & "/" & To_String (S.Name)
           & "_iobm";
         MSR_File : constant String := Dir_Name & "/" & To_String (S.Name)
           & "_msrbm";
      begin
         Write_Subject_Spec (Subject => S);

         Write (Mem_Layout   => S.Memory_Layout,
                Pml4_Address => S.Pml4_Address,
                Filename     => PT_File,
                Profile      => S.Profile);
         Write (Ports    => S.IO_Ports,
                Filename => IO_File);
         Write (MSR_List => S.MSRs,
                Filename => MSR_File);

         --  Write zero page

         if S.Profile = Vm and then S.ZP_Bitmap_Address > 0 then
            declare
               ZP_File : constant String
                 := Dir_Name & "/" & To_String (S.Name) & "_zp";
            begin
               Zero_Page.Write (Filename => ZP_File);
               Packer_Config.Add_File
                 (Filename => Policy_Topdir & "/" & ZP_File,
                  Address  => S.ZP_Bitmap_Address,
                  Kind     => Packer_Config.Zeropage);
            end;
         end if;

         Packer_Config.Add_File
           (Filename => Policy_Topdir & "/" &  PT_File,
            Address  => S.Pml4_Address,
            Kind     => Packer_Config.Pagetable);
         Packer_Config.Add_File
           (Filename => Policy_Topdir & "/" & IO_File,
            Address  => S.IO_Bitmap_Address,
            Kind     => Packer_Config.Iobitmap);
         Packer_Config.Add_File
           (Filename => Policy_Topdir & "/" & MSR_File,
            Address  => S.MSR_Bitmap_Address,
            Kind     => Packer_Config.Msrbitmap);
      end Write_Subject;

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : Subject_Type)
      is
         use type SK.Word64;
         use type Ada.Containers.Count_Type;

         Cur_Trap : Natural := 0;
         Conf     : constant Profile_Conf_Type
           := Profile_Mapping (Subject.Profile);

         --  EPT memory type WB, page-walk length 4
         EPT_Flags : constant := 16#1e#;

         --  Add event entry to output buffer.
         procedure Add_Event (Pos : Events_Package.Cursor);

         --  Add trap entry to output buffer.
         procedure Add_Trap (Pos : Traps_Package.Cursor);

         -------------------------------------------------------------------

         procedure Add_Event (Pos : Events_Package.Cursor)
         is
            use type Events_Package.Cursor;

            Event : constant Event_Table_Entry_Type
              := Events_Package.Element (Position => Pos);
         begin
            Buffer := Buffer & Indent (N => 3) & Event.Event_Nr'Img
              & " => Event_Entry_Type'("
              & ASCII.LF
              & Indent (N => 4) & "Dst_Subject =>"
              & Get_Id (Subjects => Policy.Subjects,
                        Name     => Event.Dst_Subject)'Img & ","
              & ASCII.LF
              & Indent (N => 4) & "Dst_Vector  =>"
              & Event.Dst_Vector'Img & ","
              & ASCII.LF
              & Indent (N => 4) & "Handover    => "
              & Capitalize (Str => Event.Handover'Img) & ","
              & ASCII.LF
              & Indent (N => 4) & "Send_IPI    => "
              & Capitalize (Str => Event.Send_IPI'Img) & ")";

            if Pos /= Subject.Event_Table.Last then
               Buffer := Buffer & "," & ASCII.LF;
            end if;
         end Add_Event;

         -------------------------------------------------------------------

         procedure Add_Trap (Pos : Traps_Package.Cursor)
         is
            Trap : constant Trap_Table_Entry_Type := Traps_Package.Element
              (Position => Pos);
         begin
            Buffer := Buffer & Indent (N => 3)
              & Trap_Kind'Enum_Rep (Trap.Kind)'Img
              & " => Trap_Entry_Type'(Dst_Subject =>"
              & Get_Id (Subjects => Policy.Subjects,
                        Name     => Trap.Dst_Subject)'Img
              & ", Dst_Vector =>" & Trap.Dst_Vector'Img & ")";

            Cur_Trap := Cur_Trap + 1;
            if Cur_Trap /= Natural (Subject.Trap_Table.Length) then
               Buffer := Buffer & "," & ASCII.LF;
            end if;
         end Add_Trap;

         VMCS_Address : constant SK.Word64
           := Policy.Vmcs_Start_Address + SK.Word64 (Current) * SK.Page_Size;
      begin
         Buffer := Buffer & Indent & "  " & Subject.Id'Img
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    CPU_Id             =>" & Subject.CPU'Img & ","
           & ASCII.LF
           & Indent & "    Profile            => "
           & Capitalize (Str => Subject.Profile'Img) & ","
           & ASCII.LF;

         case Subject.Profile is
            when Native => Buffer := Buffer
                 & Indent & "    PML4_Address       => 16#"
                 & SK.Utils.To_Hex (Item => Subject.Pml4_Address) & "#,"
                 & ASCII.LF
                 & Indent & "    EPT_Pointer        => 0,";
            when Vm     => Buffer := Buffer
                 & Indent & "    PML4_Address       => 0,"
                 & ASCII.LF
                 & Indent & "    EPT_Pointer        => 16#"
                 & SK.Utils.To_Hex
                 (Item => Subject.Pml4_Address + EPT_Flags) & "#,";
         end case;

         Buffer := Buffer & ASCII.LF
           & Indent & "    VMCS_Address       => 16#"
           & SK.Utils.To_Hex (Item => VMCS_Address) & "#,"
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address  => 16#"
           & SK.Utils.To_Hex (Item => Subject.IO_Bitmap_Address) & "#,"
           & ASCII.LF
           & Indent & "    MSR_Bitmap_Address => 16#"
           & SK.Utils.To_Hex (Item => Subject.MSR_Bitmap_Address) & "#,"
           & ASCII.LF
           & Indent & "    Stack_Address      => 16#"
           & SK.Utils.To_Hex (Item => Subject.Init_State.Stack_Address) & "#,"
           & ASCII.LF
           & Indent & "    Entry_Point        => 16#"
           & SK.Utils.To_Hex (Item => Subject.Init_State.Entry_Point) & "#,"
           & ASCII.LF
           & Indent & "    CR0_Value          => 16#"
           & SK.Utils.To_Hex (Item => Conf.CR0_Value) & "#,"
           & ASCII.LF
           & Indent & "    CR0_Mask           => 16#"
           & SK.Utils.To_Hex (Item => Conf.CR0_Mask) & "#,"
           & ASCII.LF
           & Indent & "    CR4_Value          => 16#"
           & SK.Utils.To_Hex (Item => Conf.CR4_Value) & "#,"
           & ASCII.LF
           & Indent & "    CR4_Mask           => 16#"
           & SK.Utils.To_Hex (Item => Conf.CR4_Mask) & "#,"
           & ASCII.LF
           & Indent & "    CS_Access          => 16#"
           & SK.Utils.To_Hex (Item => Conf.CS_Access) & "#,"
           & ASCII.LF
           & Indent & "    Exception_Bitmap   => 16#"
           & SK.Utils.To_Hex (Item => Conf.Exception_Bm) & "#,"
           & ASCII.LF
           & Indent & "    VMX_Controls       => VMX_Controls_Type'("
           & ASCII.LF
           & Indent (N => 3) & " Exec_Pin    =>" & Conf.Exec_Pin'Img & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc   =>" & Conf.Exec_Proc'Img & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc2  =>" & Conf.Exec_Proc2'Img & ","
           & ASCII.LF
           & Indent (N => 3) & " Exit_Ctrls  =>" & Conf.Exit_Ctrls'Img & ","
           & ASCII.LF
           & Indent (N => 3) & " Entry_Ctrls =>" & Conf.Entry_Ctrls'Img & "),"
           & ASCII.LF
           & Indent & "    Trap_Table         => ";

         if Subject.Trap_Table.Is_Empty then
            Buffer := Buffer & "Null_Trap_Table,";
         else
            Buffer := Buffer & "Trap_Table_Type'(" & ASCII.LF;
            Subject.Trap_Table.Iterate (Process => Add_Trap'Access);

            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & " others => Null_Trap),";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    Event_Table        => ";

         if Subject.Event_Table.Is_Empty then
            Buffer := Buffer & "Null_Event_Table)";
         else
            Buffer := Buffer & "Event_Table_Type'(" & ASCII.LF;
            Subject.Event_Table.Iterate (Process => Add_Event'Access);

            if Natural (Subject.Event_Table.Length) /= 32 then
               Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
                 & " others => Null_Event";
            end if;
            Buffer := Buffer & "))";
         end if;

         Current := Current + 1;
         if Current /= S_Count then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end Write_Subject_Spec;
   begin
      Tmpl := Templates.Load (Filename => "skp-subjects.ads");
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-subjects.ads");

      Tmpl := Templates.Load (Filename => "skp-subjects.adb");
      Policy.Subjects.Iterate (Process => Write_Subject'Access);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__subjects__",
                         Content  => To_String (Buffer));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-subjects.adb");
   end Write_Subjects;

   -------------------------------------------------------------------------

   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Tmpl    : Templates.Template_Type;
      S_Count : constant Positive := Positive (Policy.Subjects.Length);
      C_Count : constant String   := Ada.Strings.Fixed.Trim
        (Source => Policy.Hardware.Processor.Logical_CPUs'Img,
         Side   => Ada.Strings.Left);
   begin
      Tmpl := Templates.Load
        (Filename  => Dir_Name & "/" & Policy_File,
         Use_Store => False);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__cpu_count__",
                         Content  => C_Count);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vmxon_addr__",
                         Content  => SK.Utils.To_Hex
                           (Item => Policy.Vmxon_Address));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vmcs_addr__",
                         Content  => SK.Utils.To_Hex
                           (Item => Policy.Vmcs_Start_Address));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/" & Policy_File);

      Tmpl := Templates.Load (Filename => "skp.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__cpu_range__",
                         Content  => "0 .." & Natural'Image
                           (Policy.Hardware.Processor.Logical_CPUs - 1));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__subj_range__",
                         Content  => "0 .."  & Positive'Image (S_Count - 1));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vmxon_addr__",
                         Content  => SK.Utils.To_Hex
                           (Item => Policy.Vmxon_Address));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp.ads");
   end Write_System;

end Skp.Writers;
