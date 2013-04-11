with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Exceptions;

with SK.Utils;

with Skp.Paging;
with Skp.IO_Ports;
with Skp.Templates;

package body Skp.Writers
is

   use Ada.Strings.Unbounded;

   Policy_File : constant String := "policy.h";
   Indent      : constant String := "   ";

   --  Create paging structures from given memory layout and write them to the
   --  specified file. The PML4 address parameter specifies the physical start
   --  address of the PML4 paging structure.
   procedure Write
     (Mem_Layout   : Memory_Layout_Type;
      Pml4_Address : SK.Word64;
      Filename     : String);

   --  Create I/O bitmap from given ports and write them to the specified file.
   procedure Write
     (Ports    : IO_Ports_Type;
      Filename : String);

   --  Open file given by filename. Raises IO_Error if the file could not be
   --  opened.
   procedure Open
     (Filename :     String;
      File     : out Ada.Streams.Stream_IO.File_Type);

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
      Filename     : String)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
      PML4 : Paging.PML4_Table_Type := Paging.Null_PML4_Table;
      PDPT : Paging.PDP_Table_Type  := Paging.Null_PDP_Table;
      PD   : Paging.PD_Table_Type   := Paging.Null_PD_Table;
      PT   : Paging.Page_Table_Type := Paging.Null_Page_Table;

      --  Add given memory region to pagetable.
      procedure Add_Memory_Region (C : Memregion_Package.Cursor);

      -------------------------------------------------------------------

      procedure Add_Memory_Region (C : Memregion_Package.Cursor)
      is
         use type SK.Word64;

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

         PDPT_Addr := Pml4_Address + SK.Word64 (PML4_Idx_End) * SK.Page_Size;
         PD_Addr   := PDPT_Addr    + SK.Word64 (PDPT_Idx_End) * SK.Page_Size;
         PT_Addr   := PD_Addr      + SK.Word64 (PD_Idx_End)   * SK.Page_Size;

         for Idx in Paging.Table_Range range PML4_Idx_Start .. PML4_Idx_End
         loop
            if not Paging.Is_Present (E => PML4 (Idx)) then
               PML4 (Idx) := Paging.Create_PML4_Entry
                 (Address       => PDPT_Addr +
                    (SK.Word64 (Idx) - 1) * SK.Page_Size,
                  Writable      => True,
                  User_Access   => True,
                  Writethrough  => True,
                  Cache_Disable => False,
                  Exec_Disable  => False);
            end if;
         end loop;

         for Idx in Paging.Table_Range range PDPT_Idx_Start .. PDPT_Idx_End
         loop
            if not Paging.Is_Present (E => PDPT (Idx)) then
               PDPT (Idx) := Paging.Create_PDPT_Entry
                 (Address       => PD_Addr +
                    (SK.Word64 (Idx) - 1) * SK.Page_Size,
                  Writable      => True,
                  User_Access   => True,
                  Writethrough  => True,
                  Cache_Disable => False,
                  Map_Page      => False,
                  Global        => False,
                  PAT           => False,
                  Exec_Disable  => False);
            end if;
         end loop;

         for Idx in Paging.Table_Range range PD_Idx_Start .. PD_Idx_End loop
            if not Paging.Is_Present (E => PD (Idx)) then
               PD (Idx) := Paging.Create_PD_Entry
                 (Address       => PT_Addr +
                    (SK.Word64 (Idx) - 1) * SK.Page_Size,
                  Writable      => True,
                  User_Access   => True,
                  Writethrough  => True,
                  Cache_Disable => False,
                  Map_Page      => False,
                  Global        => False,
                  PAT           => False,
                  Exec_Disable  => False);
            end if;
         end loop;

         for Idx in Paging.Table_Range range PT_Idx_Start .. PT_Idx_End loop
            if not Paging.Is_Present (E => PT (Idx)) then
               PT (Idx) := Paging.Create_PT_Entry
                 (Address       => Physical_Addr,
                  Writable      => R.Writable,
                  User_Access   => True,
                  Writethrough  => True,
                  Cache_Disable => False,
                  Global        => False,
                  PAT           => True,
                  Exec_Disable  => not R.Executable);

               Physical_Addr := Physical_Addr + SK.Page_Size;
            end if;
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

      -------------------------------------------------------------------

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

   procedure Write_Binaries
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      B_Count : constant Positive := Positive (Policy.Binaries.Length);
      Current : Natural           := 0;
      Buffer  : Unbounded_String;
      Tmpl    : Templates.Template_Type;

      --  Write binary spec.
      procedure Write_Binary_Spec (C : Binary_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Write_Binary_Spec (C : Binary_Package.Cursor)
      is
         Binary : constant Binary_Type := Binary_Package.Element
           (Position => C);
      begin
         Buffer := Buffer & Indent
           & "  (Path             => To_Unbounded_String ("""
           & To_String (Binary.Path) & """),"
           & ASCII.LF
           & Indent & "   Physical_Address => 16#"
           & SK.Utils.To_Hex (Item => Binary.Physical_Address) & "#)";

         Current := Current + 1;
         if Current /= B_Count then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end Write_Binary_Spec;
   begin
      Tmpl := Templates.Load (Filename => "skp-binaries.ads");

      Policy.Binaries.Iterate (Process => Write_Binary_Spec'Access);

      Templates.Replace (Template => Tmpl,
                         Pattern  => "__binaries__",
                         Content  => To_String (Buffer));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-binaries.ads");
   end Write_Binaries;

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
         Dev_Name : String               := To_String (Dev.Name);
         Port_Idx : Ports_Package.Cursor := Dev.IO_Ports.First;
      begin
         Dev_Name (Dev_Name'First) := Ada.Characters.Handling.To_Upper
           (Item => Dev_Name (Dev_Name'First));

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

   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Tmpl : Templates.Template_Type;

      --  Replace kernel stack and PML4 patterns with actual values.
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

      Write (Mem_Layout   => Policy.Kernel.Memory_Layout,
             Pml4_Address => Policy.Kernel.Pml4_Address,
             Filename     => Dir_Name & "/kernel_pt");
   end Write_Kernel;

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
         S       : constant Subject_Type := Subjects_Package.Element
           (Position => C);
         PT_File : constant String       := Dir_Name & "/" & To_String (S.Name)
           & "_pt";
         IO_File : constant String       := Dir_Name & "/" & To_String (S.Name)
           & "_iobm";
      begin
         Write_Subject_Spec (Subject => S);
         Write (Mem_Layout   => S.Memory_Layout,
                Pml4_Address => S.Pml4_Address,
                Filename     => PT_File);
         Write (Ports    => S.IO_Ports,
                Filename => IO_File);
      end Write_Subject;

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : Subject_Type)
      is
         use type SK.Word64;

         VMCS_Address : constant SK.Word64
           := Policy.Vmcs_Start_Address + SK.Word64 (Current) * SK.Page_Size;
      begin
         Buffer := Buffer & Indent & "  " & Subject.Id'Img
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    PML4_Address      => 16#"
           & SK.Utils.To_Hex (Item => Subject.Pml4_Address) & "#,"
           & ASCII.LF
           & Indent & "    VMCS_Address      => 16#"
           & SK.Utils.To_Hex (Item => VMCS_Address) & "#,"
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address => 16#"
           & SK.Utils.To_Hex (Item => Subject.IO_Bitmap_Address) & "#,"
           & ASCII.LF
           & Indent & "    Stack_Address     => 16#"
           & SK.Utils.To_Hex (Item => Subject.Init_State.Stack_Address) & "#,"
           & ASCII.LF
           & Indent & "    Entry_Point       => 16#"
           & SK.Utils.To_Hex (Item => Subject.Init_State.Entry_Point) & "#)";

         Current := Current + 1;
         if Current /= S_Count then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end Write_Subject_Spec;
   begin
      Tmpl := Templates.Load (Filename => "skp-subjects.ads");

      Policy.Subjects.Iterate (Process => Write_Subject'Access);

      Templates.Replace (Template => Tmpl,
                         Pattern  => "__subjects__",
                         Content  => To_String (Buffer));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-subjects.ads");
   end Write_Subjects;

   -------------------------------------------------------------------------

   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      S_Count : constant Positive := Positive (Policy.Subjects.Length);
      Tmpl    : Templates.Template_Type;
   begin
      Tmpl := Templates.Load
        (Filename  => Dir_Name & "/" & Policy_File,
         Use_Store => False);
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
