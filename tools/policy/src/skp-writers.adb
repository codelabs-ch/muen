with Ada.Text_IO;
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
   --  opened. If Append is True any writes will be appended to the existing
   --  file.
   procedure Open
     (Filename :     String;
      File     : out Ada.Text_IO.File_Type;
      Append   :     Boolean := False);

   --  Open file given by filename. Raises IO_Error if the file could not be
   --  opened.
   procedure Open
     (Filename :     String;
      File     : out Ada.Streams.Stream_IO.File_Type);

   -------------------------------------------------------------------------

   procedure Open
     (Filename :     String;
      File     : out Ada.Text_IO.File_Type;
      Append   :     Boolean := False)
   is
      File_Mode : Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
   begin
      if Ada.Directories.Exists (Name => Filename) then
         if Append then
            File_Mode := Ada.Text_IO.Append_File;
         end if;
         Ada.Text_IO.Open
           (File => File,
            Mode => File_Mode,
            Name => Filename);
      else
         Ada.Text_IO.Create
           (File => File,
            Mode => File_Mode,
            Name => Filename);
      end if;

   exception
      when E : others =>
         raise IO_Error with "Unable to open file '" & Filename & "' - "
           & Ada.Exceptions.Exception_Message (X => E);
   end Open;

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
      Pkg_Name  : constant String   := "Skp.Binaries";
      Spec_Name : constant String   := Dir_Name & "/skp-binaries.ads";
      B_Count   : constant Positive := Positive (Policy.Binaries.Length);
      Current   : Natural           := 0;
      Spec_File : Ada.Text_IO.File_Type;
      Buffer    : Unbounded_String;

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
         else
            Buffer := Buffer & ");" & ASCII.LF;
         end if;
      end Write_Binary_Spec;
   begin
      Buffer := Buffer & "with Ada.Strings.Unbounded; "
        & "use Ada.Strings.Unbounded;"
        & ASCII.LF
        & "with SK;"
        & ASCII.LF & ASCII.LF
        & "package " & Pkg_Name & " is"
        & ASCII.LF & ASCII.LF
        & Indent & "type Binary_Spec_Type is record"         & ASCII.LF
        & Indent & "   Path             : Unbounded_String;" & ASCII.LF
        & Indent & "   Physical_Address : SK.Word64;"        & ASCII.LF
        & Indent & "end record;"
        & ASCII.LF & ASCII.LF
        & Indent & "type Binary_Spec_Array is array (Subject_Id_Type) of "
        & "Binary_Spec_Type;"
        & ASCII.LF & ASCII.LF
        & Indent & "Binary_Specs : constant Binary_Spec_Array := ("
        & ASCII.LF;

      Policy.Binaries.Iterate (Process => Write_Binary_Spec'Access);

      Buffer := Buffer & ASCII.LF & "end " & Pkg_Name & ";";

      Open (Filename => Spec_Name,
            File     => Spec_File);
      Ada.Text_IO.Put_Line (File => Spec_File,
                            Item => To_String (Buffer));
      Ada.Text_IO.Close (File => Spec_File);
   end Write_Binaries;

   -------------------------------------------------------------------------

   procedure Write_Hardware
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Pkg_Name  : constant String := "Skp.Hardware";
      Spec_Name : constant String := Dir_Name & "/skp-hardware.ads";

      File   : Ada.Text_IO.File_Type;
      Buffer : Unbounded_String;

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
         Buffer := Buffer & ASCII.LF;
      end Write_Device;
   begin
      Buffer := Buffer & "package " & Pkg_Name & " is" & ASCII.LF & ASCII.LF;
      Policy.Hardware.Devices.Iterate (Process => Write_Device'Access);
      Buffer := Buffer & "end " & Pkg_Name & ";";

      Open (Filename => Spec_Name,
            File     => File);
      Ada.Text_IO.Put_Line (File => File,
                            Item => To_String (Buffer));
      Ada.Text_IO.Close (File => File);
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
      Tmpl := Templates.Load (Filename => "policy.h");
      Replace_Kernel_Patterns;
      Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subj_count__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Policy.Subjects.Length'Img,
            Side   => Ada.Strings.Left));
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/policy.h");

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
      Pkg_Name  : constant String   := "Skp.Subjects";
      Spec_Name : constant String   := Dir_Name & "/skp-subjects.ads";
      S_Count   : constant Positive := Positive (Policy.Subjects.Length);
      Current   : Natural           := 0;
      Spec_File : Ada.Text_IO.File_Type;
      Buffer    : Unbounded_String;

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
         else
            Buffer := Buffer & ");" & ASCII.LF;
         end if;
      end Write_Subject_Spec;
   begin
      Buffer := Buffer & "with SK;"
        & ASCII.LF & ASCII.LF
        & "--# inherit SK, Skp;"
        & ASCII.LF
        & "package " & Pkg_Name & " is"
        & ASCII.LF & ASCII.LF
        & Indent & "type Subject_Spec_Type is record"  & ASCII.LF
        & Indent & "   PML4_Address      : SK.Word64;" & ASCII.LF
        & Indent & "   VMCS_Address      : SK.Word64;" & ASCII.LF
        & Indent & "   IO_Bitmap_Address : SK.Word64;" & ASCII.LF
        & Indent & "   Stack_Address     : SK.Word64;" & ASCII.LF
        & Indent & "   Entry_Point       : SK.Word64;" & ASCII.LF
        & Indent & "end record;"
        & ASCII.LF & ASCII.LF
        & Indent & "type Subject_Spec_Array is array (Skp.Subject_Id_Type) "
        & "of Subject_Spec_Type;"
        & ASCII.LF & ASCII.LF
        & Indent & "Subject_Specs : constant Subject_Spec_Array := "
        & "Subject_Spec_Array'("
        & ASCII.LF;

      Policy.Subjects.Iterate (Process => Write_Subject'Access);

      Buffer := Buffer & ASCII.LF & "end " & Pkg_Name & ";";

      Open (Filename => Spec_Name,
            File     => Spec_File);
      Ada.Text_IO.Put_Line (File => Spec_File,
                            Item => To_String (Buffer));
      Ada.Text_IO.Close (File => Spec_File);
   end Write_Subjects;

   -------------------------------------------------------------------------

   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      Pkg_Name  : constant String   := "Skp";
      Spec_Name : constant String   := Dir_Name & "/skp.ads";
      S_Count   : constant Positive := Positive (Policy.Subjects.Length);

      File   : Ada.Text_IO.File_Type;
      Buffer : Unbounded_String;
   begin
      Buffer := Buffer & "#define VMXON_ADDRESS 0x"
        & SK.Utils.To_Hex (Item => Policy.Vmxon_Address)
        & ASCII.LF
        & "#define VMCS_ADDRESS  0x"
        & SK.Utils.To_Hex (Item => Policy.Vmcs_Start_Address);

      Open (Filename => Dir_Name & "/" & Policy_File,
            File     => File,
            Append   => True);
      Ada.Text_IO.Put_Line (File => File,
                            Item => To_String (Buffer));
      Ada.Text_IO.Close (File => File);

      Buffer := Null_Unbounded_String & "package " & Pkg_Name & " is"
        & ASCII.LF & ASCII.LF
        & Indent & "subtype Subject_Id_Type is Natural range 0 " & ".."
        & Positive'Image (S_Count - 1) & ";"
        & ASCII.LF & ASCII.LF
        & Indent & "Vmxon_Address : constant := 16#"
        & SK.Utils.To_Hex (Item => Policy.Vmxon_Address) & "#;"
        & ASCII.LF & ASCII.LF
        & "end " & Pkg_Name & ";";

      Open (Filename => Spec_Name,
            File     => File);
      Ada.Text_IO.Put_Line (File => File,
                            Item => To_String (Buffer));
      Ada.Text_IO.Close (File => File);
   end Write_System;

end Skp.Writers;
