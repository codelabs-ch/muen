with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

with SK.Utils;

with Skp.Paging;
with Skp.IO_Ports;

package body Skp.Writers
is

   Policy_File : constant String := "policy.h";
   Indent      : constant String := "   ";

   --  Create paging structures from given memory layout and write them to the
   --  specified file.
   procedure Write
     (Layout   : Memory_Layout_Type;
      Filename : String);

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
     (Layout   : Memory_Layout_Type;
      Filename : String)
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

         --  Physical start address of PML4 paging structure(s).
         PML4_Addr : constant SK.Word64 := Layout.Pml4_Address;
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

         PDPT_Addr := PML4_Addr + SK.Word64 (PML4_Idx_End) * SK.Page_Size;
         PD_Addr   := PDPT_Addr + SK.Word64 (PDPT_Idx_End) * SK.Page_Size;
         PT_Addr   := PD_Addr   + SK.Word64 (PD_Idx_End)   * SK.Page_Size;

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
      Open (Filename => Filename,
            File     => File);

      Layout.Regions.Iterate (Process => Add_Memory_Region'Access);

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
      use Ada.Streams.Stream_IO;

      File   : File_Type;
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
      Open (Filename => Filename,
            File     => File);

      Ports.Ranges.Iterate (Process => Add_Port_Range'Access);

      Write (File => File,
             Item => IO_Ports.To_Stream (B => Bitmap));

      Close (File => File);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type)
   is

      Pkg_Name  : constant String := "Skp.Kernel";
      Spec_Name : constant String := Dir_Name & "/skp-kernel.ads";

      File : Ada.Text_IO.File_Type;
   begin
      Open (Filename => Dir_Name & "/" & Policy_File,
            File     => File);
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "#define KERNEL_STACK  0x"
         & SK.Utils.To_Hex (Item => Policy.Kernel.Stack_Address));
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "#define KERNEL_PML4   0x"
         & SK.Utils.To_Hex
           (Item => Policy.Kernel.Memory_Layout.Pml4_Address));
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "#define SUBJECT_COUNT" & Policy.Subjects.Length'Img);
      Ada.Text_IO.Close (File => File);

      Open (Filename => Spec_Name,
            File     => File);
      Ada.Text_IO.Put_Line (File => File,
                            Item => "package " & Pkg_Name & " is");
      Ada.Text_IO.New_Line (File => File);
      Ada.Text_IO.Put (File => File,
                       Item => Indent
                       & "Stack_Address : constant := 16#");
      Ada.Text_IO.Put (File => File,
                       Item => SK.Utils.To_Hex
                         (Item => Policy.Kernel.Stack_Address));
      Ada.Text_IO.Put_Line (File => File,
                            Item => "#;");
      Ada.Text_IO.New_Line (File => File);
      Ada.Text_IO.Put_Line (File => File,
                            Item => "end " & Pkg_Name & ";");
      Ada.Text_IO.Close (File => File);

      Write (Layout   => Policy.Kernel.Memory_Layout,
             Filename => Dir_Name & "/kernel_pt");
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Subjects
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;

      Pkg_Name  : constant String   := "Skp.Subjects";
      Spec_Name : constant String   := Dir_Name & "/skp-subjects.ads";
      S_Count   : constant Positive := Positive (Policy.Subjects.Length);
      Current   : Natural           := 0;
      Spec_File : File_Type;

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
         Write (Layout   => S.Memory_Layout,
                Filename => PT_File);
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
         Put_Line (File => Spec_File,
                   Item => Indent & "  " & Subject.Id'Img
                   & " => Subject_Spec_Type'(");
         Put (File => Spec_File,
              Item => Indent & "    PML4_Address      => 16#");
         Put (File => Spec_File,
              Item => SK.Utils.To_Hex
                (Item => Subject.Memory_Layout.Pml4_Address));
         Put_Line (File => Spec_File,
                   Item => "#,");
         Put (File => Spec_File,
              Item => Indent & "    VMCS_Address      => 16#");
         Put (File => Spec_File,
              Item => SK.Utils.To_Hex (Item => VMCS_Address));
         Put_Line (File => Spec_File,
                   Item => "#,");
         Put (File => Spec_File,
              Item => Indent & "    IO_Bitmap_Address => 16#");
         Put (File => Spec_File,
              Item => SK.Utils.To_Hex
                (Item => Subject.IO_Ports.IO_Bitmap_Address));
         Put (File => Spec_File,
              Item => "#)");

         Current := Current + 1;
         if Current /= S_Count then
            Put_Line (File => Spec_File,
                      Item => ",");
         else
            Put_Line (File => Spec_File,
                      Item => ");");
         end if;
      end Write_Subject_Spec;
   begin
      Open (Filename => Spec_Name,
            File     => Spec_File);

      Put_Line (File => Spec_File,
                Item => "with SK;");
      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => "--# inherit SK;");
      Put_Line (File => Spec_File,
                Item => "package " & Pkg_Name & " is");
      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => Indent & "type Subject_Id_Type is range 0 .."
                & Positive'Image (S_Count - 1) & ";");
      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => Indent & "type Subject_Spec_Type is record");
      Put_Line (File => Spec_File,
                Item => Indent & "   PML4_Address      : SK.Word64;");
      Put_Line (File => Spec_File,
                Item => Indent & "   VMCS_Address      : SK.Word64;");
      Put_Line (File => Spec_File,
                Item => Indent & "   IO_Bitmap_Address : SK.Word64;");
      Put_Line (File => Spec_File,
                Item => Indent & "end record;");
      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => Indent & "type Subject_Spec_Array is array "
                & "(Subject_Id_Type) of Subject_Spec_Type;");
      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => Indent & "Subject_Specs : constant Subject_Spec_Array"
                & " := Subject_Spec_Array'(");

      Policy.Subjects.Iterate (Process => Write_Subject'Access);

      New_Line (File => Spec_File);
      Put_Line (File => Spec_File,
                Item => "end " & Pkg_Name & ";");

      Close (File => Spec_File);
   end Write_Subjects;

   -------------------------------------------------------------------------

   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type)
   is
      use Ada.Text_IO;

      Pkg_Name  : constant String := "Skp";
      Spec_Name : constant String := Dir_Name & "/skp.ads";

      File : File_Type;
   begin
      Open (Filename => Dir_Name & "/" & Policy_File,
            File     => File,
            Append   => True);
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "#define VMXON_ADDRESS 0x"
         & SK.Utils.To_Hex (Item => Policy.Vmxon_Address));
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "#define VMCS_ADDRESS  0x"
         & SK.Utils.To_Hex (Item => Policy.Vmcs_Start_Address));
      Close (File => File);

      Open (Filename => Spec_Name,
            File     => File);
      Put_Line (File => File,
                Item => "package " & Pkg_Name & " is");
      New_Line (File => File);
      Put (File => File,
           Item => Indent & "Vmxon_Address : constant := 16#");
      Put (File => File,
           Item => SK.Utils.To_Hex
             (Item => Policy.Vmxon_Address));
      Put_Line (File => File,
                Item => "#;");
      New_Line (File => File);
      Put_Line (File => File,
                Item => "end " & Pkg_Name & ";");
      Close (File => File);
   end Write_System;

end Skp.Writers;
