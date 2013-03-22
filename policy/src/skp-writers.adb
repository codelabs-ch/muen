with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;

with SK.Utils;

with Skp.Paging;

package body Skp.Writers
is

   -------------------------------------------------------------------------

   procedure Write_Pagetables
     (Dir_Name : String;
      Policy   : Policy_Type)
   is

      --  Write pagetables of given subject.
      procedure Write_Subject (S : Subject_Type);

      ----------------------------------------------------------------------

      procedure Write_Subject (S : Subject_Type)
      is
         use Ada.Strings.Unbounded;
         use Ada.Streams.Stream_IO;

         File : File_Type;

         File_Name  : constant String := Dir_Name & "/"
           & To_String (Get_Name (Subject => S)) & ".pt";
         Mem_Layout : constant Memory_Layout_Type
           := Get_Memory_Layout (Subject => S);

         PML4 : Paging.PML4_Table_Type := Paging.Null_PML4_Table;
         PDPT : Paging.PDP_Table_Type  := Paging.Null_PDP_Table;
         PD   : Paging.PD_Table_Type   := Paging.Null_PD_Table;
         PT   : Paging.Page_Table_Type := Paging.Null_Page_Table;

         --  Add given memory region to subject's pagetables.
         procedure Add_Memory_Region (R : Memory_Region_Type);

         -------------------------------------------------------------------

         procedure Add_Memory_Region (R : Memory_Region_Type)
         is
            use type SK.Word64;

            PML4_Idx_Start, PML4_Idx_End : Paging.Table_Range;
            PDPT_Idx_Start, PDPT_Idx_End : Paging.Table_Range;
            PD_Idx_Start, PD_Idx_End     : Paging.Table_Range;
            PT_Idx_Start, PT_Idx_End     : Paging.Table_Range;

            --  Physical start address of PML4 paging structure(s).
            PML4_Addr : constant SK.Word64 := Get_Pml4_Address
              (Layout => Mem_Layout);
            --  Physical start address of PDPT paging structure(s).
            PDPT_Addr : SK.Word64;
            --  Physical start address of PD paging structure(s).
            PD_Addr   : SK.Word64;
            --  Physical start address of PT paging structure(s).
            PT_Addr   : SK.Word64;

            Physical_Addr : SK.Word64 := Get_Physical_Address (Region => R);
            Virt_Start    : constant SK.Word64
              := Get_Virtual_Address (Region => R);
            Virt_End      : constant SK.Word64
              := Virt_Start + Get_Size (Region => R) - 1;
            Permission    : constant Memory_Permission_Type
              := Get_Permission (Region => R);
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
                     Writable      => Permission = Read_Write,
                     User_Access   => True,
                     Writethrough  => True,
                     Cache_Disable => False,
                     Global        => False,
                     PAT           => True,
                     Exec_Disable  => not Is_Executable (Region => R));

                  Physical_Addr := Physical_Addr + SK.Page_Size;
               end if;
            end loop;
         end Add_Memory_Region;
      begin
         if Ada.Directories.Exists (Name => File_Name) then
            Open (File => File,
                  Mode => Out_File,
                  Name => File_Name);
         else
            Create (File => File,
                    Mode => Out_File,
                    Name => File_Name);
         end if;

         Iterate (Layout  => Mem_Layout,
                  Process => Add_Memory_Region'Access);

         Paging.PML4_Table_Type'Write (Stream (File => File), PML4);
         Paging.PDP_Table_Type'Write  (Stream (File => File), PDPT);
         Paging.PD_Table_Type'Write   (Stream (File => File), PD);
         Paging.Page_Table_Type'Write (Stream (File => File), PT);

         Close (File => File);
      end Write_Subject;
   begin
      Iterate (Policy  => Policy,
               Process => Write_Subject'Access);
   end Write_Pagetables;

   -------------------------------------------------------------------------

   procedure Write_Subjects
     (File_Name    : String;
      Package_Name : String;
      Policy       : Policy_Type)
   is
      use Ada.Text_IO;

      File    : File_Type;
      Current : Natural           := 0;
      S_Count : constant Positive := Get_Subject_Count (Policy => Policy);
      Indent  : constant String   := "   ";

      --  Write specification of given subject.
      procedure Write_Subject (S : Subject_Type);

      ----------------------------------------------------------------------

      procedure Write_Subject (S : Subject_Type)
      is
         IO_Ports   : constant IO_Ports_Type := Get_IO_Ports (Subject => S);
         Mem_Layout : constant Memory_Layout_Type
           := Get_Memory_Layout (Subject => S);
      begin
         Put_Line (File => File,
                   Item => Indent & "  " & Get_Id (Subject => S)'Img
                   & " => Subject_Spec_Type'(");
         Put (File => File,
              Item => Indent & "    PML4_Address      => 16#");
         Put (File => File,
              Item => SK.Utils.To_Hex
                (Item => Get_Pml4_Address (Layout => Mem_Layout)));
         Put_Line (File => File,
                   Item => "#,");

         Put (File => File,
              Item => Indent & "    IO_Bitmap_Address => 16#");
         Put (File => File,
              Item => SK.Utils.To_Hex
                (Item => Get_Bitmap_Address (Ports => IO_Ports)));
         Put (File => File,
              Item => "#)");

         Current := Current + 1;
         if Current /= S_Count then
            Put_Line (File => File,
                      Item => ",");
         else
            Put_Line (File => File,
                      Item => ");");
         end if;
      end Write_Subject;
   begin
      if Ada.Directories.Exists (Name => File_Name) then
         Open (File => File,
               Mode => Out_File,
               Name => File_Name);
      else
         Create (File => File,
                 Mode => Out_File,
                 Name => File_Name);
      end if;

      Put_Line (File => File,
                Item => "with SK;");
      New_Line (File => File);
      Put_Line (File => File,
                Item => "--# inherit SK;");
      Put_Line (File => File,
                Item => "package " & Package_Name & " is");
      New_Line (File => File);
      Put_Line (File => File,
                Item => Indent & "type Subject_Id_Type is range 0 .."
                & Positive'Image (S_Count - 1) & ";");
      New_Line (File => File);
      Put_Line (File => File,
                Item => Indent & "type Subject_Spec_Type is record");
      Put_Line (File => File,
                Item => Indent & "   PML4_Address      : SK.Word64;");
      Put_Line (File => File,
                Item => Indent & "   IO_Bitmap_Address : SK.Word64;");
      Put_Line (File => File,
                Item => Indent & "end record;");
      New_Line (File => File);
      Put_Line (File => File,
                Item => Indent & "type Subject_Spec_Array is array "
                & "(Subject_Id_Type) of Subject_Spec_Type;");
      New_Line (File => File);
      Put_Line (File => File,
                Item => Indent & "Subject_Specs : constant Subject_Spec_Array"
                & " := Subject_Spec_Array'(");

      Iterate (Policy  => Policy,
               Process => Write_Subject'Access);

      New_Line (File => File);
      Put_Line (File => File,
                Item => "end " & Package_Name & ";");

      Close (File => File);
   end Write_Subjects;

end Skp.Writers;
