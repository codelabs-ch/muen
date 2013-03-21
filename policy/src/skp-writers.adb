with Ada.Text_IO;
with Ada.Directories;

with SK.Utils;

package body Skp.Writers
is

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
