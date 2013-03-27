with Ada.Text_IO;
with Ada.Command_Line;

with Skp.Xml;
with Skp.Writers;

procedure Skpolicy
is
   Data     : Skp.Xml.XML_Data_Type;
   Policy   : Skp.Policy_Type;
   Out_Dir  : constant String := "include";
   Out_File : constant String := Out_Dir & "/skp-subjects.ads";

begin
   Skp.Xml.Parse (Data   => Data,
                  File   => Ada.Command_Line.Argument (Number => 1),
                  Schema => "schema/system.xsd");

   Policy := Skp.Xml.To_Policy (Data => Data);
   Skp.Writers.Write_Kernel (Dir_Name => Out_Dir,
                             Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote kernel policy to '" & Out_Dir
                         & "' directory");

   Skp.Writers.Write_Subjects
     (File_Name    => Out_File,
      Package_Name => "Skp.Subjects",
      Policy       => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote subject specs to '" & Out_File & "'");

   Skp.Writers.Write_Pagetables
     (Dir_Name => Out_Dir,
      Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote pagetables to '" & Out_Dir
                         & "' directory");

   Skp.Writers.Write_IO_Bitmaps
     (Dir_Name => Out_Dir,
      Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote I/O bitmaps to '" & Out_Dir
                         & "' directory");
end Skpolicy;
