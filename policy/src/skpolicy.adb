with Ada.Text_IO;
with Ada.Command_Line;

with Skp.Xml;
with Skp.Writers;

procedure Skpolicy
is
   Data     : Skp.Xml.XML_Data_Type;
   Policy   : Skp.Policy_Type;
   Out_File : constant String := "include/skp-subjects.ads";
begin
   Skp.Xml.Parse (Data   => Data,
                  File   => Ada.Command_Line.Argument (Number => 1),
                  Schema => "schema/system.xsd");

   Policy := Skp.Xml.To_Policy (Data => Data);
   Skp.Writers.Write_Subjects
     (File_Name    => Out_File,
      Package_Name => "Skp.Subjects",
      Policy       => Policy);

   Ada.Text_IO.Put_Line ("Wrote subject specs to " & Out_File);
end Skpolicy;
