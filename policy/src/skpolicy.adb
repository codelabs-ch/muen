with Ada.Text_IO;
with Ada.Command_Line;

with Skp.Xml;
with Skp.Writers;

procedure Skpolicy
is
   Data    : Skp.Xml.XML_Data_Type;
   Policy  : Skp.Policy_Type;
   Inc_Dir : constant String := "include";
   Pac_Dir : constant String := "pack";
begin
   Skp.Xml.Parse (Data   => Data,
                  File   => Ada.Command_Line.Argument (Number => 1),
                  Schema => "schema/system.xsd");

   Policy := Skp.Xml.To_Policy (Data => Data);
   Skp.Writers.Write_Kernel (Dir_Name => Inc_Dir,
                             Policy   => Policy);

   Ada.Text_IO.Put_Line (Item => "Wrote kernel policy to '" & Inc_Dir
                         & "' directory");

   Skp.Writers.Write_Subjects (Dir_Name => Inc_Dir,
                               Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote subject policy to '" & Inc_Dir
                         & "' directory");

   Skp.Writers.Write_System (Dir_Name => Inc_Dir,
                             Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote system policy to '" & Inc_Dir
                         & "' directory");

   Skp.Writers.Write_Hardware (Dir_Name => Inc_Dir,
                               Policy   => Policy);

   Skp.Writers.Write_Binaries (Dir_Name => Pac_Dir,
                               Policy   => Policy);
   Ada.Text_IO.Put_Line (Item => "Wrote packer policy to '" & Pac_Dir
                         & "' directory");
end Skpolicy;
