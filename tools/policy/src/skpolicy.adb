with Ada.Text_IO;
with Ada.Command_Line;

with Skp.Xml;
with Skp.Writers;
with Skp.Templates;

procedure Skpolicy
is

   --  Print tool usage.
   procedure Print_Usage;
   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <schema> <templates_dir> <policy>");
   end Print_Usage;

   Data    : Skp.Xml.XML_Data_Type;
   Policy  : Skp.Policy_Type;
   Inc_Dir : constant String := "include";
   Pac_Dir : constant String := "pack";
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Skp.Xml.Parse (Data   => Data,
                  File   => Ada.Command_Line.Argument (Number => 3),
                  Schema => Ada.Command_Line.Argument (Number => 1));

   Policy := Skp.Xml.To_Policy (Data => Data);

   Skp.Templates.Set_Template_Dir
     (Path => Ada.Command_Line.Argument (Number => 2));

   Skp.Writers.Write_Kernel (Dir_Name => Inc_Dir,
                             Policy   => Policy);
   Skp.Writers.Write_Subjects (Dir_Name => Inc_Dir,
                               Policy   => Policy);
   Skp.Writers.Write_System (Dir_Name => Inc_Dir,
                             Policy   => Policy);
   Skp.Writers.Write_Hardware (Dir_Name => Inc_Dir,
                               Policy   => Policy);
   Skp.Writers.Write_Scheduling (Dir_Name => Inc_Dir,
                                 Policy   => Policy);
   Skp.Writers.Write_Binaries (Dir_Name => Pac_Dir,
                               Policy   => Policy);

   Ada.Text_IO.Put_Line (Item => "Policy compilation successful");
   Ada.Text_IO.Put_Line (Item => "  * Include directory: " & Inc_Dir);
   Ada.Text_IO.Put_Line (Item => "  * Packer  directory: " & Pac_Dir);
end Skpolicy;
