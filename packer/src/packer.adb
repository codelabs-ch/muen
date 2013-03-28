with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;

with SK.Utils;

with Skp.Subjects;

with Pack.Image;

procedure Packer
is
   use Ada.Strings.Unbounded;
   use Pack;
   use Skp.Subjects;

   Policy_Dir : constant String := "../policy/include";

   type Binary_Type is record
      Filename : Unbounded_String;
      Address  : SK.Word64;
   end record;

   Subjects : constant array (Subject_Id_Type) of Binary_Type :=
       ((Filename => To_Unbounded_String ("obj/tau0"),
         Address  => 16#00240000#),
        (Filename => To_Unbounded_String ("obj/subject1"),
         Address  => 16#00250000#),
        (Filename => To_Unbounded_String ("obj/subject2"),
         Address  => 16#00260000#));
begin
   Ada.Text_IO.Put_Line ("Packaging kernel image ...");

   --  Kernel sections.

   Image.Add_Section (Filename => Policy_Dir & "/kernel_pt",
                      Address  => 16#00200000#);

   --  Subjects.

   for S in Subject_Specs'Range loop
      declare
         Fn   : constant String := To_String (Subjects (S).Filename);
         Name : constant String := Ada.Directories.Base_Name (Name => Fn);
      begin
         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Subjects (S).Address) & " => " & Name);
         Ada.Text_IO.Put_Line
           ("  [PML4 @ " & SK.Utils.To_Hex
              (Item => Subject_Specs (S).PML4_Address) & "]");
         Ada.Text_IO.Put_Line
           ("  [IOBM @ " & SK.Utils.To_Hex
              (Item => Subject_Specs (S).IO_Bitmap_Address) & "]");

         Image.Add_Section (Filename => Fn,
                            Address  => Subjects (S).Address);
         Image.Add_Section (Filename => Policy_Dir & "/" & Name & "_pt",
                            Address  => Subject_Specs (S).PML4_Address);
         Image.Add_Section (Filename => Policy_Dir & "/" & Name & "_iobm",
                            Address  => Subject_Specs (S).IO_Bitmap_Address);
      end;
   end loop;
end Packer;
