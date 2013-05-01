with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Strings.Fixed;

with SK.Utils;

with Skp.Kernel;
with Skp.Binaries;
with Skp.Subjects;

with Pack.Image;

procedure Skpacker
is
   use Ada.Strings.Unbounded;
   use Pack;
   use Skp;
   use Skp.Subjects;
   use Skp.Binaries;

   --  Print packer usage.
   procedure Print_Usage;
   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <kernel_elf> <image>");
   end Print_Usage;

   Knl_Elf    : constant String := "obj/kernel.elf";
   Top_Dir    : constant String := "..";
   Policy_Dir : constant String := Top_Dir & "/policy/include";
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Text_IO.Put_Line ("Packaging kernel image '"
                         & Ada.Command_Line.Argument (2) & "'");

   --  Kernel

   Ada.Directories.Copy_File
     (Source_Name => Ada.Command_Line.Argument (1),
      Target_Name => Knl_Elf);

   --  Per-CPU pagetables

   for I in Skp.CPU_Range loop
      declare
         use type SK.Word64;

         PML4_Addr : constant SK.Word64
           := Kernel.PML4_Address + SK.Word64 (I) * (4 * SK.Page_Size);
         CPU_Nr    : constant String
           := Ada.Strings.Fixed.Trim (Source => I'Img,
                                      Side   => Ada.Strings.Left);
      begin
         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Policy_Dir & "/kernel_pt_" & CPU_Nr,
            Name     => "kernel_pt_" & CPU_Nr,
            Address  => PML4_Addr);
         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => PML4_Addr)
            & " [PML4] kernel (" & CPU_Nr & ")");
      end;
   end loop;

   --  Subjects

   for S in Subject_Specs'Range loop
      declare
         Fn      : constant String := Top_Dir & "/" & To_String
           (Binary_Specs (S).Path);
         Name    : constant String := To_String (Binary_Specs (S).Name);
         Raw_Bin : constant String := "obj/" & Name;
      begin
         Image.To_Binary (Src_Elf => Fn,
                          Dst_Bin => Raw_Bin);

         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Subject_Specs (S).PML4_Address)
            & " [PML4] " & Name);
         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Subject_Specs (S).IO_Bitmap_Address)
            & " [IOBM] " & Name);
         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Binary_Specs (S).Physical_Address)
            & " [BIN ] " & Name);

         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Raw_Bin,
            Name     => Name,
            Address  => Binary_Specs (S).Physical_Address);
         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Policy_Dir & "/" & Name & "_pt",
            Name     => Name & "_pt",
            Address  => Subject_Specs (S).PML4_Address);
         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Policy_Dir & "/" & Name & "_iobm",
            Name     => Name & "_iobm",
            Address  => Subject_Specs (S).IO_Bitmap_Address);
      end;
   end loop;

   Image.To_Binary (Src_Elf => Knl_Elf,
                    Dst_Bin => Ada.Command_Line.Argument (2));
end Skpacker;
