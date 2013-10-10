--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Strings.Fixed;

with SK.Utils;

with Skp.Kernel;
with Skp.Subjects;
with Skp.Packer_Config;

with Pack.Image;

procedure Skpacker
is
   use Ada.Strings.Unbounded;
   use Pack;
   use Skp;
   use Skp.Subjects;
   use Skp.Packer_Config;

   --  Print packer usage.
   procedure Print_Usage;
   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <kernel_elf> <image>");
   end Print_Usage;

   Knl_Elf    : constant String    := "obj/kernel.elf";
   Top_Dir    : constant String    := "..";
   Policy_Dir : constant String    := Top_Dir & "/policy/include";
   Addr_Mask  : constant SK.Word64 := 16#0000fffffffff000#;
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

   for S in Subject_Id_Type loop
      declare
         use type SK.Word64;

         Fn        : constant String := Top_Dir & "/" & To_String
           (Binary_Specs (S).Path);
         Name      : constant String := To_String (Binary_Specs (S).Name);
         Raw_Bin   : constant String := "obj/" & Name;
         PML4_Addr : SK.Word64       := Get_PML4_Address (Subject_Id => S);
      begin
         Image.To_Binary (Src_Elf => Fn,
                          Dst_Bin => Raw_Bin);

         if Get_Profile (Subject_Id => S) = Vm then
            PML4_Addr := Get_EPT_Pointer (Subject_Id => S) and Addr_Mask;
            Ada.Text_IO.Put_Line
              (SK.Utils.To_Hex (Item => PML4_Addr) & " [PML4] " & Name
               & " (EPT)");
         else
            Ada.Text_IO.Put_Line
              (SK.Utils.To_Hex (Item => PML4_Addr) & " [PML4] " & Name);
         end if;

         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Get_IO_Bitmap_Address (Subject_Id => S))
            & " [IOBM] " & Name);
         Ada.Text_IO.Put_Line
           (SK.Utils.To_Hex (Item => Get_MSR_Bitmap_Address (Subject_Id => S))
            & " [MSBM] " & Name);
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
            Address  => PML4_Addr);
         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Policy_Dir & "/" & Name & "_iobm",
            Name     => Name & "_iobm",
            Address  => Get_IO_Bitmap_Address (Subject_Id => S));
         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => Policy_Dir & "/" & Name & "_msrbm",
            Name     => Name & "_msrbm",
            Address  => Get_MSR_Bitmap_Address (Subject_Id => S));
      end;
   end loop;

   Image.To_Binary (Src_Elf => Knl_Elf,
                    Dst_Bin => Ada.Command_Line.Argument (2));
end Skpacker;
