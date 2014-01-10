--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Mulog;

with Pack.Image;
with Pack.Command_Line;

package body Pack.File_Transforms
is

   type Transform_Procedure is not null access procedure
     (File : access Parser.File_Entry_Type);

   --  Normalize name field of file entry.
   function Normalize (Name : String) return String;

   --  Default transform: prepend input directory to file path and normalize
   --  names. This transform must be called from the other transform
   --  implementations first.
   procedure Default_Transform (File : access Parser.File_Entry_Type);

   --  Convert given ELF binary to raw object format.
   procedure To_Raw_Binary (File : access Parser.File_Entry_Type);

   Transforms : constant array (Parser.File_Format_Type) of Transform_Procedure
     := (Parser.Elf => To_Raw_Binary'Access,
         others     => Default_Transform'Access);

   -------------------------------------------------------------------------

   procedure Default_Transform (File : access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      File.Path := Command_Line.Get_Input_Dir & "/" & File.Path;
      File.Name := U (Normalize (Name => S (File.Name)));
   end Default_Transform;

   -------------------------------------------------------------------------

   function Normalize (Name : String) return String
   is
      Charmap : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping (From => "|",
                                        To   => "_");
   begin
      return Ada.Strings.Fixed.Translate
        (Source  => Name,
         Mapping => Charmap);
   end Normalize;

   -------------------------------------------------------------------------

   procedure Process (Files : in out Parser.File_Array)
   is
   begin
      for I in Files'Range loop
         Transforms (Files (I).Format) (File => Files (I)'Access);
      end loop;
   end Process;

   -------------------------------------------------------------------------

   procedure To_Raw_Binary (File : access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Output : constant String := Command_Line.Get_Output_Dir & "/"
        & S (File.Path) & ".bin";
   begin
      Default_Transform (File => File);

      Mulog.Log (Msg => "Converting file '" & S (File.Path) & "' from ELF to "
                 & "raw binary '" & Output & "'");
      Image.To_Binary (Src_Elf => S (File.Path),
                       Dst_Bin => Output);

      --  Update path.

      File.Path := U (Output);
   end To_Raw_Binary;

end Pack.File_Transforms;
